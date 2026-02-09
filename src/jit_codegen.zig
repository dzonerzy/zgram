//! JIT code generator: converts grammar IR into LLVM IR using the LLVM C API.
//!
//! Builds an in-memory LLVMModuleRef directly (no bitcode step).
//! The module is handed to jit_compiler.zig for JIT compilation via LLJIT.
//!
//! Architecture:
//!   - Each grammar rule becomes a function: rule_N(ptr input, i64 len, ptr output, i64 pos) → i64
//!     Returns new position on match, -1 on failure.
//!   - zgram_parse(ptr input, i64 len, ptr output) → i32 is the C ABI entry point.
//!   - Expression types emit basic block patterns within their rule function.
//!   - Memory management (node alloc, error reporting) calls into exported Zig helpers.

const std = @import("std");
const Allocator = std.mem.Allocator;
const gp = @import("grammar_parser.zig");
const LB = @import("llvm_builder.zig");

pub const CodegenError = error{
    OutOfMemory,
    InvalidGrammar,
};

/// Result of code generation — pass both to jit_compiler.jitCompile().
pub const CodegenResult = struct {
    module: LB.LLVMModuleRef,
    context: LB.LLVMContextRef,
};

/// Codegen state passed through expression emission.
const Codegen = struct {
    b: *LB.Builder,

    // Function args (available in every rule function)
    input_ptr: LB.Value,
    input_len: LB.Value,
    output_ptr: LB.Value,

    // Rule functions array
    rule_fns: []LB.Value,
    rule_fn_type: LB.Type,

    // Helper function declarations
    helper_reserve_node: LB.Value,
    helper_fill_node: LB.Value,
    helper_set_error: LB.Value,
    helper_set_error_at_hwm: LB.Value,
    helper_set_rule_name: LB.Value,
    helper_ensure_capacity: LB.Value,

    // Helper function types
    helper_reserve_node_type: LB.Type,
    helper_fill_node_type: LB.Type,
    helper_set_error_type: LB.Type,
    helper_set_error_at_hwm_type: LB.Type,
    helper_set_rule_name_type: LB.Type,
    helper_ensure_capacity_type: LB.Type,

    // Grammar info
    grammar: *const gp.Grammar,
    silent_flags: []const bool,

    // Per-rule child count tracking (alloca i32, counts direct children)
    child_count_ptr: LB.Value,
};

/// Generate an LLVM module from a parsed grammar.
/// Returns module + context (caller passes to jit_compiler.jitCompile).
pub fn generateModule(allocator: Allocator, grammar: *const gp.Grammar) CodegenError!CodegenResult {
    var b = LB.Builder.init(allocator, "zgram_grammar");

    // Rule function type: i64 @rule_N(ptr input, i64 len, ptr output, i64 pos)
    const rule_fn_type = b.fnType(b.i64, &.{ b.ptr, b.i64, b.ptr, b.i64 });

    // Declare helper functions (defined in jit_helpers.zig, resolved by LLJIT)
    // i32 zgram_reserve_node(ptr output)
    const helper_reserve_node_type = b.fnType(b.i32, &.{b.ptr});
    const helper_reserve_node = b.addFunction("zgram_reserve_node", helper_reserve_node_type);

    // void zgram_fill_node(ptr output, i32 idx, i16 rule_id, i32 start, i32 end, i32 subtree_size, i16 child_count)
    const helper_fill_node_type = b.fnType(b.void, &.{ b.ptr, b.i32, b.i16, b.i32, b.i32, b.i32, b.i16 });
    const helper_fill_node = b.addFunction("zgram_fill_node", helper_fill_node_type);

    // void zgram_set_error(ptr output, ptr input, i64 input_len, i64 pos, ptr msg, i64 msg_len)
    const helper_set_error_type = b.fnType(b.void, &.{ b.ptr, b.ptr, b.i64, b.i64, b.ptr, b.i64 });
    const helper_set_error = b.addFunction("zgram_set_error", helper_set_error_type);

    // void zgram_set_error_at_hwm(ptr output, ptr input, i64 input_len)
    const helper_set_error_at_hwm_type = b.fnType(b.void, &.{ b.ptr, b.ptr, b.i64 });
    const helper_set_error_at_hwm = b.addFunction("zgram_set_error_at_hwm", helper_set_error_at_hwm_type);

    // void zgram_set_rule_name(ptr output, i16 rule_id, ptr name, i8 name_len)
    const helper_set_rule_name_type = b.fnType(b.void, &.{ b.ptr, b.i16, b.ptr, b.i8 });
    const helper_set_rule_name = b.addFunction("zgram_set_rule_name", helper_set_rule_name_type);

    // i32 zgram_ensure_capacity(ptr output, i32 needed)
    const helper_ensure_capacity_type = b.fnType(b.i32, &.{ b.ptr, b.i32 });
    const helper_ensure_capacity = b.addFunction("zgram_ensure_capacity", helper_ensure_capacity_type);

    // Compute silent flags
    const silent_flags = try computeSilentFlags(allocator, grammar);
    defer allocator.free(silent_flags);

    // Create rule functions
    const rule_fns = allocator.alloc(LB.Value, grammar.rules.len) catch return CodegenError.OutOfMemory;
    defer allocator.free(rule_fns);

    for (0..grammar.rules.len) |i| {
        const name = std.fmt.allocPrintSentinel(allocator, "rule_{d}", .{i}, 0) catch return CodegenError.OutOfMemory;
        defer allocator.free(name);
        rule_fns[i] = b.addFunction(name, rule_fn_type);
        b.setLinkageInternal(rule_fns[i]);
    }

    // Generate each rule function body
    for (grammar.rules, 0..) |rule, i| {
        var cg = Codegen{
            .b = &b,
            .input_ptr = undefined,
            .input_len = undefined,
            .output_ptr = undefined,
            .rule_fns = rule_fns,
            .rule_fn_type = rule_fn_type,
            .helper_reserve_node = helper_reserve_node,
            .helper_fill_node = helper_fill_node,
            .helper_set_error = helper_set_error,
            .helper_set_error_at_hwm = helper_set_error_at_hwm,
            .helper_set_rule_name = helper_set_rule_name,
            .helper_ensure_capacity = helper_ensure_capacity,
            .helper_reserve_node_type = helper_reserve_node_type,
            .helper_fill_node_type = helper_fill_node_type,
            .helper_set_error_type = helper_set_error_type,
            .helper_set_error_at_hwm_type = helper_set_error_at_hwm_type,
            .helper_set_rule_name_type = helper_set_rule_name_type,
            .helper_ensure_capacity_type = helper_ensure_capacity_type,
            .grammar = grammar,
            .silent_flags = silent_flags,
            .child_count_ptr = undefined,
        };
        try emitRuleFunction(&cg, rule, @intCast(i));
    }

    // Generate zgram_parse entry point
    try emitParseEntryPoint(&b, rule_fns[0], rule_fn_type, grammar, helper_set_error, helper_set_error_type, helper_set_error_at_hwm, helper_set_error_at_hwm_type, helper_set_rule_name, helper_set_rule_name_type, silent_flags[0]);

    return .{ .module = b.module, .context = b.ctx };
}

/// Emit the body of a rule function.
fn emitRuleFunction(cg: *Codegen, rule: *const gp.Rule, rule_id: u16) CodegenError!void {
    const b = cg.b;
    const func = cg.rule_fns[rule_id];
    b.setCurrentFn(func);

    const entry = b.appendBlock("entry");
    b.positionAtEnd(entry);

    // Get function args
    cg.input_ptr = b.param(func, 0);
    cg.input_len = b.param(func, 1);
    cg.output_ptr = b.param(func, 2);
    const pos_arg = b.param(func, 3);

    const is_silent = cg.silent_flags[rule_id];

    // HWM field offsets in ParseOutput (computed from extern struct layout)
    const hwm_max_pos_offset = 16940;
    const hwm_rule_id_offset = 16944;

    if (is_silent) {
        // Silent rule: no node for itself, but track child count so the caller
        // can adopt this rule's non-silent children as its own direct children.
        // Return convention: (child_count << 32) | position on success, -1 on failure.
        cg.child_count_ptr = LB.llvm.LLVMBuildAlloca(b.b, b.i32, "silent_cc");
        _ = b.store(b.constInt(b.i32, 0), cg.child_count_ptr, 4);
        const fail_block = try b.newBlock("fail");
        const result_pos = try emitExpr(cg, rule.expr, pos_arg, fail_block);

        // Success: pack child count into upper 32 bits of return value
        const cc_val = b.load(b.i32, cg.child_count_ptr, 4, "silent_cc_val");
        const cc_i64 = b.zext(cc_val, b.i64, "cc_i64");
        const cc_shifted = b.shl(cc_i64, b.constInt(b.i64, 32), "cc_shifted");
        const ret_val = b.@"or"(cc_shifted, result_pos, "packed_ret");
        _ = b.ret(ret_val);

        // Fail block: update high-water mark, then return -1
        b.positionAtEnd(fail_block);
        const hwm_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, hwm_max_pos_offset)}, "hwm_ptr");
        const cur_hwm = b.load(b.i32, hwm_ptr, 4, "cur_hwm");
        const pos_i32 = b.trunc(pos_arg, b.i32, "pos_i32");
        const is_further = b.icmp(.ugt, pos_i32, cur_hwm, "is_further");
        const update_hwm = try b.newBlock("update_hwm");
        const skip_hwm = try b.newBlock("skip_hwm");
        _ = b.condBr(is_further, update_hwm, skip_hwm);

        b.positionAtEnd(update_hwm);
        _ = b.store(pos_i32, hwm_ptr, 4);
        const hwm_rid_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, hwm_rule_id_offset)}, "hwm_rid_ptr");
        _ = b.store(b.constInt(b.i16, rule_id), hwm_rid_ptr, 2);
        _ = b.br(skip_hwm);

        b.positionAtEnd(skip_hwm);
        _ = b.ret(b.constSInt(b.i64, -1));
    } else {
        // Non-silent rule: reserve node, match, fill node on success
        const fail_block = try b.newBlock("fail");
        const alloc_fail_block = try b.newBlock("alloc_fail");
        const alloc_ok_block = try b.newBlock("alloc_ok");
        const slow_alloc_block = try b.newBlock("slow_alloc");
        const after_alloc_block = try b.newBlock("after_alloc");

        // Inline fast path for node reservation:
        // if (node_count < node_capacity) { idx = node_count; node_count++; } else { call ensure_capacity }
        const node_count_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, 16)}, "nc_ptr");
        const node_cap_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, 20)}, "cap_ptr");
        const cur_count = b.load(b.i32, node_count_ptr, 4, "cur_nc");
        const cur_cap = b.load(b.i32, node_cap_ptr, 4, "cur_cap");
        const has_room = b.icmp(.ult, cur_count, cur_cap, "has_room");
        const fast_path_block = b.getCurrentBlock();
        _ = b.condBr(has_room, alloc_ok_block, slow_alloc_block);

        // Slow path: call ensure_capacity, then check
        b.positionAtEnd(slow_alloc_block);
        const needed = b.add(cur_count, b.constInt(b.i32, 1), "needed");
        const ensure_ok = b.call(cg.helper_ensure_capacity_type, cg.helper_ensure_capacity, &.{ cg.output_ptr, needed }, "ensure_ok");
        const ensure_failed = b.icmp(.eq, ensure_ok, b.constInt(b.i32, 0), "ensure_fail");
        _ = b.condBr(ensure_failed, alloc_fail_block, after_alloc_block);

        // After slow alloc: reload count (ensure_capacity doesn't change it, but capacity changed)
        b.positionAtEnd(after_alloc_block);
        _ = b.br(alloc_ok_block);

        // alloc_ok block: phi for node_idx from fast or slow path
        b.positionAtEnd(alloc_ok_block);
        const node_idx = b.phi(b.i32, "node_idx");
        b.addIncoming(node_idx, &.{ cur_count, cur_count }, &.{ fast_path_block, after_alloc_block });

        // Store incremented node_count
        const new_count = b.add(node_idx, b.constInt(b.i32, 1), "new_nc");
        _ = b.store(new_count, node_count_ptr, 4);

        // Initialize child count to 0 for this rule
        cg.child_count_ptr = LB.llvm.LLVMBuildAlloca(b.b, b.i32, "child_count");
        _ = b.store(b.constInt(b.i32, 0), cg.child_count_ptr, 4);

        // Match expression
        const result_pos = try emitExpr(cg, rule.expr, pos_arg, fail_block);

        // Success: compute subtree info and fill node inline
        const final_node_count = b.load(b.i32, node_count_ptr, 4, "final_nc");
        const idx_plus_1 = b.add(node_idx, b.constInt(b.i32, 1), "idx_plus_1");
        const subtree_size = b.sub(final_node_count, idx_plus_1, "subtree_sz");

        // Inline fill_node: write directly to nodes_ptr[idx]
        // FlatNode is 16 bytes: { text_start: u32, text_end: u32, subtree_size: u32, child_count_and_rule: u32 }
        const nodes_ptr_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, 8)}, "nodes_pp");
        const nodes_base = b.load(b.ptr, nodes_ptr_ptr, 8, "nodes_base");
        const node_idx_i64 = b.zext(node_idx, b.i64, "nidx64");
        // GEP by 16 bytes per node (FlatNode size)
        const node_offset = b.shl(node_idx_i64, b.constInt(b.i64, 4), "noff"); // idx * 16
        const node_ptr = b.gep(b.i8, nodes_base, &.{node_offset}, "node_ptr");

        // Truncate pos values to i32 for FlatNode
        const start_i32 = b.trunc(pos_arg, b.i32, "start_i32");
        const end_i32 = b.trunc(result_pos, b.i32, "end_i32");

        // child_count_and_rule = (rule_id << 16) | child_count
        const child_count = b.load(b.i32, cg.child_count_ptr, 4, "cc");
        const rule_shifted = b.constInt(b.i32, @as(u32, rule_id) << 16);
        const rule_field = b.@"or"(rule_shifted, child_count, "cc_rule");

        // Store 4 u32 fields
        const f0_ptr = node_ptr;
        _ = b.store(start_i32, f0_ptr, 4);
        const f1_ptr = b.gep(b.i8, node_ptr, &.{b.constInt(b.i64, 4)}, "f1");
        _ = b.store(end_i32, f1_ptr, 4);
        const f2_ptr = b.gep(b.i8, node_ptr, &.{b.constInt(b.i64, 8)}, "f2");
        _ = b.store(subtree_size, f2_ptr, 4);
        const f3_ptr = b.gep(b.i8, node_ptr, &.{b.constInt(b.i64, 12)}, "f3");
        _ = b.store(rule_field, f3_ptr, 4);

        _ = b.ret(result_pos);

        // Fail block: rollback node_count, update high-water mark, return -1
        b.positionAtEnd(fail_block);
        _ = b.store(node_idx, node_count_ptr, 4); // rollback to before we reserved
        const hwm_ptr2 = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, hwm_max_pos_offset)}, "hwm_ptr2");
        const cur_hwm2 = b.load(b.i32, hwm_ptr2, 4, "cur_hwm2");
        const pos_i32_2 = b.trunc(pos_arg, b.i32, "pos_i32_2");
        const is_further2 = b.icmp(.ugt, pos_i32_2, cur_hwm2, "is_further2");
        const update_hwm2 = try b.newBlock("update_hwm2");
        const skip_hwm2 = try b.newBlock("skip_hwm2");
        _ = b.condBr(is_further2, update_hwm2, skip_hwm2);

        b.positionAtEnd(update_hwm2);
        _ = b.store(pos_i32_2, hwm_ptr2, 4);
        const hwm_rid_ptr2 = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, hwm_rule_id_offset)}, "hwm_rid_ptr2");
        _ = b.store(b.constInt(b.i16, rule_id), hwm_rid_ptr2, 2);
        _ = b.br(skip_hwm2);

        b.positionAtEnd(skip_hwm2);
        _ = b.ret(b.constSInt(b.i64, -1));

        // Alloc fail block: return -1
        b.positionAtEnd(alloc_fail_block);
        _ = b.ret(b.constSInt(b.i64, -1));
    }
}

/// Emit LLVM IR for an expression. Returns the Value representing the new position.
/// On failure, branches to `fail_block`. On success, falls through with the result position.
fn emitExpr(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    switch (expr.tag) {
        .literal => return emitLiteral(cg, expr, pos, fail_block),
        .char_class => return emitCharClass(cg, expr, pos, fail_block),
        .any_char => return emitAnyChar(cg, pos, fail_block),
        .reference => return emitReference(cg, expr, pos, fail_block),
        .sequence => return emitSequence(cg, expr, pos, fail_block),
        .alternative => return emitAlternative(cg, expr, pos, fail_block),
        .repetition => return emitRepetition(cg, expr, pos, fail_block),
        .not_predicate => return emitNotPredicate(cg, expr, pos, fail_block),
        .and_predicate => return emitAndPredicate(cg, expr, pos, fail_block),
    }
}

/// Pack bytes into an integer constant (little-endian).
fn packLitBytes(lit: []const u8, offset: usize, width: usize) u64 {
    var val: u64 = 0;
    for (0..width) |i| {
        val |= @as(u64, lit[offset + i]) << @as(u6, @intCast(i * 8));
    }
    return val;
}

/// Emit literal matching using word-aligned comparisons.
/// Chunks the literal into 8/4/2/1-byte pieces, each compared with a single load+icmp.
fn emitLiteral(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const lit = expr.literal_value orelse return CodegenError.InvalidGrammar;
    const lit_len = lit.len;

    // Check: pos + lit_len <= input_len
    const end_pos = b.add(pos, b.constInt(b.i64, lit_len), "end");
    const bounds_ok = b.icmp(.ule, end_pos, cg.input_len, "bounds");
    const check_block = try b.newBlock("lit_check");
    _ = b.condBr(bounds_ok, check_block, fail_block);

    b.positionAtEnd(check_block);

    // Chunk the literal into word-sized comparisons
    var offset: usize = 0;
    var current_block = check_block;

    while (offset < lit_len) {
        b.positionAtEnd(current_block);
        const remaining = lit_len - offset;

        // Pick the largest chunk size that fits
        const chunk_size: usize = if (remaining >= 8) 8 else if (remaining >= 4) 4 else if (remaining >= 2) 2 else 1;
        const load_ty = switch (chunk_size) {
            8 => b.i64,
            4 => b.i32,
            2 => b.i16,
            1 => b.i8,
            else => unreachable,
        };

        const chunk_ptr = b.gep(b.i8, cg.input_ptr, &.{b.add(pos, b.constInt(b.i64, offset), "off")}, "lptr");
        const loaded = b.load(load_ty, chunk_ptr, 1, "lval");
        const expected = b.constInt(load_ty, packLitBytes(lit, offset, chunk_size));
        const match = b.icmp(.eq, loaded, expected, "lmatch");

        offset += chunk_size;

        if (offset < lit_len) {
            const next_block = try b.newBlock("lit_next");
            _ = b.condBr(match, next_block, fail_block);
            current_block = next_block;
        } else {
            const ok_block = try b.newBlock("lit_ok");
            _ = b.condBr(match, ok_block, fail_block);
            b.positionAtEnd(ok_block);
        }
    }

    return end_pos;
}

/// Emit character class matching using 32-byte bitmap lookup.
fn emitCharClass(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const ranges = expr.char_ranges orelse return CodegenError.InvalidGrammar;
    const negated = expr.char_negated;

    // Check: pos < input_len
    const in_bounds = b.icmp(.ult, pos, cg.input_len, "inb");
    const check_block = try b.newBlock("cc_check");
    _ = b.condBr(in_bounds, check_block, fail_block);

    b.positionAtEnd(check_block);

    // Load the byte at input[pos]
    const byte_ptr = b.gep(b.i8, cg.input_ptr, &.{pos}, "cc_bptr");
    const ch = b.load(b.i8, byte_ptr, 1, "ch");

    // Build the 32-byte bitmap at compile time
    var bitmap: [32]u8 = [_]u8{0} ** 32;
    for (ranges) |r| {
        var cv: u16 = r.start;
        while (cv <= r.end) : (cv += 1) {
            bitmap[@as(u8, @truncate(cv)) >> 3] |= @as(u8, 1) << @as(u3, @truncate(@as(u8, @truncate(cv))));
        }
    }
    if (negated) {
        for (&bitmap) |*bv| bv.* = ~bv.*;
    }

    // Create bitmap as a global constant
    var bitmap_consts: [32]LB.Value = undefined;
    for (bitmap, 0..) |bv, idx| {
        bitmap_consts[idx] = b.constInt(b.i8, bv);
    }
    const bitmap_val = b.constArray(b.i8, &bitmap_consts);
    const bitmap_ty = b.arrayType(b.i8, 32);

    const bitmap_name = std.fmt.allocPrintSentinel(b.allocator, "bitmap_{d}", .{b.block_counter}, 0) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(bitmap_name);
    const bitmap_global = b.addGlobalConstant(bitmap_name, bitmap_ty, bitmap_val);

    // byte_index = ch >> 3 (zero-extended to i64 for GEP)
    const ch_i32 = b.zext(ch, b.i32, "ch32");
    const byte_index = b.lshr(ch_i32, b.constInt(b.i32, 3), "bidx");
    const byte_index_i64 = b.zext(byte_index, b.i64, "bidx64");

    // GEP into bitmap: &bitmap[byte_index]
    const bitmap_byte_ptr = b.gep(b.i8, bitmap_global, &.{byte_index_i64}, "bm_ptr");
    const bitmap_byte = b.load(b.i8, bitmap_byte_ptr, 1, "bm_byte");

    // bit_mask = 1 << (ch & 7)
    const bit_pos = b.@"and"(ch, b.constInt(b.i8, 7), "bpos");
    const bit_mask = b.shl(b.constInt(b.i8, 1), bit_pos, "bmask");

    // result = bitmap_byte & bit_mask
    const test_result = b.@"and"(bitmap_byte, bit_mask, "tst");
    const is_match = b.icmp(.ne, test_result, b.constInt(b.i8, 0), "cc_match");

    const ok_block = try b.newBlock("cc_ok");
    _ = b.condBr(is_match, ok_block, fail_block);
    b.positionAtEnd(ok_block);

    // Return pos + 1
    return b.add(pos, b.constInt(b.i64, 1), "cc_next");
}

/// Emit any_char matching: just check pos < input_len.
fn emitAnyChar(cg: *Codegen, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const in_bounds = b.icmp(.ult, pos, cg.input_len, "any_inb");
    const ok_block = try b.newBlock("any_ok");
    _ = b.condBr(in_bounds, ok_block, fail_block);
    b.positionAtEnd(ok_block);
    return b.add(pos, b.constInt(b.i64, 1), "any_next");
}

/// Emit rule reference: call the rule function.
fn emitReference(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const ref_name = expr.ref_name orelse return CodegenError.InvalidGrammar;

    // Resolve rule name to index
    var rule_idx: usize = 0;
    var found = false;
    for (cg.grammar.rules, 0..) |rule, i| {
        if (std.mem.eql(u8, rule.name, ref_name)) {
            rule_idx = i;
            found = true;
            break;
        }
    }
    if (!found) return CodegenError.InvalidGrammar;

    // Call rule function
    const result = b.call(cg.rule_fn_type, cg.rule_fns[rule_idx], &.{ cg.input_ptr, cg.input_len, cg.output_ptr, pos }, "ref_result");

    // Check if failed (result == -1)
    const failed = b.icmp(.eq, result, b.constSInt(b.i64, -1), "ref_fail");
    const ok_block = try b.newBlock("ref_ok");
    _ = b.condBr(failed, fail_block, ok_block);
    b.positionAtEnd(ok_block);

    if (cg.silent_flags[rule_idx]) {
        // Silent rule packs child count in upper 32 bits: (cc << 32) | pos
        // Extract position from lower 32 bits, child count from upper 32 bits
        const pos_masked = b.@"and"(result, b.constInt(b.i64, 0xFFFFFFFF), "silent_pos");

        // Add the silent rule's child count to our own (if we're tracking)
        if (cg.child_count_ptr != null) {
            const silent_cc = b.lshr(result, b.constInt(b.i64, 32), "silent_cc");
            const silent_cc_i32 = b.trunc(silent_cc, b.i32, "silent_cc32");
            const cur_cc = b.load(b.i32, cg.child_count_ptr, 4, "cur_cc");
            const new_cc = b.add(cur_cc, silent_cc_i32, "new_cc");
            _ = b.store(new_cc, cg.child_count_ptr, 4);
        }

        return pos_masked;
    } else {
        // Non-silent rule: increment parent's direct child count by 1
        if (cg.child_count_ptr != null) {
            const cur_cc = b.load(b.i32, cg.child_count_ptr, 4, "cur_cc");
            const new_cc = b.add(cur_cc, b.constInt(b.i32, 1), "new_cc");
            _ = b.store(new_cc, cg.child_count_ptr, 4);
        }

        return result;
    }
}

/// Emit sequence: match each child in order, fail if any fails.
fn emitSequence(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const children = expr.children orelse return CodegenError.InvalidGrammar;
    var cur_pos = pos;
    for (children) |child| {
        cur_pos = try emitExpr(cg, child, cur_pos, fail_block);
    }
    return cur_pos;
}

/// Check if an expression can allocate nodes when evaluated.
/// Returns false for expressions that are guaranteed to never allocate (literals,
/// char classes, any_char, predicates, references to silent rules, and combinations thereof).
fn exprAllocatesNodes(cg: *const Codegen, expr: *const gp.Expr) bool {
    switch (expr.tag) {
        .literal, .char_class, .any_char => return false,
        .not_predicate, .and_predicate => return false, // predicates always restore node_count
        .reference => {
            // Check if the referenced rule is silent
            const ref_name = expr.ref_name orelse return true;
            for (cg.grammar.rules, 0..) |rule, i| {
                if (std.mem.eql(u8, rule.name, ref_name)) {
                    return !cg.silent_flags[i];
                }
            }
            return true; // unknown rule, assume it allocates
        },
        .sequence => {
            const children = expr.children orelse return false;
            for (children) |child| {
                if (exprAllocatesNodes(cg, child)) return true;
            }
            return false;
        },
        .alternative => {
            const children = expr.children orelse return false;
            for (children) |child| {
                if (exprAllocatesNodes(cg, child)) return true;
            }
            return false;
        },
        .repetition => {
            const sub = expr.rep_expr orelse return false;
            return exprAllocatesNodes(cg, sub);
        },
    }
}

/// Check if an expression always consumes at least 1 byte when it succeeds.
/// Used to skip zero-length match checks in repetition loops.
fn exprAlwaysConsumes(expr: *const gp.Expr) bool {
    switch (expr.tag) {
        .literal => return if (expr.literal_value) |lit| lit.len > 0 else false,
        .char_class, .any_char => return true,
        .reference => return false, // can't know without deeper analysis
        .sequence => {
            // A sequence consumes if any child consumes
            const children = expr.children orelse return false;
            for (children) |child| {
                if (exprAlwaysConsumes(child)) return true;
            }
            return false;
        },
        .alternative => {
            // An alternative consumes if ALL children consume
            const children = expr.children orelse return false;
            for (children) |child| {
                if (!exprAlwaysConsumes(child)) return false;
            }
            return children.len > 0;
        },
        .repetition => return false, // *  and ? can match zero
        .not_predicate, .and_predicate => return false, // predicates never consume
    }
}

/// Emit alternative: try each child, restore state on failure, try next.
fn emitAlternative(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const children = expr.children orelse return CodegenError.InvalidGrammar;
    if (children.len == 0) return CodegenError.InvalidGrammar;

    // Check if any alternative can allocate nodes — if not, skip save/restore
    var needs_save_restore = false;
    for (children) |child| {
        if (exprAllocatesNodes(cg, child)) {
            needs_save_restore = true;
            break;
        }
    }

    // We need a merge block where successful alternatives converge
    const merge_block = try b.newBlock("alt_merge");

    // For the phi node: collect (value, block) pairs
    const phi_vals = b.allocator.alloc(LB.Value, children.len) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(phi_vals);
    const phi_blocks = b.allocator.alloc(LB.Block, children.len) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(phi_blocks);
    var phi_count: usize = 0;

    // node_count pointer for save/restore (only if needed)
    var node_count_ptr: LB.Value = undefined;
    var saved_nc: LB.Value = undefined;
    if (needs_save_restore) {
        node_count_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, 16)}, "alt_nc_ptr");
        saved_nc = b.load(b.i32, node_count_ptr, 4, "alt_saved_nc");
    }

    for (children, 0..) |child, i| {
        const is_last = (i + 1 == children.len);
        const child_fail = if (is_last) fail_block else try b.newBlock("alt_next");

        // Restore node_count before trying each alternative (except first)
        if (needs_save_restore and i > 0) {
            _ = b.store(saved_nc, node_count_ptr, 4);
        }

        const result_pos = try emitExpr(cg, child, pos, child_fail);

        // Success: branch to merge
        phi_vals[phi_count] = result_pos;
        phi_blocks[phi_count] = b.getCurrentBlock();
        phi_count += 1;
        _ = b.br(merge_block);

        if (!is_last) {
            b.positionAtEnd(child_fail);
        }
    }

    // Merge block: phi to select the successful result
    b.positionAtEnd(merge_block);
    const phi_node = b.phi(b.i64, "alt_pos");
    b.addIncoming(phi_node, phi_vals[0..phi_count], phi_blocks[0..phi_count]);

    return phi_node;
}

// ── SIMD Character Scanning ──

/// SIMD pattern classification for character classes.
const SimdPattern = enum {
    /// Single contiguous range: [a-z], [0-9], [A-Z]
    /// Uses vector uge + ule comparisons.
    single_range,

    /// Small set of included chars (<=4): [ \t\n\r]
    /// Uses vector equality OR-chain.
    small_included_set,

    /// Small set of excluded chars (<=4, negated): [^"\\], [^\n]
    /// Uses vector equality OR-chain, then NOT.
    small_excluded_set,

    /// Not suitable for SIMD.
    unsupported,
};

/// Extra info extracted during classification.
const SimdClassification = struct {
    pattern: SimdPattern,
    /// For single_range: the lo and hi bounds.
    range_lo: u8 = 0,
    range_hi: u8 = 0,
    /// For small sets: the individual characters (up to 4).
    chars: [4]u8 = .{ 0, 0, 0, 0 },
    char_count: u8 = 0,
};

/// Classify a char_class expression for SIMD suitability.
fn classifySimd(expr: *const gp.Expr) SimdClassification {
    const ranges = expr.char_ranges orelse return .{ .pattern = .unsupported };
    const negated = expr.char_negated;

    if (!negated and ranges.len == 1 and ranges[0].start != ranges[0].end) {
        // Single contiguous range like [a-z]
        return .{
            .pattern = .single_range,
            .range_lo = ranges[0].start,
            .range_hi = ranges[0].end,
        };
    }

    // Count total individual characters in ranges
    var total_chars: u16 = 0;
    for (ranges) |r| {
        total_chars += @as(u16, r.end) - @as(u16, r.start) + 1;
        if (total_chars > 4) break;
    }

    if (total_chars <= 4) {
        // Extract individual characters
        var chars: [4]u8 = .{ 0, 0, 0, 0 };
        var idx: u8 = 0;
        for (ranges) |r| {
            var ch: u16 = r.start;
            while (ch <= r.end) : (ch += 1) {
                if (idx >= 4) break;
                chars[idx] = @truncate(ch);
                idx += 1;
            }
        }

        if (negated) {
            return .{
                .pattern = .small_excluded_set,
                .chars = chars,
                .char_count = idx,
            };
        } else {
            return .{
                .pattern = .small_included_set,
                .chars = chars,
                .char_count = idx,
            };
        }
    }

    return .{ .pattern = .unsupported };
}

/// Check if a repetition sub-expression is suitable for SIMD scanning.
fn canSimdScan(expr: *const gp.Expr) bool {
    if (expr.tag != .char_class) return false;
    const cls = classifySimd(expr);
    return cls.pattern != .unsupported;
}

/// Emit a SIMD-accelerated character class scanning loop.
/// Processes 16 bytes per iteration using SSE2 vector operations.
///
/// Generated structure:
///   vec_check: can we load 16 bytes? if yes → vec_body, else → scalar_loop
///   vec_body:  load <16 x i8>, vector compare, all match? → vec_check, else → find_end
///   find_end:  bitcast mismatch mask to i16, cttz to find first mismatch offset
///   scalar_loop: one byte at a time for tail < 16 bytes
///   exit: phi merges results from find_end and scalar_loop
fn emitSimdCharScan(cg: *Codegen, expr: *const gp.Expr, start_pos: LB.Value, fail_block: LB.Block, rep_kind: u8) CodegenError!LB.Value {
    const b = cg.b;
    const cls = classifySimd(expr);

    const vec16i8 = b.vectorType(b.i8, 16);
    const vec16i1 = b.vectorType(b.i1, 16);

    // Look up intrinsics we need
    const cttz_id = b.lookupIntrinsic("llvm.cttz");
    const reduce_and_id = b.lookupIntrinsic("llvm.vector.reduce.and");

    // Blocks
    const vec_check = try b.newBlock("simd_check");
    const vec_body = try b.newBlock("simd_body");
    const find_end = try b.newBlock("simd_find_end");
    const scalar_loop = try b.newBlock("simd_scalar");
    const scalar_body = try b.newBlock("simd_scalar_body");
    const scalar_exit = try b.newBlock("simd_scalar_exit");
    const exit = try b.newBlock("simd_exit");

    const entry_block = b.getCurrentBlock();
    _ = b.br(vec_check);

    // ── vec_check: can we load 16 bytes? ──
    b.positionAtEnd(vec_check);
    const pos_phi = b.phi(b.i64, "simd_pos");
    const remaining = b.sub(cg.input_len, pos_phi, "rem");
    const can_vec = b.icmp(.uge, remaining, b.constInt(b.i64, 16), "can_vec");
    _ = b.condBr(can_vec, vec_body, scalar_loop);

    // ── vec_body: load 16 bytes, vector compare ──
    b.positionAtEnd(vec_body);
    const chunk_ptr = b.gep(b.i8, cg.input_ptr, &.{pos_phi}, "chunk_ptr");
    const chunk = b.load(vec16i8, chunk_ptr, 1, "chunk");

    // Generate pattern-specific vector mask
    const match_mask = switch (cls.pattern) {
        .single_range => blk: {
            // %ge = icmp uge <16 x i8> %chunk, splat(lo)
            // %le = icmp ule <16 x i8> %chunk, splat(hi)
            // %mask = and %ge, %le
            const lo_splat = b.splatVector(b.constInt(b.i8, cls.range_lo), 16);
            const hi_splat = b.splatVector(b.constInt(b.i8, cls.range_hi), 16);
            const ge = b.icmp(.uge, chunk, lo_splat, "vec_ge");
            const le = b.icmp(.ule, chunk, hi_splat, "vec_le");
            break :blk b.@"and"(ge, le, "vec_mask");
        },
        .small_included_set => blk: {
            // OR-chain of equality comparisons
            var mask: LB.Value = undefined;
            for (0..cls.char_count) |i| {
                const splat = b.splatVector(b.constInt(b.i8, cls.chars[i]), 16);
                const eq = b.icmp(.eq, chunk, splat, "vec_eq");
                if (i == 0) {
                    mask = eq;
                } else {
                    mask = b.@"or"(mask, eq, "vec_or");
                }
            }
            break :blk mask;
        },
        .small_excluded_set => blk: {
            // OR-chain of equality for excluded chars, then NOT
            var excluded: LB.Value = undefined;
            for (0..cls.char_count) |i| {
                const splat = b.splatVector(b.constInt(b.i8, cls.chars[i]), 16);
                const eq = b.icmp(.eq, chunk, splat, "vec_eq");
                if (i == 0) {
                    excluded = eq;
                } else {
                    excluded = b.@"or"(excluded, eq, "vec_or");
                }
            }
            // Negate: match = NOT excluded
            const all_true = b.splatVector(b.constInt(b.i1, 1), 16);
            break :blk b.xor(excluded, all_true, "vec_mask");
        },
        .unsupported => unreachable,
    };

    // Check if ALL 16 bytes matched
    const all_match = b.callIntrinsic(reduce_and_id, &.{vec16i1}, &.{match_mask}, "all_match");
    const next_pos = b.add(pos_phi, b.constInt(b.i64, 16), "next_pos");
    const vec_body_end = b.getCurrentBlock();
    _ = b.condBr(all_match, vec_check, find_end);

    // Add phi incoming for vec_check
    b.addIncoming(pos_phi, &.{ start_pos, next_pos }, &.{ entry_block, vec_body_end });

    // ── find_end: find first non-matching byte in the 16-byte chunk ──
    b.positionAtEnd(find_end);
    // Invert the mask: we want the first bit that is 0 (non-matching)
    const inv_mask = b.xor(match_mask, b.splatVector(b.constInt(b.i1, 1), 16), "inv_mask");
    // Bitcast <16 x i1> to i16
    const bitmask = b.bitcast(inv_mask, b.i16, "bitmask");
    // Count trailing zeros to find first mismatch position
    const offset = b.callIntrinsic(cttz_id, &.{b.i16}, &.{ bitmask, b.constInt(b.i1, 0) }, "offset");
    const offset64 = b.zext(offset, b.i64, "offset64");
    const find_end_pos = b.add(pos_phi, offset64, "find_end_pos");
    _ = b.br(exit);

    // ── scalar_loop: handle remaining < 16 bytes ──
    b.positionAtEnd(scalar_loop);
    const scalar_pos_phi = b.phi(b.i64, "scalar_pos");
    // Check bounds
    const scalar_in_bounds = b.icmp(.ult, scalar_pos_phi, cg.input_len, "s_inb");
    _ = b.condBr(scalar_in_bounds, scalar_body, scalar_exit);

    // scalar_body: test one byte with bitmap (reuse existing char class logic)
    b.positionAtEnd(scalar_body);
    const s_byte_ptr = b.gep(b.i8, cg.input_ptr, &.{scalar_pos_phi}, "s_bptr");
    const s_ch = b.load(b.i8, s_byte_ptr, 1, "s_ch");

    // Build bitmap (same as emitCharClass)
    const ranges = expr.char_ranges orelse return CodegenError.InvalidGrammar;
    const negated = expr.char_negated;
    var bitmap: [32]u8 = [_]u8{0} ** 32;
    for (ranges) |r| {
        var cv: u16 = r.start;
        while (cv <= r.end) : (cv += 1) {
            bitmap[@as(u8, @truncate(cv)) >> 3] |= @as(u8, 1) << @as(u3, @truncate(@as(u8, @truncate(cv))));
        }
    }
    if (negated) {
        for (&bitmap) |*bv| bv.* = ~bv.*;
    }

    var bitmap_consts: [32]LB.Value = undefined;
    for (bitmap, 0..) |bv, idx| {
        bitmap_consts[idx] = b.constInt(b.i8, bv);
    }
    const bitmap_val = b.constArray(b.i8, &bitmap_consts);
    const bitmap_ty = b.arrayType(b.i8, 32);
    const bm_name = std.fmt.allocPrintSentinel(b.allocator, "simd_bm_{d}", .{b.block_counter}, 0) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(bm_name);
    const bitmap_global = b.addGlobalConstant(bm_name, bitmap_ty, bitmap_val);

    const s_ch_i32 = b.zext(s_ch, b.i32, "s_ch32");
    const s_byte_index = b.lshr(s_ch_i32, b.constInt(b.i32, 3), "s_bidx");
    const s_byte_index_i64 = b.zext(s_byte_index, b.i64, "s_bidx64");
    const s_bm_ptr = b.gep(b.i8, bitmap_global, &.{s_byte_index_i64}, "s_bm_ptr");
    const s_bm_byte = b.load(b.i8, s_bm_ptr, 1, "s_bm_byte");
    const s_bit_pos = b.@"and"(s_ch, b.constInt(b.i8, 7), "s_bpos");
    const s_bit_mask = b.shl(b.constInt(b.i8, 1), s_bit_pos, "s_bmask");
    const s_test = b.@"and"(s_bm_byte, s_bit_mask, "s_tst");
    const s_match = b.icmp(.ne, s_test, b.constInt(b.i8, 0), "s_match");

    const scalar_next = b.add(scalar_pos_phi, b.constInt(b.i64, 1), "s_next");
    const scalar_body_end = b.getCurrentBlock();
    _ = b.condBr(s_match, scalar_loop, scalar_exit);

    // Incoming for scalar_pos_phi: from vec_check (when < 16 remaining) and from scalar_body (loop back)
    b.addIncoming(scalar_pos_phi, &.{ pos_phi, scalar_next }, &.{ vec_check, scalar_body_end });

    // ── scalar_exit: done scanning byte-by-byte ──
    b.positionAtEnd(scalar_exit);
    const scalar_final = b.phi(b.i64, "s_final");
    b.addIncoming(scalar_final, &.{ scalar_pos_phi, scalar_pos_phi }, &.{ scalar_loop, scalar_body_end });
    _ = b.br(exit);

    // ── exit: merge results ──
    b.positionAtEnd(exit);
    const final_pos = b.phi(b.i64, "simd_final");
    b.addIncoming(final_pos, &.{ find_end_pos, scalar_final }, &.{ find_end, scalar_exit });

    // For '+' repetition, we need at least one match
    if (rep_kind == '+') {
        const no_match = b.icmp(.eq, final_pos, start_pos, "no_match");
        const ok_block = try b.newBlock("simd_ok");
        _ = b.condBr(no_match, fail_block, ok_block);
        b.positionAtEnd(ok_block);
        return final_pos;
    }

    return final_pos;
}

/// Emit repetition: *, +, ?
fn emitRepetition(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const sub = expr.rep_expr orelse return CodegenError.InvalidGrammar;
    const kind = expr.rep_kind;

    // SIMD fast path: if sub-expression is a char_class with a supported pattern,
    // emit vectorized 16-bytes-at-a-time scanning instead of byte-by-byte loop.
    if ((kind == '*' or kind == '+') and canSimdScan(sub)) {
        return emitSimdCharScan(cg, sub, pos, fail_block, kind);
    }

    const needs_nc_save = exprAllocatesNodes(cg, sub);
    const always_consumes = exprAlwaysConsumes(sub);

    if (kind == '?') {
        // Optional: try once, succeed either way
        const try_fail = try b.newBlock("opt_fail");
        const merge = try b.newBlock("opt_merge");

        var nc_ptr: LB.Value = undefined;
        var saved_nc: LB.Value = undefined;
        if (needs_nc_save) {
            nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, 16)}, "opt_nc_ptr");
            saved_nc = b.load(b.i32, nc_ptr, 4, "opt_saved_nc");
        }

        const result_pos = try emitExpr(cg, sub, pos, try_fail);
        const success_block = b.getCurrentBlock();
        _ = b.br(merge);

        // Fail: restore node_count if needed, use original pos
        b.positionAtEnd(try_fail);
        if (needs_nc_save) _ = b.store(saved_nc, nc_ptr, 4);
        _ = b.br(merge);

        // Merge
        b.positionAtEnd(merge);
        const phi_node = b.phi(b.i64, "opt_pos");
        b.addIncoming(phi_node, &.{ result_pos, pos }, &.{ success_block, try_fail });

        return phi_node;
    }

    // * or +: loop
    const loop_header = try b.newBlock("rep_header");
    const loop_body = try b.newBlock("rep_body");
    const loop_fail = try b.newBlock("rep_fail");
    const loop_exit = try b.newBlock("rep_exit");

    var nc_ptr: LB.Value = undefined;
    if (needs_nc_save) {
        nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, 16)}, "rep_nc_ptr");
    }

    if (kind == '+') {
        // +: try first match, fail entirely if it doesn't match
        const first_result = try emitExpr(cg, sub, pos, fail_block);
        const first_block = b.getCurrentBlock();
        _ = b.br(loop_header);

        // Loop header: phi for current position
        b.positionAtEnd(loop_header);
        const pos_phi = b.phi(b.i64, "rep_pos");

        // Branch to body
        _ = b.br(loop_body);

        // Loop body
        b.positionAtEnd(loop_body);
        var saved_nc: LB.Value = undefined;
        if (needs_nc_save) saved_nc = b.load(b.i32, nc_ptr, 4, "rep_saved");
        const body_result = try emitExpr(cg, sub, pos_phi, loop_fail);
        const body_end_block = b.getCurrentBlock();
        if (always_consumes) {
            // Sub-expr always consumes >=1 byte, no need for zero-length check
            _ = b.br(loop_header);
        } else {
            const no_progress = b.icmp(.eq, body_result, pos_phi, "no_prog");
            _ = b.condBr(no_progress, loop_exit, loop_header);
        }

        // Loop fail: restore node_count if needed, exit loop
        b.positionAtEnd(loop_fail);
        if (needs_nc_save) _ = b.store(saved_nc, nc_ptr, 4);
        _ = b.br(loop_exit);

        // Finish phi: incoming from entry (first result) and from body (next result)
        b.addIncoming(pos_phi, &.{ first_result, body_result }, &.{ first_block, body_end_block });

        // Exit
        b.positionAtEnd(loop_exit);
        if (always_consumes) {
            // Only one incoming edge: loop_fail
            return pos_phi;
        } else {
            const exit_phi = b.phi(b.i64, "rep_exit_pos");
            b.addIncoming(exit_phi, &.{ pos_phi, pos_phi }, &.{ loop_fail, body_end_block });
            return exit_phi;
        }
    } else {
        // * (zero or more)
        const entry_block = b.getCurrentBlock();
        _ = b.br(loop_header);

        // Loop header: phi for current position
        b.positionAtEnd(loop_header);
        const pos_phi = b.phi(b.i64, "rep_pos");
        _ = b.br(loop_body);

        // Loop body
        b.positionAtEnd(loop_body);
        var saved_nc: LB.Value = undefined;
        if (needs_nc_save) saved_nc = b.load(b.i32, nc_ptr, 4, "rep_saved");
        const body_result = try emitExpr(cg, sub, pos_phi, loop_fail);
        const body_end_block = b.getCurrentBlock();
        if (always_consumes) {
            _ = b.br(loop_header);
        } else {
            const no_progress = b.icmp(.eq, body_result, pos_phi, "no_prog");
            _ = b.condBr(no_progress, loop_exit, loop_header);
        }

        // Loop fail: restore nc if needed, exit
        b.positionAtEnd(loop_fail);
        if (needs_nc_save) _ = b.store(saved_nc, nc_ptr, 4);
        _ = b.br(loop_exit);

        // Finish phi
        b.addIncoming(pos_phi, &.{ pos, body_result }, &.{ entry_block, body_end_block });

        // Exit
        b.positionAtEnd(loop_exit);
        if (always_consumes) {
            return pos_phi;
        } else {
            const exit_phi = b.phi(b.i64, "rep_exit_pos");
            b.addIncoming(exit_phi, &.{ pos_phi, pos_phi }, &.{ loop_fail, body_end_block });
            return exit_phi;
        }
    }
}

/// Emit not predicate: !expr — succeeds if expr fails, consumes nothing.
fn emitNotPredicate(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const sub = expr.pred_expr orelse return CodegenError.InvalidGrammar;
    const needs_save = exprAllocatesNodes(cg, sub);

    var nc_ptr: LB.Value = undefined;
    var saved_nc: LB.Value = undefined;
    if (needs_save) {
        nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, 16)}, "not_nc_ptr");
        saved_nc = b.load(b.i32, nc_ptr, 4, "not_saved");
    }

    const pred_fail = try b.newBlock("not_fail");

    // Try the sub-expression
    _ = try emitExpr(cg, sub, pos, pred_fail);

    // Sub-expression matched — NOT predicate fails
    if (needs_save) _ = b.store(saved_nc, nc_ptr, 4);
    _ = b.br(fail_block);

    // Sub-expression failed — NOT predicate succeeds
    b.positionAtEnd(pred_fail);
    if (needs_save) _ = b.store(saved_nc, nc_ptr, 4);

    return pos;
}

/// Emit and predicate: &expr — succeeds if expr succeeds, consumes nothing.
fn emitAndPredicate(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const sub = expr.pred_expr orelse return CodegenError.InvalidGrammar;
    const needs_save = exprAllocatesNodes(cg, sub);

    var nc_ptr: LB.Value = undefined;
    var saved_nc: LB.Value = undefined;
    if (needs_save) {
        nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, 16)}, "and_nc_ptr");
        saved_nc = b.load(b.i32, nc_ptr, 4, "and_saved");
    }

    // Try the sub-expression (on fail, branches to fail_block)
    _ = try emitExpr(cg, sub, pos, fail_block);

    // Sub-expression matched — restore node_count
    if (needs_save) _ = b.store(saved_nc, nc_ptr, 4);

    return pos;
}

/// Emit the zgram_parse entry point function.
fn emitParseEntryPoint(
    b: *LB.Builder,
    rule_0: LB.Value,
    rule_fn_type: LB.Type,
    grammar: *const gp.Grammar,
    helper_set_error: LB.Value,
    helper_set_error_type: LB.Type,
    helper_set_error_at_hwm: LB.Value,
    helper_set_error_at_hwm_type: LB.Type,
    helper_set_rule_name: LB.Value,
    helper_set_rule_name_type: LB.Type,
    root_is_silent: bool,
) CodegenError!void {
    // i32 @zgram_parse(ptr input, i64 len, ptr output)
    const parse_fn_type = b.fnType(b.i32, &.{ b.ptr, b.i64, b.ptr });
    const func = b.addFunction("zgram_parse", parse_fn_type);
    b.setCurrentFn(func);

    const entry = b.appendBlock("entry");
    b.positionAtEnd(entry);

    const input_ptr = b.param(func, 0);
    const input_len = b.param(func, 1);
    const output_ptr = b.param(func, 2);

    // Reset output fields
    // status = 0
    _ = b.store(b.constInt(b.i8, 0), output_ptr, 1);
    // node_count = 0 (offset 16)
    const nc_ptr = b.gep(b.i8, output_ptr, &.{b.constInt(b.i64, 16)}, "nc_ptr");
    _ = b.store(b.constInt(b.i32, 0), nc_ptr, 4);
    // max_pos = 0 (offset 16940)
    const hwm_ptr = b.gep(b.i8, output_ptr, &.{b.constInt(b.i64, 16940)}, "hwm_reset_ptr");
    _ = b.store(b.constInt(b.i32, 0), hwm_ptr, 4);

    // Set up rule names (only on first call, using a global flag)
    const names_registered = b.addGlobal("names_registered", b.i8);
    LB.llvm.LLVMSetInitializer(names_registered, b.constInt(b.i8, 0));
    LB.llvm.LLVMSetLinkage(names_registered, LB.llvm.LLVMInternalLinkage);

    const flag_val = b.load(b.i8, names_registered, 1, "names_flag");
    const already_set = b.icmp(.ne, flag_val, b.constInt(b.i8, 0), "names_set");
    const register_names_block = try b.newBlock("register_names");
    const after_names_block = try b.newBlock("after_names");
    _ = b.condBr(already_set, after_names_block, register_names_block);

    b.positionAtEnd(register_names_block);
    for (grammar.rules, 0..) |rule, i| {
        const name_global_name = std.fmt.allocPrintSentinel(b.allocator, "rule_name_{d}", .{i}, 0) catch return CodegenError.OutOfMemory;
        defer b.allocator.free(name_global_name);
        const rule_name_global = b.addGlobalString(name_global_name, rule.name);
        const rule_id_val = b.constInt(b.i16, @as(u16, @intCast(i)));
        const name_len_val = b.constInt(b.i8, @as(u8, @intCast(rule.name.len)));

        _ = b.call(helper_set_rule_name_type, helper_set_rule_name, &.{ output_ptr, rule_id_val, rule_name_global, name_len_val }, "");
    }
    _ = b.store(b.constInt(b.i8, 1), names_registered, 1);
    _ = b.br(after_names_block);

    b.positionAtEnd(after_names_block);

    // Call rule_0(input, len, output, 0)
    const zero_pos = b.constInt(b.i64, 0);
    const result = b.call(rule_fn_type, rule_0, &.{ input_ptr, input_len, output_ptr, zero_pos }, "result");

    // If root rule is silent, it packs child count in upper 32 bits.
    // Extract the position from lower 32 bits; -1 check uses slt since
    // packed values are always non-negative.
    const result_pos = if (root_is_silent)
        b.@"and"(result, b.constInt(b.i64, 0xFFFFFFFF), "root_pos")
    else
        result;

    // Check result
    const failed = b.icmp(.slt, result, b.constSInt(b.i64, 0), "failed");
    const check_full = try b.newBlock("check_full");
    const parse_failed = try b.newBlock("parse_failed");
    _ = b.condBr(failed, parse_failed, check_full);

    // Check full consumption
    b.positionAtEnd(check_full);
    const fully_consumed = b.icmp(.eq, result_pos, input_len, "full");
    const success_block = try b.newBlock("success");
    const partial_block = try b.newBlock("partial");
    _ = b.condBr(fully_consumed, success_block, partial_block);

    // Success: set status = 1, return 0
    b.positionAtEnd(success_block);
    _ = b.store(b.constInt(b.i8, 1), output_ptr, 1);
    _ = b.ret(b.constInt(b.i32, 0));

    // Partial match: set error
    b.positionAtEnd(partial_block);
    const partial_msg = "unexpected input after match";
    const partial_msg_global = b.addGlobalString("partial_err_msg", partial_msg);
    _ = b.call(helper_set_error_type, helper_set_error, &.{ output_ptr, input_ptr, input_len, result_pos, partial_msg_global, b.constInt(b.i64, partial_msg.len) }, "");
    _ = b.ret(b.constInt(b.i32, 0));

    // Parse failed: use high-water mark for error position and rule name
    b.positionAtEnd(parse_failed);
    _ = b.call(helper_set_error_at_hwm_type, helper_set_error_at_hwm, &.{ output_ptr, input_ptr, input_len }, "");
    _ = b.ret(b.constInt(b.i32, 0));
}

/// Compute silent rule flags from explicit @silent annotations.
/// Rules marked @silent produce no parse tree nodes.
fn computeSilentFlags(allocator: Allocator, grammar: *const gp.Grammar) CodegenError![]bool {
    const rule_count = grammar.rules.len;
    const flags = allocator.alloc(bool, rule_count) catch return CodegenError.OutOfMemory;
    for (grammar.rules, 0..) |rule, ri| {
        flags[ri] = rule.silent;
    }
    return flags;
}
