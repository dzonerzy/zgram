//! JIT code generator: converts grammar IR into LLVM IR using the LLVM C API.
//!
//! Builds an in-memory LLVMModuleRef directly (no bitcode step).
//! The module is handed to jit_compiler.zig for JIT compilation via LLJIT.
//!
//! Architecture:
//!   - Each grammar rule becomes a function: rule_N(ptr input, i64 len, ptr output, i64 pos) → i64
//!     Returns new position on match, -1 on failure.
//!   - zgram_parse(ptr input, i64 len, ptr output, i32 start_rule, i32 flags) → i32
//!     is the C ABI entry point.
//!   - Expression types emit basic block patterns within their rule function.
//!   - Memory management (node alloc, error reporting) calls into exported Zig helpers.

const std = @import("std");
const Allocator = std.mem.Allocator;
const gp = @import("grammar_parser.zig");
const LB = @import("llvm_builder.zig");
const abi = @import("parse_abi.zig");

pub const CodegenError = error{
    OutOfMemory,
    InvalidGrammar,
};

/// What the generated parser produces.
pub const Mode = enum {
    /// Build the parse tree and track the furthest failure for error messages
    tree,
    /// Only accept or reject: no nodes, no child counts, no error tracking
    /// (callers re-run the tree parser to explain a rejection). Rules that
    /// aren't part of a recursion cycle are inlined into their callers.
    validate,
    /// Build the tree like `tree`, and recover from syntax errors: a
    /// repetition whose element runs into a known error (ParseOutput
    /// .known_errors) skips the broken text into an error node and goes on.
    /// Where to resume is found with validator copies of the rules, compiled
    /// into the same module.
    recover,
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
    helper_set_error_trailing: LB.Value,
    helper_set_error_at_hwm: LB.Value,
    helper_ensure_capacity: LB.Value,
    helper_tag_field: LB.Value,
    helper_tag_field_type: LB.Type,
    helper_fold: LB.Value,
    helper_fold_type: LB.Type,
    /// In a @left/@right/@postfix rule: the trailing repetition whose
    /// iterations get folded (see emitFoldRepetition)
    fold_rep: ?*const gp.Expr = null,
    /// ... the alloca i32 counting its marked iterations, and the one
    /// holding the node index where the last of them starts
    fold_count_ptr: LB.Value = null,
    fold_iter_ptr: LB.Value = null,

    // Helper function types
    helper_reserve_node_type: LB.Type,
    helper_fill_node_type: LB.Type,
    helper_set_error_trailing_type: LB.Type,
    helper_set_error_at_hwm_type: LB.Type,
    helper_ensure_capacity_type: LB.Type,

    // Grammar info
    grammar: *const gp.Grammar,
    silent_flags: []const bool,

    // Per-rule child count tracking (alloca i32, counts direct children)
    child_count_ptr: LB.Value,

    // Bytes per SIMD step: 32 with AVX2 on this CPU, else 16
    simd_width: u32,

    // Per rule: can calling it add nodes? True for non-silent rules and for
    // silent rules whose body references node-producing rules.
    alloc_flags: []const bool,

    mode: Mode,

    /// A validator that still tracks the furthest failure: recovery's
    /// probes, which learn from it how far an element got before failing
    probe_hwm: bool = false,
    /// Recover mode: the validator copies of the rules, and the flags that
    /// go with them (every rule silent, none allocating), for emitProbe()
    vrule_fns: []LB.Value = &.{},
    probe_silent: []const bool = &.{},
    probe_alloc: []const bool = &.{},
    helper_recover_error: LB.Value = null,
    helper_recover_error_type: LB.Type = null,
    helper_recover_begin: LB.Value = null,
    helper_recover_begin_type: LB.Type = null,
    helper_recover_step: LB.Value = null,
    helper_recover_step_type: LB.Type = null,
    helper_error_node: LB.Value = null,
    helper_error_node_type: LB.Type = null,
};

/// Generate an LLVM module from a parsed grammar.
/// Returns module + context (caller passes to jit_compiler.jitCompile).
pub fn generateModule(allocator: Allocator, grammar: *const gp.Grammar, mode: Mode) CodegenError!CodegenResult {
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

    // void zgram_set_error_trailing(ptr output, ptr input, i64 input_len, i64 pos)
    const helper_set_error_trailing_type = b.fnType(b.void, &.{ b.ptr, b.ptr, b.i64, b.i64 });
    const helper_set_error_trailing = b.addFunction("zgram_set_error_trailing", helper_set_error_trailing_type);

    // void zgram_set_error_at_hwm(ptr output, ptr input, i64 input_len)
    const helper_set_error_at_hwm_type = b.fnType(b.void, &.{ b.ptr, b.ptr, b.i64 });
    const helper_set_error_at_hwm = b.addFunction("zgram_set_error_at_hwm", helper_set_error_at_hwm_type);

    // i32 zgram_ensure_capacity(ptr output, i32 needed)
    const helper_ensure_capacity_type = b.fnType(b.i32, &.{ b.ptr, b.i32 });
    const helper_ensure_capacity = b.addFunction("zgram_ensure_capacity", helper_ensure_capacity_type);

    // void zgram_tag_field(ptr output, i32 from, i32 field)
    const helper_tag_field_type = b.fnType(b.void, &.{ b.ptr, b.i32, b.i32 });
    const helper_tag_field = b.addFunction("zgram_tag_field", helper_tag_field_type);

    // i32 zgram_fold(ptr output, i32 first, i32 rule_id, i32 kind, i32 k, i32 start, i32 end)
    const helper_fold_type = b.fnType(b.i32, &.{ b.ptr, b.i32, b.i32, b.i32, b.i32, b.i32, b.i32 });
    const helper_fold = b.addFunction("zgram_fold", helper_fold_type);

    // None of the helpers unwind
    for ([_]LB.Value{ helper_reserve_node, helper_fill_node, helper_set_error_trailing, helper_set_error_at_hwm, helper_ensure_capacity, helper_tag_field, helper_fold }) |h| {
        b.addFnAttr(h, "nounwind");
    }

    // The JIT targets this machine, so size SIMD scans for its CPU
    const simd_width: u32 = blk: {
        const feats = LB.llvm.LLVMGetHostCPUFeatures();
        defer LB.llvm.LLVMDisposeMessage(feats);
        break :blk if (std.mem.indexOf(u8, std.mem.span(feats), "+avx2") != null) 32 else 16;
    };

    // Compute silent flags. A validator makes no nodes: every rule is silent.
    const silent_flags = try computeSilentFlags(allocator, grammar);
    defer allocator.free(silent_flags);
    if (mode == .validate) @memset(silent_flags, true);
    const alloc_flags = try computeAllocFlags(allocator, grammar, silent_flags);
    defer allocator.free(alloc_flags);

    // Packrat memo helpers (only called from @memo rule wrappers)
    // i64 zgram_memo_lookup(ptr output, i32 rule_id, i64 pos) -> MEMO_MISS (-2), -1, or result
    const helper_memo_lookup_type = b.fnType(b.i64, &.{ b.ptr, b.i32, b.i64 });
    const helper_memo_lookup = b.addFunction("zgram_memo_lookup", helper_memo_lookup_type);
    // void zgram_memo_store(ptr output, i32 rule_id, i64 pos, i64 result, i32 node_start)
    const helper_memo_store_type = b.fnType(b.void, &.{ b.ptr, b.i32, b.i64, b.i64, b.i32 });
    const helper_memo_store = b.addFunction("zgram_memo_store", helper_memo_store_type);
    b.addFnAttr(helper_memo_lookup, "nounwind");
    b.addFnAttr(helper_memo_store, "nounwind");

    // Create rule functions. References call rule_fns[i]; the rule's code
    // goes in body_fns[i], which is the same function unless the rule is
    // @memo, where rule_fns[i] is a caching wrapper around the body.
    const rule_fns = allocator.alloc(LB.Value, grammar.rules.len) catch return CodegenError.OutOfMemory;
    defer allocator.free(rule_fns);
    const body_fns = allocator.alloc(LB.Value, grammar.rules.len) catch return CodegenError.OutOfMemory;
    defer allocator.free(body_fns);

    for (grammar.rules, 0..) |rule, i| {
        const name = std.fmt.allocPrintSentinel(allocator, "rule_{d}", .{i}, 0) catch return CodegenError.OutOfMemory;
        defer allocator.free(name);
        rule_fns[i] = b.addFunction(name, rule_fn_type);
        body_fns[i] = rule_fns[i];
        if (rule.memo) {
            const body_name = std.fmt.allocPrintSentinel(allocator, "rule_{d}_body", .{i}, 0) catch return CodegenError.OutOfMemory;
            defer allocator.free(body_name);
            body_fns[i] = b.addFunction(body_name, rule_fn_type);
        }
        for ([_]LB.Value{ rule_fns[i], body_fns[i] }) |f| {
            b.setLinkageInternal(f);
            b.addFnAttr(f, "nounwind");
            // The input is read-only and never overlaps the output struct
            b.addParamAttr(f, 0, "noalias");
            b.addParamAttr(f, 0, "readonly");
            b.addParamAttr(f, 2, "noalias");
        }
    }

    // A validator is small enough to inline into its callers every rule
    // except one per recursion cycle (the cycle has to be a real call), and
    // those that would grow too big with everything they use inlined.
    // (The cycle breakers are also where the depth of the recursion is checked.)
    const breakers = try computeCycleBreakers(allocator, grammar);
    defer allocator.free(breakers);
    if (mode == .validate) {
        const inlined = try computeInlined(allocator, grammar, breakers);
        defer allocator.free(inlined);
        for (grammar.rules, 0..) |_, i| {
            if (!inlined[i]) continue;
            b.addFnAttr(rule_fns[i], "alwaysinline");
            if (body_fns[i] != rule_fns[i]) b.addFnAttr(body_fns[i], "alwaysinline");
        }
    }

    // Recover mode: validator copies of the rules, for recovery's probes
    // ("could the repetition's element start here?"). No memo wrappers: the
    // memo table is the tree parser's, and a validator's entries have no nodes.
    const vrule_fns = allocator.alloc(LB.Value, if (mode == .recover) grammar.rules.len else 0) catch return CodegenError.OutOfMemory;
    defer allocator.free(vrule_fns);
    const probe_silent = allocator.alloc(bool, vrule_fns.len) catch return CodegenError.OutOfMemory;
    defer allocator.free(probe_silent);
    @memset(probe_silent, true);
    const probe_alloc = allocator.alloc(bool, vrule_fns.len) catch return CodegenError.OutOfMemory;
    defer allocator.free(probe_alloc);
    @memset(probe_alloc, false);
    for (vrule_fns, 0..) |*f, i| {
        const name = std.fmt.allocPrintSentinel(allocator, "vrule_{d}", .{i}, 0) catch return CodegenError.OutOfMemory;
        defer allocator.free(name);
        f.* = b.addFunction(name, rule_fn_type);
        b.setLinkageInternal(f.*);
        b.addFnAttr(f.*, "nounwind");
        b.addParamAttr(f.*, 0, "noalias");
        b.addParamAttr(f.*, 0, "readonly");
        b.addParamAttr(f.*, 2, "noalias");
    }

    // Recovery helpers (jit_helpers.zig)
    // i64 zgram_recover_error(ptr output, i64 start, i64 reach)
    const helper_recover_error_type = b.fnType(b.i64, &.{ b.ptr, b.i64, b.i64 });
    // void zgram_recover_begin(ptr output, ptr input, i64 start, i64 err)
    const helper_recover_begin_type = b.fnType(b.void, &.{ b.ptr, b.ptr, b.i64, b.i64 });
    // i64 zgram_recover_step(ptr output, ptr input, i64 len)
    const helper_recover_step_type = b.fnType(b.i64, &.{ b.ptr, b.ptr, b.i64 });
    // i32 zgram_error_node(ptr output, i64 start, i64 end, i32 rule)
    const helper_error_node_type = b.fnType(b.i32, &.{ b.ptr, b.i64, b.i64, b.i32 });
    var helper_recover_error: LB.Value = null;
    var helper_recover_begin: LB.Value = null;
    var helper_recover_step: LB.Value = null;
    var helper_error_node: LB.Value = null;
    if (mode == .recover) {
        helper_recover_error = b.addFunction("zgram_recover_error", helper_recover_error_type);
        helper_recover_begin = b.addFunction("zgram_recover_begin", helper_recover_begin_type);
        helper_recover_step = b.addFunction("zgram_recover_step", helper_recover_step_type);
        helper_error_node = b.addFunction("zgram_error_node", helper_error_node_type);
        for ([_]LB.Value{ helper_recover_error, helper_recover_begin, helper_recover_step, helper_error_node }) |h| b.addFnAttr(h, "nounwind");
    }

    // Generate each rule function body (and in recover mode, each validator copy)
    for (0..if (mode == .recover) 2 else 1) |pass| {
    const probe = pass == 1;
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
            .helper_set_error_trailing = helper_set_error_trailing,
            .helper_set_error_at_hwm = helper_set_error_at_hwm,
            .helper_ensure_capacity = helper_ensure_capacity,
            .helper_tag_field = helper_tag_field,
            .helper_tag_field_type = helper_tag_field_type,
            .helper_fold = helper_fold,
            .helper_fold_type = helper_fold_type,
            .helper_reserve_node_type = helper_reserve_node_type,
            .helper_fill_node_type = helper_fill_node_type,
            .helper_set_error_trailing_type = helper_set_error_trailing_type,
            .helper_set_error_at_hwm_type = helper_set_error_at_hwm_type,
            .helper_ensure_capacity_type = helper_ensure_capacity_type,
            .grammar = grammar,
            .silent_flags = if (probe) probe_silent else silent_flags,
            .child_count_ptr = undefined,
            .simd_width = simd_width,
            .alloc_flags = if (probe) probe_alloc else alloc_flags,
            .mode = if (probe) .validate else mode,
            .vrule_fns = vrule_fns,
            .probe_silent = probe_silent,
            .probe_alloc = probe_alloc,
            .helper_recover_error = helper_recover_error,
            .helper_recover_error_type = helper_recover_error_type,
            .helper_recover_begin = helper_recover_begin,
            .helper_recover_begin_type = helper_recover_begin_type,
            .helper_recover_step = helper_recover_step,
            .helper_recover_step_type = helper_recover_step_type,
            .helper_error_node = helper_error_node,
            .helper_error_node_type = helper_error_node_type,
        };
        if (probe) {
            cg.rule_fns = vrule_fns;
            cg.probe_hwm = true;
        }
        try emitRuleFunction(&cg, rule, @intCast(i), if (probe) vrule_fns[i] else body_fns[i], breakers[i]);
    }
    }

    // Wrappers for @memo rules
    for (grammar.rules, 0..) |rule, i| {
        if (!rule.memo) continue;
        emitMemoWrapper(&b, rule_fns[i], body_fns[i], rule_fn_type, @intCast(i), helper_memo_lookup, helper_memo_lookup_type, helper_memo_store, helper_memo_store_type);
    }

    // Generate zgram_parse entry point
    try emitParseEntryPoint(&b, rule_fns, rule_fn_type, silent_flags, helper_set_error_trailing, helper_set_error_trailing_type, helper_set_error_at_hwm, helper_set_error_at_hwm_type);

    // The module and context go to the caller; only the IR builder is ours
    b.deinitBuilderOnly();
    return .{ .module = b.module, .context = b.ctx };
}

/// Emit the body of a rule function.
fn emitRuleFunction(cg: *Codegen, rule: *const gp.Rule, rule_id: u16, func: LB.Value, check_depth: bool) CodegenError!void {
    const b = cg.b;
    b.setCurrentFn(func);

    const entry = b.appendBlock("entry");
    b.positionAtEnd(entry);

    // Get function args
    cg.input_ptr = b.param(func, 0);
    cg.input_len = b.param(func, 1);
    cg.output_ptr = b.param(func, 2);
    const pos_arg = b.param(func, 3);

    // A rule on a recursion cycle: every level of nesting in the input
    // passes through here, so this is where the stack can run out. Below
    // the caller's limit the parse is over: say why, and make every rule
    // entered from now on fail as well (any frame is below the top address).
    if (check_depth) {
        const deep_block = try b.newBlock("too_deep");
        const body_block = try b.newBlock("body");
        const frame = b.callIntrinsic(b.lookupIntrinsic("llvm.frameaddress"), &.{b.ptr}, &.{b.constInt(b.i32, 0)}, "frame");
        const frame_addr = LB.llvm.LLVMBuildPtrToInt(b.b, frame, b.i64, "frame_addr");
        const limit_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_STACK_LIMIT)}, "stack_limit_ptr");
        const limit = b.load(b.i64, limit_ptr, 8, "stack_limit");
        _ = b.condBr(b.icmp(.ult, frame_addr, limit, "is_too_deep"), deep_block, body_block);

        b.positionAtEnd(deep_block);
        const kind_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_ERROR_KIND)}, "error_kind_ptr");
        _ = b.store(b.constInt(b.i8, @intFromEnum(abi.ErrorKind.too_deep)), kind_ptr, 1);
        _ = b.store(b.constSInt(b.i64, -1), limit_ptr, 8);
        _ = b.ret(b.constSInt(b.i64, -1));

        b.positionAtEnd(body_block);
    }

    const is_silent = cg.silent_flags[rule_id];

    // HWM field offsets in ParseOutput
    const hwm_max_pos_offset = abi.OFF_MAX_POS;
    const hwm_rule_id_offset = abi.OFF_MAX_POS_RULE_ID;

    if (cg.mode == .validate) {
        // Validator: return the end position or -1, nothing else
        cg.child_count_ptr = null;
        const fail_block = try b.newBlock("fail");
        const result_pos = try emitExpr(cg, rule.expr, pos_arg, fail_block);
        _ = b.ret(result_pos);
        b.positionAtEnd(fail_block);
        // A recovery probe notes how far it got (see emitRecovery)
        if (cg.probe_hwm) try emitHwmUpdate(cg, pos_arg, rule_id);
        _ = b.ret(b.constSInt(b.i64, -1));
    } else if (rule.fold != .none) {
        // Folded rule: reserves no node. Its nodes pile up from `first`, the
        // trailing repetition marks where each iteration starts, and
        // zgram_fold nests them on success. Exactly one node results, so
        // callers treat it like any node-producing rule.
        const fail_block = try b.newBlock("fail");
        const nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "nc_ptr");
        const first = b.load(b.i32, nc_ptr, 4, "fold_first");

        // Top-level nodes produced so far (the children of a wrapper over them)
        cg.child_count_ptr = LB.llvm.LLVMBuildAlloca(b.b, b.i32, "fold_cc");
        _ = b.store(b.constInt(b.i32, 0), cg.child_count_ptr, 4);
        const seq = rule.expr.children orelse return CodegenError.InvalidGrammar;
        cg.fold_rep = seq[seq.len - 1];
        cg.fold_count_ptr = LB.llvm.LLVMBuildAlloca(b.b, b.i32, "fold_count");
        _ = b.store(b.constInt(b.i32, 0), cg.fold_count_ptr, 4);
        cg.fold_iter_ptr = LB.llvm.LLVMBuildAlloca(b.b, b.i32, "fold_last_iter");
        _ = b.store(b.constInt(b.i32, 0), cg.fold_iter_ptr, 4);
        const result_pos = try emitExpr(cg, rule.expr, pos_arg, fail_block);
        cg.fold_rep = null;

        const reps = b.load(b.i32, cg.fold_count_ptr, 4, "fold_reps");
        const children = b.load(b.i32, cg.child_count_ptr, 4, "fold_children");
        const nc = b.load(b.i32, nc_ptr, 4, "fold_nc");
        const nodes_pp = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODES_PTR)}, "fold_nodes_pp");
        const first64 = b.zext(first, b.i64, "fold_first64");

        const no_reps_block = try b.newBlock("fold_no_reps");
        const some_reps_block = try b.newBlock("fold_some_reps");
        const single_block = try b.newBlock("fold_single");
        const one_rep_block = try b.newBlock("fold_one_rep");
        const fold_block = try b.newBlock("fold");
        const done_block = try b.newBlock("fold_done");
        const oom_block = try b.newBlock("fold_oom");
        _ = b.condBr(b.icmp(.eq, reps, b.constInt(b.i32, 0), "fold_is_no_reps"), no_reps_block, some_reps_block);

        // No repetition and a single operand node: it stands in for this rule
        b.positionAtEnd(no_reps_block);
        _ = b.condBr(b.icmp(.eq, children, b.constInt(b.i32, 1), "fold_is_single"), single_block, fold_block);

        // ... without its label, which named its place in this rule's node
        b.positionAtEnd(single_block);
        {
            const nodes_base = b.load(b.ptr, nodes_pp, 8, "fold_nodes");
            const meta_off = b.add(b.shl(first64, b.constInt(b.i64, 4), "fold_off"), b.constInt(b.i64, @offsetOf(abi.FlatNode, "meta")), "fold_meta_off");
            const meta_ptr = b.gep(b.i8, nodes_base, &.{meta_off}, "fold_meta_ptr");
            const meta = b.load(b.i32, meta_ptr, 4, "fold_meta");
            _ = b.store(b.@"and"(meta, b.constInt(b.i32, (1 << abi.FIELD_SHIFT) - 1), "fold_unlabelled"), meta_ptr, 4);
            _ = b.br(done_block);
        }

        // One repetition (`a + b`), by far the most common fold, is done
        // inline: one header over everything, which moves up one slot.
        // Left and right folds agree on it; a @postfix one may not add a node.
        b.positionAtEnd(some_reps_block);
        const inline_one = b.icmp(.eq, reps, b.constInt(b.i32, 1), "fold_is_one_rep");
        _ = b.condBr(inline_one, if (rule.fold == .postfix) fold_block else one_rep_block, fold_block);

        b.positionAtEnd(one_rep_block);
        {
            const grow_block = try b.newBlock("fold_grow");
            const shift_block = try b.newBlock("fold_shift");
            const loop_block = try b.newBlock("fold_shift_loop");
            const body_block = try b.newBlock("fold_shift_body");
            const header_block = try b.newBlock("fold_header_fill");

            const cap_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_CAPACITY)}, "fold_cap_ptr");
            const cap = b.load(b.i32, cap_ptr, 4, "fold_cap");
            _ = b.condBr(b.icmp(.ult, nc, cap, "fold_has_room"), shift_block, grow_block);

            b.positionAtEnd(grow_block);
            const grown = b.call(cg.helper_ensure_capacity_type, cg.helper_ensure_capacity, &.{ cg.output_ptr, b.add(nc, b.constInt(b.i32, 1), "fold_needed") }, "fold_grown");
            _ = b.condBr(b.icmp(.eq, grown, b.constInt(b.i32, 0), "fold_grow_failed"), oom_block, shift_block);

            // nodes[first + 1 .. nc + 1] = nodes[first .. nc], from the top down
            b.positionAtEnd(shift_block);
            const nodes_base = b.load(b.ptr, nodes_pp, 8, "fold_nodes1");
            const nc64 = b.zext(nc, b.i64, "fold_nc64");
            _ = b.br(loop_block);

            b.positionAtEnd(loop_block);
            const idx = b.phi(b.i64, "fold_shift_i");
            _ = b.condBr(b.icmp(.ugt, idx, first64, "fold_shift_more"), body_block, header_block);

            b.positionAtEnd(body_block);
            const below = b.sub(idx, b.constInt(b.i64, 1), "fold_shift_below");
            const src = b.gep(b.i8, nodes_base, &.{b.shl(below, b.constInt(b.i64, 4), "fold_src_off")}, "fold_src");
            const lo = b.load(b.i64, src, 4, "fold_lo");
            const hi = b.load(b.i64, b.gep(b.i8, src, &.{b.constInt(b.i64, 8)}, "fold_src_hi"), 4, "fold_hi");
            _ = b.store(lo, b.gep(b.i8, src, &.{b.constInt(b.i64, 16)}, "fold_dst"), 4);
            _ = b.store(hi, b.gep(b.i8, src, &.{b.constInt(b.i64, 24)}, "fold_dst_hi"), 4);
            _ = b.br(loop_block);
            b.addIncoming(idx, &.{ nc64, below }, &.{ shift_block, body_block });

            b.positionAtEnd(header_block);
            // The repetition's first node (now one slot up) loses its mark
            const last_iter = b.zext(b.load(b.i32, cg.fold_iter_ptr, 4, "fold_last_iter_v"), b.i64, "fold_last_iter64");
            const marked_off = b.add(b.shl(last_iter, b.constInt(b.i64, 4), "fold_marked_off"), b.constInt(b.i64, 16 + @offsetOf(abi.FlatNode, "subtree_size")), "fold_marked_size_off");
            const marked_ptr = b.gep(b.i8, nodes_base, &.{marked_off}, "fold_marked_ptr");
            const marked_size = b.load(b.i32, marked_ptr, 4, "fold_marked_size");
            _ = b.store(b.@"and"(marked_size, b.constInt(b.i32, 0x7FFFFFFF), "fold_unmarked"), marked_ptr, 4);

            const header = b.gep(b.i8, nodes_base, &.{b.shl(first64, b.constInt(b.i64, 4), "fold_header_off")}, "fold_header");
            _ = b.store(b.trunc(pos_arg, b.i32, "fold_hs"), header, 4);
            _ = b.store(b.trunc(result_pos, b.i32, "fold_he"), b.gep(b.i8, header, &.{b.constInt(b.i64, 4)}, "fold_h1"), 4);
            _ = b.store(b.sub(nc, first, "fold_subtree"), b.gep(b.i8, header, &.{b.constInt(b.i64, 8)}, "fold_h2"), 4);
            const stored_cc = b.callIntrinsic(b.lookupIntrinsic("llvm.umin"), &.{b.i32}, &.{ children, b.constInt(b.i32, abi.CHILD_COUNT_MANY) }, "fold_cc_sat");
            const header_meta = b.@"or"(b.constInt(b.i32, @as(u32, rule_id) << abi.RULE_SHIFT), stored_cc, "fold_header_meta");
            _ = b.store(header_meta, b.gep(b.i8, header, &.{b.constInt(b.i64, 12)}, "fold_h3"), 4);
            _ = b.store(b.add(nc, b.constInt(b.i32, 1), "fold_nc1"), nc_ptr, 4);
            _ = b.br(done_block);
        }

        b.positionAtEnd(fold_block);
        const ok = b.call(cg.helper_fold_type, cg.helper_fold, &.{
            cg.output_ptr,                     first,
            b.constInt(b.i32, rule_id),        b.constInt(b.i32, @intFromEnum(rule.fold)),
            reps,                              b.trunc(pos_arg, b.i32, "fold_s"),
            b.trunc(result_pos, b.i32, "fold_e"),
        }, "fold_ok");
        _ = b.condBr(b.icmp(.eq, ok, b.constInt(b.i32, 0), "fold_failed"), oom_block, done_block);

        b.positionAtEnd(done_block);
        _ = b.ret(result_pos);

        b.positionAtEnd(oom_block);
        _ = b.ret(b.constSInt(b.i64, -1));

        b.positionAtEnd(fail_block);
        _ = b.store(first, nc_ptr, 4);
        try emitHwmUpdate(cg, pos_arg, rule_id);
        _ = b.ret(b.constSInt(b.i64, -1));
    } else if (is_silent) {
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
        const node_count_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "nc_ptr");
        const node_cap_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_CAPACITY)}, "cap_ptr");
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
        const nodes_ptr_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODES_PTR)}, "nodes_pp");
        const nodes_base = b.load(b.ptr, nodes_ptr_ptr, 8, "nodes_base");
        const node_idx_i64 = b.zext(node_idx, b.i64, "nidx64");
        // GEP by 16 bytes per node (FlatNode size)
        const node_offset = b.shl(node_idx_i64, b.constInt(b.i64, 4), "noff"); // idx * 16
        const node_ptr = b.gep(b.i8, nodes_base, &.{node_offset}, "node_ptr");

        // Truncate pos values to i32 for FlatNode
        const start_i32 = b.trunc(pos_arg, b.i32, "start_i32");
        const end_i32 = b.trunc(result_pos, b.i32, "end_i32");

        // meta = (rule_id << RULE_SHIFT) | child_count; the field id stays 0
        // until a labelled reference in the parent sets it. The count
        // saturates at CHILD_COUNT_MANY; readers then count the children.
        const raw_child_count = b.load(b.i32, cg.child_count_ptr, 4, "cc_raw");
        const child_count = b.callIntrinsic(b.lookupIntrinsic("llvm.umin"), &.{b.i32}, &.{ raw_child_count, b.constInt(b.i32, abi.CHILD_COUNT_MANY) }, "cc");
        const rule_shifted = b.constInt(b.i32, @as(u32, rule_id) << abi.RULE_SHIFT);
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

/// Emit the caching wrapper for a @memo rule:
///   r = memo_lookup(out, id, pos); if r != MISS return r   (a hit also replays the nodes)
///   nc0 = node_count; r = body(...); memo_store(out, id, pos, r, nc0); return r
/// A replay leaves the high-water mark untouched, which matches re-running
/// the rule: the first run already recorded every failure position it reached.
fn emitMemoWrapper(
    b: *LB.Builder,
    wrapper: LB.Value,
    body: LB.Value,
    rule_fn_type: LB.Type,
    rule_id: u32,
    lookup: LB.Value,
    lookup_type: LB.Type,
    store: LB.Value,
    store_type: LB.Type,
) void {
    b.setCurrentFn(wrapper);
    const entry = b.appendBlock("entry");
    const hit = b.appendBlock("memo_hit");
    const miss = b.appendBlock("memo_miss");
    b.positionAtEnd(entry);

    const input_ptr = b.param(wrapper, 0);
    const input_len = b.param(wrapper, 1);
    const output_ptr = b.param(wrapper, 2);
    const pos = b.param(wrapper, 3);
    const id = b.constInt(b.i32, rule_id);

    const cached = b.call(lookup_type, lookup, &.{ output_ptr, id, pos }, "memo");
    const is_miss = b.icmp(.eq, cached, b.constSInt(b.i64, -2), "memo_is_miss");
    _ = b.condBr(is_miss, miss, hit);

    b.positionAtEnd(hit);
    _ = b.ret(cached);

    b.positionAtEnd(miss);
    const nc_ptr = b.gep(b.i8, output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "nc_ptr");
    const nc0 = b.load(b.i32, nc_ptr, 4, "nc0");
    const result = b.call(rule_fn_type, body, &.{ input_ptr, input_len, output_ptr, pos }, "result");
    _ = b.call(store_type, store, &.{ output_ptr, id, pos, result, nc0 }, "");
    _ = b.ret(result);
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

    // Check: pos < input_len
    const in_bounds = b.icmp(.ult, pos, cg.input_len, "inb");
    const check_block = try b.newBlock("cc_check");
    _ = b.condBr(in_bounds, check_block, fail_block);

    b.positionAtEnd(check_block);

    // Load the byte at input[pos]
    const byte_ptr = b.gep(b.i8, cg.input_ptr, &.{pos}, "cc_bptr");
    const ch = b.load(b.i8, byte_ptr, 1, "ch");

    const is_match = try emitClassMatch(cg, expr, ch);

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

    // A labelled reference tags the nodes the call adds, which start here
    const field_id = if (cg.mode != .validate and cg.alloc_flags[rule_idx]) expr.field_id else 0;
    const nc_ptr = if (field_id != 0) b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "lbl_nc_ptr") else undefined;
    const first_node = if (field_id != 0) b.load(b.i32, nc_ptr, 4, "lbl_first") else undefined;

    // Call rule function
    const result = b.call(cg.rule_fn_type, cg.rule_fns[rule_idx], &.{ cg.input_ptr, cg.input_len, cg.output_ptr, pos }, "ref_result");

    // Check if failed (result == -1)
    const failed = b.icmp(.eq, result, b.constSInt(b.i64, -1), "ref_fail");
    const ok_block = try b.newBlock("ref_ok");
    _ = b.condBr(failed, fail_block, ok_block);
    b.positionAtEnd(ok_block);

    if (cg.mode == .validate) return result;

    if (field_id != 0) {
        if (cg.silent_flags[rule_idx]) {
            // Any number of top-level nodes
            _ = b.call(cg.helper_tag_field_type, cg.helper_tag_field, &.{ cg.output_ptr, first_node, b.constInt(b.i32, field_id) }, "");
        } else {
            // Exactly one node, at first_node, whose field id is still 0 (a
            // rule fills its node without one, and @memo caches it before
            // the caller gets here): nodes[first_node].meta |= field << FIELD_SHIFT
            const nodes_pp = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODES_PTR)}, "lbl_nodes_pp");
            const nodes_base = b.load(b.ptr, nodes_pp, 8, "lbl_nodes");
            const off = b.add(b.shl(b.zext(first_node, b.i64, "lbl_idx64"), b.constInt(b.i64, 4), "lbl_off"), b.constInt(b.i64, @offsetOf(abi.FlatNode, "meta")), "lbl_meta_off");
            const meta_ptr = b.gep(b.i8, nodes_base, &.{off}, "lbl_meta_ptr");
            const meta = b.load(b.i32, meta_ptr, 4, "lbl_meta");
            const tagged = b.@"or"(meta, b.constInt(b.i32, @as(u32, field_id) << abi.FIELD_SHIFT), "lbl_tagged");
            _ = b.store(tagged, meta_ptr, 4);
        }
    }

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
    for (children, 0..) |child, i| {
        // Recover mode: a literal after the start of a sequence (a closing
        // `}`, a `;`, an `=`) that's missing exactly at a known error is
        // taken as there, zero-width, so what surrounds it keeps its
        // structure. The first item decides whether the sequence applies at
        // all, so it is never made up.
        if (cg.mode == .recover and i > 0 and child.tag == .literal) {
            cur_pos = try emitInsertableLiteral(cg, child, cur_pos, fail_block);
        } else {
            cur_pos = try emitExpr(cg, child, cur_pos, fail_block);
        }
    }
    return cur_pos;
}

/// A literal that recovery may insert: see emitSequence.
fn emitInsertableLiteral(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const missing = try b.newBlock("ins_missing");
    const inserted = try b.newBlock("ins_inserted");
    const merge = try b.newBlock("ins_merge");
    const end = try emitLiteral(cg, expr, pos, missing);
    const matched = b.getCurrentBlock();
    _ = b.br(merge);

    // Missing: note how far such misses go (the caller learns precise error
    // positions from it); and is a known error right here?
    b.positionAtEnd(missing);
    const lit_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_LIT_POS)}, "ins_lit_ptr");
    const lit_cur = b.load(b.i32, lit_ptr, 4, "ins_lit_cur");
    const pos32 = b.trunc(pos, b.i32, "ins_pos32");
    const further = b.icmp(.ugt, pos32, lit_cur, "ins_further");
    _ = b.store(b.select(further, pos32, lit_cur, "ins_lit_new"), lit_ptr, 4);
    // and which literal it was, for the message
    const text = expr.literal_value orelse "";
    const len_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_LIT_LEN)}, "ins_len_ptr");
    const text_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_LIT_TEXT)}, "ins_text_ptr");
    const len_cur = b.load(b.i32, len_ptr, 4, "ins_len_cur");
    const text_cur = b.load(b.ptr, text_ptr, 8, "ins_text_cur");
    _ = b.store(b.select(further, b.constInt(b.i32, @intCast(text.len)), len_cur, "ins_len_new"), len_ptr, 4);
    _ = b.store(b.select(further, b.addGlobalString("ins_lit", text), text_cur, "ins_text_new"), text_ptr, 8);
    const err = b.call(cg.helper_recover_error_type, cg.helper_recover_error, &.{ cg.output_ptr, pos, pos }, "ins_err");
    _ = b.condBr(b.icmp(.sge, err, b.constSInt(b.i64, 0), "ins_known"), inserted, fail_block);

    b.positionAtEnd(inserted);
    _ = b.br(merge);

    b.positionAtEnd(merge);
    const result = b.phi(b.i64, "ins_pos");
    b.addIncoming(result, &.{ end, pos }, &.{ matched, inserted });
    return result;
}

/// Check if an expression can allocate nodes when evaluated.
/// Returns false for expressions that are guaranteed to never allocate (literals,
/// char classes, any_char, predicates, references to silent rules, and combinations thereof).
fn exprAllocatesNodes(cg: *const Codegen, expr: *const gp.Expr) bool {
    switch (expr.tag) {
        .literal, .char_class, .any_char => return false,
        .not_predicate, .and_predicate => return false, // predicates always restore node_count
        .reference => {
            const ref_name = expr.ref_name orelse return true;
            const idx = ruleIndex(cg, ref_name) orelse return true; // unknown rule, assume it allocates
            return cg.alloc_flags[idx];
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
    var saved_cc: ?LB.Value = null;
    if (needs_save_restore) {
        node_count_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "alt_nc_ptr");
        saved_nc = b.load(b.i32, node_count_ptr, 4, "alt_saved_nc");
        saved_cc = saveChildCount(cg);
    }

    for (children, 0..) |child, i| {
        const is_last = (i + 1 == children.len);
        const child_fail = if (is_last) fail_block else try b.newBlock("alt_next");

        // Restore node_count before trying each alternative (except first)
        if (needs_save_restore and i > 0) {
            restoreState(cg, node_count_ptr, saved_nc, saved_cc);
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

/// Bytes tested one at a time before a run switches to vector steps
const SCALAR_PREFIX = 8;

/// A 256-bit membership bitmap for a character class, as a global constant.
fn classBitmapGlobal(cg: *Codegen, expr: *const gp.Expr) CodegenError!LB.Value {
    const b = cg.b;
    const set = charClassSet(expr);
    var bitmap_consts: [32]LB.Value = undefined;
    for (0..32) |i| {
        var byte: u8 = 0;
        for (0..8) |bit| {
            if (set.isSet(i * 8 + bit)) byte |= @as(u8, 1) << @intCast(bit);
        }
        bitmap_consts[i] = b.constInt(b.i8, byte);
    }
    const name = std.fmt.allocPrintSentinel(b.allocator, "cls_bm_{d}", .{b.block_counter}, 0) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(name);
    b.block_counter += 1;
    return b.addGlobalConstant(name, b.arrayType(b.i8, 32), b.constArray(b.i8, &bitmap_consts));
}

/// i1: is byte `ch` in `expr`'s character class? Uses the cheapest test for
/// the class: one range compare for a single range ([0-9]), equality tests
/// for up to 4 bytes ([ \t\n\r], [^"\\]), a bitmap lookup otherwise.
fn emitClassMatch(cg: *Codegen, expr: *const gp.Expr, ch: LB.Value) CodegenError!LB.Value {
    const b = cg.b;
    const ranges = expr.char_ranges orelse return CodegenError.InvalidGrammar;
    const negated = expr.char_negated;

    var total: u32 = 0;
    for (ranges) |r| total += @as(u32, r.end) - @as(u32, r.start) + 1;

    if (ranges.len == 1 and total > 4) {
        // (ch - lo) <= (hi - lo), unsigned
        const off = b.sub(ch, b.constInt(b.i8, ranges[0].start), "cls_off");
        const in_range = b.icmp(.ule, off, b.constInt(b.i8, ranges[0].end - ranges[0].start), "cls_in");
        return if (negated) b.xor(in_range, b.constInt(b.i1, 1), "cls_not") else in_range;
    }
    if (total <= 4) {
        var any: ?LB.Value = null;
        for (ranges) |r| {
            var cv: u16 = r.start;
            while (cv <= r.end) : (cv += 1) {
                const eq = b.icmp(.eq, ch, b.constInt(b.i8, cv), "cls_eq");
                any = if (any) |a| b.@"or"(a, eq, "cls_or") else eq;
            }
        }
        const m = any orelse b.constInt(b.i1, 0);
        return if (negated) b.xor(m, b.constInt(b.i1, 1), "cls_not") else m;
    }
    return emitClassTest(cg, try classBitmapGlobal(cg, expr), ch);
}

/// i1: is byte `ch` in the class described by `bitmap_global`?
fn emitClassTest(cg: *Codegen, bitmap_global: LB.Value, ch: LB.Value) LB.Value {
    const b = cg.b;
    const idx = b.zext(b.lshr(b.zext(ch, b.i32, "ch32"), b.constInt(b.i32, 3), "bidx"), b.i64, "bidx64");
    const bm_byte = b.load(b.i8, b.gep(b.i8, bitmap_global, &.{idx}, "bm_ptr"), 1, "bm_byte");
    const mask = b.shl(b.constInt(b.i8, 1), b.@"and"(ch, b.constInt(b.i8, 7), "bpos"), "bmask");
    return b.icmp(.ne, b.@"and"(bm_byte, mask, "tst"), b.constInt(b.i8, 0), "cls_match");
}

/// Scan a run of `expr`'s character class from start_pos; returns where it ends.
///
/// Most runs are short (a space, a few digits), where a vector step costs far
/// more than a byte test. So the first SCALAR_PREFIX bytes are tested inline,
/// one at a time, and only a longer run continues in an out-of-line vector
/// function. Keeping the vector loop out of line also keeps rule functions
/// small enough for LLVM to inline.
fn emitSimdCharScan(cg: *Codegen, expr: *const gp.Expr, start_pos: LB.Value, fail_block: LB.Block, rep_kind: u8, hwm_chain: []const u16) CodegenError!LB.Value {
    const b = cg.b;
    const scan_fn = try simdScanFunction(cg, expr);

    const pre_header = try b.newBlock("scan_pre");
    const pre_bounds = try b.newBlock("scan_pre_bounds");
    const pre_body = try b.newBlock("scan_pre_body");
    const vec_call = try b.newBlock("scan_vec");
    const exit = try b.newBlock("scan_exit");

    const entry_block = b.getCurrentBlock();
    _ = b.br(pre_header);

    b.positionAtEnd(pre_header);
    const pre_pos = b.phi(b.i64, "pre_pos");
    const consumed = b.sub(pre_pos, start_pos, "pre_consumed");
    const go_vec = b.icmp(.uge, consumed, b.constInt(b.i64, SCALAR_PREFIX), "go_vec");
    _ = b.condBr(go_vec, vec_call, pre_bounds);

    b.positionAtEnd(pre_bounds);
    const pre_inb = b.icmp(.ult, pre_pos, cg.input_len, "pre_inb");
    _ = b.condBr(pre_inb, pre_body, exit);

    b.positionAtEnd(pre_body);
    const pre_ch = b.load(b.i8, b.gep(b.i8, cg.input_ptr, &.{pre_pos}, "pre_ptr"), 1, "pre_ch");
    const pre_match = try emitClassMatch(cg, expr, pre_ch);
    const pre_next = b.add(pre_pos, b.constInt(b.i64, 1), "pre_next");
    _ = b.condBr(pre_match, pre_header, exit);
    b.addIncoming(pre_pos, &.{ start_pos, pre_next }, &.{ entry_block, pre_body });

    b.positionAtEnd(vec_call);
    const scan_type = b.fnType(b.i64, &.{ b.ptr, b.i64, b.i64 });
    const vec_end = b.call(scan_type, scan_fn, &.{ cg.input_ptr, cg.input_len, pre_pos }, "vec_end");
    _ = b.br(exit);

    b.positionAtEnd(exit);
    const final_pos = b.phi(b.i64, "scan_end");
    b.addIncoming(final_pos, &.{ vec_end, pre_pos, pre_pos }, &.{ vec_call, pre_bounds, pre_body });

    // The (inlined) silent rules around the class failed at final_pos
    try emitHwmChain(cg, final_pos, hwm_chain);

    // For '+' repetition, we need at least one match
    if (rep_kind == '+') {
        const no_match = b.icmp(.eq, final_pos, start_pos, "no_match");
        const ok_block = try b.newBlock("scan_ok");
        _ = b.condBr(no_match, fail_block, ok_block);
        b.positionAtEnd(ok_block);
    }
    return final_pos;
}

/// Out-of-line vector scan for one character class:
///   i64 scan(ptr input, i64 len, i64 pos) -> end of the run starting at pos
/// Processes 16 bytes (SSE2) or 32 bytes (AVX2) per iteration:
///   vec_check: can we load W bytes? if yes → vec_body, else → scalar_loop
///   vec_body:  load <W x i8>, vector compare, all match? → vec_check, else → find_end
///   find_end:  bitcast mismatch mask to iW, cttz to find first mismatch offset
///   scalar_loop: one byte at a time for the tail (< W bytes)
fn simdScanFunction(cg: *Codegen, expr: *const gp.Expr) CodegenError!LB.Value {
    const b = cg.b;
    const cls = classifySimd(expr);

    // Build the function, then restore the builder to where we were
    const saved_fn = b.current_fn;
    const saved_block = b.getCurrentBlock();
    const saved_input_ptr = cg.input_ptr;
    const saved_input_len = cg.input_len;
    defer {
        b.setCurrentFn(saved_fn);
        b.positionAtEnd(saved_block);
        cg.input_ptr = saved_input_ptr;
        cg.input_len = saved_input_len;
    }

    const name = std.fmt.allocPrintSentinel(b.allocator, "simd_scan_{d}", .{b.block_counter}, 0) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(name);
    b.block_counter += 1;
    const func = b.addFunction(name, b.fnType(b.i64, &.{ b.ptr, b.i64, b.i64 }));
    b.setLinkageInternal(func);
    b.addFnAttr(func, "nounwind");
    b.addFnAttr(func, "noinline");
    b.addParamAttr(func, 0, "noalias");
    b.addParamAttr(func, 0, "readonly");
    b.setCurrentFn(func);
    b.positionAtEnd(b.appendBlock("entry"));
    cg.input_ptr = b.param(func, 0);
    cg.input_len = b.param(func, 1);
    const start_pos = b.param(func, 2);

    const W = cg.simd_width;
    const vec16i8 = b.vectorType(b.i8, W);
    const vec16i1 = b.vectorType(b.i1, W);
    const mask_int = if (W == 32) b.i32 else b.i16;
    const cttz_id = b.lookupIntrinsic("llvm.cttz");
    const reduce_and_id = b.lookupIntrinsic("llvm.vector.reduce.and");

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
    const can_vec = b.icmp(.uge, remaining, b.constInt(b.i64, W), "can_vec");
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
            const lo_splat = b.splatVector(b.constInt(b.i8, cls.range_lo), W);
            const hi_splat = b.splatVector(b.constInt(b.i8, cls.range_hi), W);
            const ge = b.icmp(.uge, chunk, lo_splat, "vec_ge");
            const le = b.icmp(.ule, chunk, hi_splat, "vec_le");
            break :blk b.@"and"(ge, le, "vec_mask");
        },
        .small_included_set => blk: {
            // OR-chain of equality comparisons
            var mask: LB.Value = undefined;
            for (0..cls.char_count) |i| {
                const splat = b.splatVector(b.constInt(b.i8, cls.chars[i]), W);
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
                const splat = b.splatVector(b.constInt(b.i8, cls.chars[i]), W);
                const eq = b.icmp(.eq, chunk, splat, "vec_eq");
                if (i == 0) {
                    excluded = eq;
                } else {
                    excluded = b.@"or"(excluded, eq, "vec_or");
                }
            }
            // Negate: match = NOT excluded
            const all_true = b.splatVector(b.constInt(b.i1, 1), W);
            break :blk b.xor(excluded, all_true, "vec_mask");
        },
        .unsupported => unreachable,
    };

    // Check if ALL 16 bytes matched
    const all_match = b.callIntrinsic(reduce_and_id, &.{vec16i1}, &.{match_mask}, "all_match");
    const next_pos = b.add(pos_phi, b.constInt(b.i64, W), "next_pos");
    const vec_body_end = b.getCurrentBlock();
    _ = b.condBr(all_match, vec_check, find_end);

    // Add phi incoming for vec_check
    b.addIncoming(pos_phi, &.{ start_pos, next_pos }, &.{ entry_block, vec_body_end });

    // ── find_end: find first non-matching byte in the 16-byte chunk ──
    b.positionAtEnd(find_end);
    // Invert the mask: we want the first bit that is 0 (non-matching)
    const inv_mask = b.xor(match_mask, b.splatVector(b.constInt(b.i1, 1), W), "inv_mask");
    // Bitcast <W x i1> to iW
    const bitmask = b.bitcast(inv_mask, mask_int, "bitmask");
    // Count trailing zeros to find first mismatch position
    const offset = b.callIntrinsic(cttz_id, &.{mask_int}, &.{ bitmask, b.constInt(b.i1, 0) }, "offset");
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

    const s_match = try emitClassMatch(cg, expr, s_ch);

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

    b.positionAtEnd(exit);
    const final_pos = b.phi(b.i64, "simd_final");
    b.addIncoming(final_pos, &.{ find_end_pos, scalar_final }, &.{ find_end, scalar_exit });
    _ = b.ret(final_pos);
    return func;
}
fn ruleIndex(cg: *const Codegen, name: []const u8) ?usize {
    for (cg.grammar.rules, 0..) |rule, i| {
        if (std.mem.eql(u8, rule.name, name)) return i;
    }
    return null;
}

/// Follow references to @silent rules down to the expression they match.
/// `chain` receives the ids of the silent rules passed through, outermost first.
/// Inlining a silent rule is equivalent except for its high-water-mark update
/// on failure, which callers re-emit with emitHwmChain.
fn resolveSilent(cg: *const Codegen, expr: *const gp.Expr, chain: *[8]u16, chain_len: *usize) *const gp.Expr {
    var e = expr;
    while (e.tag == .reference and chain_len.* < chain.len) {
        const idx = ruleIndex(cg, e.ref_name orelse return e) orelse return e;
        if (!cg.silent_flags[idx]) return e;
        chain[chain_len.*] = @intCast(idx);
        chain_len.* += 1;
        e = cg.grammar.rules[idx].expr;
    }
    return e;
}

/// Record a failure of `rule_id` at `pos` in the high-water mark, exactly as
/// the rule's own fail block would.
fn emitHwmUpdate(cg: *Codegen, pos: LB.Value, rule_id: u16) CodegenError!void {
    if (cg.mode == .validate and !cg.probe_hwm) return;
    const b = cg.b;
    const hwm_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_MAX_POS)}, "hwm_ptr");
    const cur = b.load(b.i32, hwm_ptr, 4, "cur_hwm");
    const pos_i32 = b.trunc(pos, b.i32, "pos_i32");
    const further = b.icmp(.ugt, pos_i32, cur, "is_further");
    const update = try b.newBlock("hwm_update");
    const done = try b.newBlock("hwm_done");
    _ = b.condBr(further, update, done);
    b.positionAtEnd(update);
    _ = b.store(pos_i32, hwm_ptr, 4);
    const rid_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_MAX_POS_RULE_ID)}, "hwm_rid_ptr");
    _ = b.store(b.constInt(b.i16, rule_id), rid_ptr, 2);
    _ = b.br(done);
    b.positionAtEnd(done);
}

/// Failure updates for a chain of inlined silent rules: innermost fails first.
fn emitHwmChain(cg: *Codegen, pos: LB.Value, chain: []const u16) CodegenError!void {
    var i = chain.len;
    while (i > 0) {
        i -= 1;
        try emitHwmUpdate(cg, pos, chain[i]);
    }
}

const ByteSet = std.StaticBitSet(256);

fn charClassSet(expr: *const gp.Expr) ByteSet {
    var set = ByteSet.initEmpty();
    if (expr.char_ranges) |ranges| {
        for (ranges) |r| {
            var cv: u16 = r.start;
            while (cv <= r.end) : (cv += 1) set.set(cv);
        }
    }
    if (expr.char_negated) set.toggleAll();
    return set;
}

/// Bytes an expression can start with, or null if unknown or if it can match
/// the empty string (so a non-null result also means "consumes at least 1 byte").
fn firstSet(cg: *const Codegen, expr: *const gp.Expr, depth: u8) ?ByteSet {
    if (depth > 16) return null;
    switch (expr.tag) {
        .literal => {
            const lit = expr.literal_value orelse return null;
            if (lit.len == 0) return null;
            var set = ByteSet.initEmpty();
            set.set(lit[0]);
            return set;
        },
        .char_class => return charClassSet(expr),
        .any_char => return ByteSet.initFull(),
        .reference => {
            const idx = ruleIndex(cg, expr.ref_name orelse return null) orelse return null;
            return firstSet(cg, cg.grammar.rules[idx].expr, depth + 1);
        },
        .sequence => {
            const children = expr.children orelse return null;
            if (children.len == 0) return null;
            return firstSet(cg, children[0], depth + 1);
        },
        .alternative => {
            const children = expr.children orelse return null;
            if (children.len == 0) return null;
            var set = ByteSet.initEmpty();
            for (children) |child| set.setUnion(firstSet(cg, child, depth + 1) orelse return null);
            return set;
        },
        .repetition => {
            if (expr.rep_kind != '+') return null;
            return firstSet(cg, expr.rep_expr orelse return null, depth + 1);
        },
        .not_predicate, .and_predicate => return null,
    }
}

/// `(a | cls | b)*` where exactly one branch is a SIMD-able character class
/// and no other branch can start with a byte in it: at any position inside a
/// run of cls bytes every other branch fails on the first byte, so the run
/// can be vector-scanned and the other branches tried only where it stops.
/// Returns null if the pattern doesn't apply.
fn emitSimdAltLoop(cg: *Codegen, alt: *const gp.Expr, pos: LB.Value, outer_chain: []const u16) CodegenError!?LB.Value {
    const branches = alt.children orelse return null;

    var cls_idx: ?usize = null;
    var cls_expr: *const gp.Expr = undefined;
    var cls_chain: [8]u16 = undefined;
    var cls_chain_len: usize = 0;
    for (branches, 0..) |br, i| {
        var ch: [8]u16 = undefined;
        var n: usize = 0;
        const r = resolveSilent(cg, br, &ch, &n);
        if (canSimdScan(r)) {
            if (cls_idx != null) return null;
            cls_idx = i;
            cls_expr = r;
            cls_chain = ch;
            cls_chain_len = n;
        }
    }
    const ci = cls_idx orelse return null;
    const cls_set = charClassSet(cls_expr);
    var needs_nc_save = false;
    for (branches, 0..) |br, i| {
        if (i == ci) continue;
        const fs = firstSet(cg, br, 0) orelse return null;
        if (fs.intersectWith(cls_set).count() != 0) return null;
        if (exprAllocatesNodes(cg, br)) needs_nc_save = true;
    }

    const b = cg.b;
    const header = try b.newBlock("salt_header");
    const exit = try b.newBlock("salt_exit");

    const entry_block = b.getCurrentBlock();
    _ = b.br(header);

    b.positionAtEnd(header);
    const pos_phi = b.phi(b.i64, "salt_pos");

    // Scan the run of cls bytes ('*': never fails)
    const run_end = try emitSimdCharScan(cg, cls_expr, pos_phi, exit, '*', &.{});

    var nc_ptr: LB.Value = undefined;
    var saved_nc: LB.Value = undefined;
    var saved_cc: ?LB.Value = null;
    if (needs_nc_save) {
        nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "salt_nc_ptr");
        saved_nc = b.load(b.i32, nc_ptr, 4, "salt_saved_nc");
        saved_cc = saveChildCount(cg);
    }

    const loop_vals = b.allocator.alloc(LB.Value, branches.len + 1) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(loop_vals);
    const loop_blocks = b.allocator.alloc(LB.Block, branches.len + 1) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(loop_blocks);
    loop_vals[0] = pos;
    loop_blocks[0] = entry_block;
    var n_loop: usize = 1;

    // Try the other branches in their original order at the end of the run
    for (branches, 0..) |br, i| {
        if (i == ci) {
            // The class branch fails here: its inlined rules' failure updates
            try emitHwmChain(cg, run_end, cls_chain[0..cls_chain_len]);
            continue;
        }
        const next = try b.newBlock("salt_next");
        if (needs_nc_save) restoreState(cg, nc_ptr, saved_nc, saved_cc);
        const end = try emitExpr(cg, br, run_end, next);
        loop_vals[n_loop] = end;
        loop_blocks[n_loop] = b.getCurrentBlock();
        n_loop += 1;
        _ = b.br(header);
        b.positionAtEnd(next);
    }

    // Every branch failed: the repetition ends at run_end
    if (needs_nc_save) restoreState(cg, nc_ptr, saved_nc, saved_cc);
    try emitHwmChain(cg, run_end, outer_chain);
    _ = b.br(exit);

    b.addIncoming(pos_phi, loop_vals[0..n_loop], loop_blocks[0..n_loop]);

    b.positionAtEnd(exit);
    return run_end;
}

/// Flag the node at `iter_start` (if the iteration produced one) as the start
/// of a repetition, and count it.
fn emitFoldMark(cg: *Codegen, nc_ptr: LB.Value, iter_start: LB.Value) CodegenError!void {
    const b = cg.b;
    const mark = try b.newBlock("fold_mark");
    const after = try b.newBlock("fold_mark_done");
    const nc = b.load(b.i32, nc_ptr, 4, "fold_mark_nc");
    _ = b.condBr(b.icmp(.ugt, nc, iter_start, "fold_has_nodes"), mark, after);

    b.positionAtEnd(mark);
    const nodes_pp = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODES_PTR)}, "fold_mark_pp");
    const nodes_base = b.load(b.ptr, nodes_pp, 8, "fold_mark_nodes");
    const off = b.add(b.shl(b.zext(iter_start, b.i64, "fold_mark_idx"), b.constInt(b.i64, 4), "fold_mark_off"), b.constInt(b.i64, @offsetOf(abi.FlatNode, "subtree_size")), "fold_mark_size_off");
    const size_ptr = b.gep(b.i8, nodes_base, &.{off}, "fold_mark_ptr");
    const size = b.load(b.i32, size_ptr, 4, "fold_mark_size");
    _ = b.store(b.@"or"(size, b.constInt(b.i32, 1 << 31), "fold_marked_size"), size_ptr, 4);
    const reps = b.load(b.i32, cg.fold_count_ptr, 4, "fold_reps_so_far");
    _ = b.store(b.add(reps, b.constInt(b.i32, 1), "fold_reps_next"), cg.fold_count_ptr, 4);
    _ = b.store(iter_start, cg.fold_iter_ptr, 4);
    _ = b.br(after);

    b.positionAtEnd(after);
}

/// The trailing repetition of a @left/@right/@postfix rule: a plain loop that
/// marks the first node of every iteration for zgram_fold. An iteration that
/// fails or matches nothing leaves no nodes.
fn emitFoldRepetition(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const sub = expr.rep_expr orelse return CodegenError.InvalidGrammar;
    const kind = expr.rep_kind;
    const nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "fold_nc_ptr");

    var start_pos = pos;
    if (kind == '+') {
        const first_start = b.load(b.i32, nc_ptr, 4, "fold_iter0");
        start_pos = try emitExpr(cg, sub, pos, fail_block);
        try emitFoldMark(cg, nc_ptr, first_start);
    }
    const entry_block = b.getCurrentBlock();

    const header = try b.newBlock("fold_header");
    const iter_fail = try b.newBlock("fold_iter_fail");
    const marked = try b.newBlock("fold_marked");
    const exit = try b.newBlock("fold_exit");
    _ = b.br(header);

    b.positionAtEnd(header);
    const pos_phi = b.phi(b.i64, "fold_pos");
    const iter_start = b.load(b.i32, nc_ptr, 4, "fold_iter");
    const iter_cc = saveChildCount(cg);
    const iter_pos = try emitExpr(cg, sub, pos_phi, iter_fail);
    const no_progress = b.icmp(.eq, iter_pos, pos_phi, "fold_no_prog");
    _ = b.condBr(no_progress, iter_fail, marked);

    b.positionAtEnd(marked);
    try emitFoldMark(cg, nc_ptr, iter_start);
    const marked_block = b.getCurrentBlock();
    _ = b.br(if (kind == '?') exit else header);

    b.positionAtEnd(iter_fail);
    restoreState(cg, nc_ptr, iter_start, iter_cc);
    _ = b.br(exit);

    if (kind == '?') {
        b.addIncoming(pos_phi, &.{start_pos}, &.{entry_block});
    } else {
        b.addIncoming(pos_phi, &.{ start_pos, iter_pos }, &.{ entry_block, marked_block });
    }

    b.positionAtEnd(exit);
    if (kind != '?') return pos_phi;
    const exit_phi = b.phi(b.i64, "fold_exit_pos");
    b.addIncoming(exit_phi, &.{ pos_phi, iter_pos }, &.{ iter_fail, marked_block });
    return exit_phi;
}

/// Emit repetition: *, +, ?
fn emitRepetition(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    if (cg.fold_rep == expr) return emitFoldRepetition(cg, expr, pos, fail_block);

    const b = cg.b;
    const sub = expr.rep_expr orelse return CodegenError.InvalidGrammar;
    const kind = expr.rep_kind;

    // SIMD fast paths, looking through references to @silent rules:
    //   cls* / cls+             -> vector scan
    //   (a | cls | b)*          -> vector scan of cls runs, a/b tried where a run stops
    if (kind == '*' or kind == '+') {
        var chain: [8]u16 = undefined;
        var chain_len: usize = 0;
        const resolved = resolveSilent(cg, sub, &chain, &chain_len);
        if (canSimdScan(resolved)) {
            return emitSimdCharScan(cg, resolved, pos, fail_block, kind, chain[0..chain_len]);
        }
        // Inlining the silent rule would drop the label on the reference to
        // it; and the vector loop doesn't recover from errors
        const labelled = sub.tag == .reference and sub.field_id != 0;
        if (kind == '*' and resolved.tag == .alternative and !labelled and !recovers(cg, sub)) {
            if (try emitSimdAltLoop(cg, resolved, pos, chain[0..chain_len])) |end| return end;
        }
    }

    const needs_nc_save = exprAllocatesNodes(cg, sub);
    const always_consumes = exprAlwaysConsumes(sub);
    const rec = kind != '?' and recovers(cg, sub);

    if (kind == '?') {
        // Optional: try once, succeed either way
        const try_fail = try b.newBlock("opt_fail");
        const merge = try b.newBlock("opt_merge");

        var nc_ptr: LB.Value = undefined;
        var saved_nc: LB.Value = undefined;
        var saved_cc: ?LB.Value = null;
        if (needs_nc_save) {
            nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "opt_nc_ptr");
            saved_nc = b.load(b.i32, nc_ptr, 4, "opt_saved_nc");
            saved_cc = saveChildCount(cg);
        }

        const result_pos = try emitExpr(cg, sub, pos, try_fail);
        const success_block = b.getCurrentBlock();
        _ = b.br(merge);

        // Fail: restore node_count if needed, use original pos
        b.positionAtEnd(try_fail);
        if (needs_nc_save) restoreState(cg, nc_ptr, saved_nc, saved_cc);
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
        nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "rep_nc_ptr");
    }

    if (kind == '+') {
        // +: try first match, fail entirely if it doesn't match
        const first_result = try emitExpr(cg, sub, pos, fail_block);
        const first_block = b.getCurrentBlock();
        if (always_consumes) {
            _ = b.br(loop_header);
        } else {
            // A zero-length first match ends the loop, as in `*`; looping
            // again would match the same empty string (and its nodes) twice.
            const first_no_progress = b.icmp(.eq, first_result, pos, "first_no_prog");
            _ = b.condBr(first_no_progress, loop_exit, loop_header);
        }

        // Loop header: phi for current position
        b.positionAtEnd(loop_header);
        const pos_phi = b.phi(b.i64, "rep_pos");

        // Branch to body
        _ = b.br(loop_body);

        // Loop body
        b.positionAtEnd(loop_body);
        var saved_nc: LB.Value = undefined;
        var saved_cc: ?LB.Value = null;
        if (needs_nc_save) {
            saved_nc = b.load(b.i32, nc_ptr, 4, "rep_saved");
            saved_cc = saveChildCount(cg);
        }
        const hwm_saved = if (rec) hwmRestart(cg, pos_phi) else undefined;
        const body_result = try emitExpr(cg, sub, pos_phi, loop_fail);
        if (rec) _ = hwmMerge(cg, hwm_saved);
        const body_end_block = b.getCurrentBlock();
        if (always_consumes) {
            // Sub-expr always consumes >=1 byte, no need for zero-length check
            _ = b.br(loop_header);
        } else {
            const no_progress = b.icmp(.eq, body_result, pos_phi, "no_prog");
            _ = b.condBr(no_progress, loop_exit, loop_header);
        }

        // Loop fail: restore node_count if needed, exit loop (or recover)
        b.positionAtEnd(loop_fail);
        if (needs_nc_save) restoreState(cg, nc_ptr, saved_nc, saved_cc);
        const fail_exit = try endIteration(cg, rec, sub, pos_phi, hwm_saved, loop_header, loop_exit);

        // Finish phi: incoming from entry (first result) and from body (next result)
        b.addIncoming(pos_phi, &.{ first_result, body_result }, &.{ first_block, body_end_block });
        if (fail_exit.recovered) |r| b.addIncoming(pos_phi, &.{r[0]}, &.{r[1]});

        // Exit
        b.positionAtEnd(loop_exit);
        if (always_consumes) {
            // Only one incoming edge: the failed iteration
            return pos_phi;
        } else {
            const exit_phi = b.phi(b.i64, "rep_exit_pos");
            b.addIncoming(exit_phi, &.{ pos_phi, pos_phi, first_result }, &.{ fail_exit.block, body_end_block, first_block });
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
        var saved_cc: ?LB.Value = null;
        if (needs_nc_save) {
            saved_nc = b.load(b.i32, nc_ptr, 4, "rep_saved");
            saved_cc = saveChildCount(cg);
        }
        const hwm_saved = if (rec) hwmRestart(cg, pos_phi) else undefined;
        const body_result = try emitExpr(cg, sub, pos_phi, loop_fail);
        if (rec) _ = hwmMerge(cg, hwm_saved);
        const body_end_block = b.getCurrentBlock();
        if (always_consumes) {
            _ = b.br(loop_header);
        } else {
            const no_progress = b.icmp(.eq, body_result, pos_phi, "no_prog");
            _ = b.condBr(no_progress, loop_exit, loop_header);
        }

        // Loop fail: restore nc if needed, exit (or recover)
        b.positionAtEnd(loop_fail);
        if (needs_nc_save) restoreState(cg, nc_ptr, saved_nc, saved_cc);
        const fail_exit = try endIteration(cg, rec, sub, pos_phi, hwm_saved, loop_header, loop_exit);

        // Finish phi
        b.addIncoming(pos_phi, &.{ pos, body_result }, &.{ entry_block, body_end_block });
        if (fail_exit.recovered) |r| b.addIncoming(pos_phi, &.{r[0]}, &.{r[1]});

        // Exit
        b.positionAtEnd(loop_exit);
        if (always_consumes) {
            return pos_phi;
        } else {
            const exit_phi = b.phi(b.i64, "rep_exit_pos");
            b.addIncoming(exit_phi, &.{ pos_phi, pos_phi }, &.{ fail_exit.block, body_end_block });
            return exit_phi;
        }
    }
}

// ── Error recovery (recover mode) ──

const IterationEnd = struct {
    /// The block the loop exit is reached from when an iteration fails
    block: LB.Block,
    /// With recovery: the position to go on at, and the block that goes on
    recovered: ?struct { LB.Value, LB.Block } = null,
};

/// The end of a repetition's failed iteration (the current block, after the
/// node count is restored): exit the loop, or with `rec` try recovering first.
/// Every way out of the loop through here leaves from one block, so the
/// exit's phi keeps a single incoming edge for it.
fn endIteration(cg: *Codegen, rec: bool, sub: *const gp.Expr, start: LB.Value, hwm_saved: HwmSave, loop_header: LB.Block, loop_exit: LB.Block) CodegenError!IterationEnd {
    const b = cg.b;
    if (!rec) {
        const here = b.getCurrentBlock();
        _ = b.br(loop_exit);
        return .{ .block = here };
    }
    const rec_exit = try b.newBlock("rec_exit");
    const recovered = try emitRecovery(cg, sub, start, hwmMerge(cg, hwm_saved), rec_exit);
    _ = b.br(loop_header);
    b.positionAtEnd(rec_exit);
    _ = b.br(loop_exit);
    return .{ .block = rec_exit, .recovered = recovered };
}

/// Does this repetition recover from syntax errors? In recover mode, the
/// repetitions of elements that make nodes do: lists of statements, of
/// items, of arguments. Not whitespace or the characters of a token.
fn recovers(cg: *const Codegen, sub: *const gp.Expr) bool {
    return cg.mode == .recover and exprAllocatesNodes(cg, sub);
}

const HwmSave = struct { pos: LB.Value, rule: LB.Value };

fn hwmPointers(cg: *Codegen) [2]LB.Value {
    const b = cg.b;
    return .{
        b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_MAX_POS)}, "rec_hwm_ptr"),
        b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_MAX_POS_RULE_ID)}, "rec_hwm_rule_ptr"),
    };
}

/// Before an element's attempt: save the furthest-failure mark and restart
/// it at the element's start, to learn how far this attempt gets.
fn hwmRestart(cg: *Codegen, pos: LB.Value) HwmSave {
    const b = cg.b;
    const ptrs = hwmPointers(cg);
    const saved = HwmSave{ .pos = b.load(b.i32, ptrs[0], 4, "rec_saved_hwm"), .rule = b.load(b.i16, ptrs[1], 2, "rec_saved_rule") };
    _ = b.store(b.trunc(pos, b.i32, "rec_start32"), ptrs[0], 4);
    return saved;
}

/// After the attempt: how far it got. The mark becomes the further of that
/// and the saved one again.
fn hwmMerge(cg: *Codegen, saved: HwmSave) LB.Value {
    const b = cg.b;
    const ptrs = hwmPointers(cg);
    const reach = b.load(b.i32, ptrs[0], 4, "rec_reach");
    const rule = b.load(b.i16, ptrs[1], 2, "rec_reach_rule");
    const keep = b.icmp(.ult, reach, saved.pos, "rec_keep_saved");
    _ = b.store(b.select(keep, saved.pos, reach, "rec_hwm"), ptrs[0], 4);
    _ = b.store(b.select(keep, saved.rule, rule, "rec_rule"), ptrs[1], 2);
    return b.zext(reach, b.i64, "rec_reach64");
}

/// After a probe: how far it got. The mark is the saved one again: where
/// recovery looked for a place to resume isn't a failure of the parse (a
/// broken element it resumes at fails again when the loop tries it).
fn hwmRestore(cg: *Codegen, saved: HwmSave) LB.Value {
    const b = cg.b;
    const ptrs = hwmPointers(cg);
    const reach = b.load(b.i32, ptrs[0], 4, "rec_probe_reach");
    _ = b.store(saved.pos, ptrs[0], 4);
    _ = b.store(saved.rule, ptrs[1], 2);
    return b.zext(reach, b.i64, "rec_probe_reach64");
}

/// `expr` as a validator would match it: no nodes, no child counts, the
/// validator copies of the rules. For recovery's "could this start here?".
fn emitProbe(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const saved = cg.*;
    defer cg.* = saved;
    cg.mode = .validate;
    cg.probe_hwm = true;
    cg.rule_fns = cg.vrule_fns;
    cg.silent_flags = cg.probe_silent;
    cg.alloc_flags = cg.probe_alloc;
    cg.child_count_ptr = null;
    cg.fold_rep = null;
    return emitExpr(cg, expr, pos, fail_block);
}

/// The rule a repetition's element is (or begins with), if any.
fn elementRule(cg: *const Codegen, sub: *const gp.Expr) ?usize {
    var e = sub;
    if (e.tag == .sequence) e = (e.children orelse return null)[0];
    if (e.tag != .reference) return null;
    return ruleIndex(cg, e.ref_name orelse return null);
}

/// The @recover(sync) of that rule.
fn syncOf(cg: *const Codegen, sub: *const gp.Expr) ?*const gp.Expr {
    return cg.grammar.rules[elementRule(cg, sub) orelse return null].recover;
}

/// In a repetition's failure block, after its node count is restored: the
/// element's attempt at `start` got as far as `reach`. If a known error lies
/// in between, skip the broken text into an error node and go on with the
/// loop; otherwise leave the loop as usual. Returns the position and block to
/// add to the loop header's phi.
///
/// Where the loop goes on: from the error, at the first place outside the
/// brackets the broken text opened where the element matches, or starts
/// matching and gets somewhere before failing (another broken element: the
/// next round of recovery, knowing that error too, skips it on its own
/// rather than swallowing it here). With a @recover(sync) on the element's
/// rule, the broken element ends after the first match of `sync` instead,
/// and the look for the element goes on from there. It stops at a closing
/// bracket the broken text didn't open, or at the end of the input.
fn emitRecovery(cg: *Codegen, sub: *const gp.Expr, start: LB.Value, reach: LB.Value, loop_exit: LB.Block) CodegenError!struct { LB.Value, LB.Block } {
    const b = cg.b;
    const err = b.call(cg.helper_recover_error_type, cg.helper_recover_error, &.{ cg.output_ptr, start, reach }, "rec_err");
    const begin_block = try b.newBlock("rec_begin");
    _ = b.condBr(b.icmp(.sge, err, b.constSInt(b.i64, 0), "rec_found"), begin_block, loop_exit);

    b.positionAtEnd(begin_block);
    _ = b.call(cg.helper_recover_begin_type, cg.helper_recover_begin, &.{ cg.output_ptr, cg.input_ptr, start, err }, "");
    const make_block = try b.newBlock("rec_make");
    var resume_vals: [3]LB.Value = undefined;
    var resume_blocks: [3]LB.Block = undefined;
    var n_resume: usize = 0;

    // With a sync point: skip to just after it (or stop)
    const elem_scan = try b.newBlock("rec_scan");
    if (syncOf(cg, sub)) |sync| {
        const sync_scan = try b.newBlock("rec_sync_scan");
        _ = b.br(sync_scan);
        b.positionAtEnd(sync_scan);
        const cand = b.call(cg.helper_recover_step_type, cg.helper_recover_step, &.{ cg.output_ptr, cg.input_ptr, cg.input_len }, "rec_sync_cand");
        const sync_stop = try b.newBlock("rec_sync_stop");
        const sync_probe = try b.newBlock("rec_sync_probe");
        _ = b.condBr(b.icmp(.slt, cand, b.constSInt(b.i64, 0), "rec_sync_is_stop"), sync_stop, sync_probe);

        b.positionAtEnd(sync_stop);
        resume_vals[n_resume] = b.sub(b.constSInt(b.i64, -1), cand, "rec_sync_stop_pos");
        resume_blocks[n_resume] = sync_stop;
        n_resume += 1;
        _ = b.br(make_block);

        b.positionAtEnd(sync_probe);
        const sync_fail = try b.newBlock("rec_sync_fail");
        const sync_saved = hwmRestart(cg, cand);
        const sync_end = try emitProbe(cg, sync, cand, sync_fail);
        _ = hwmRestore(cg, sync_saved);
        // After the sync point, look for the element from there
        _ = b.call(cg.helper_recover_begin_type, cg.helper_recover_begin, &.{ cg.output_ptr, cg.input_ptr, sync_end, sync_end }, "");
        _ = b.br(elem_scan);

        b.positionAtEnd(sync_fail);
        _ = hwmRestore(cg, sync_saved);
        _ = b.br(sync_scan);
    } else _ = b.br(elem_scan);

    // The next candidate place to resume, or where to stop
    b.positionAtEnd(elem_scan);
    const cand = b.call(cg.helper_recover_step_type, cg.helper_recover_step, &.{ cg.output_ptr, cg.input_ptr, cg.input_len }, "rec_cand");
    const stop_block = try b.newBlock("rec_stop");
    const probe_block = try b.newBlock("rec_probe");
    _ = b.condBr(b.icmp(.slt, cand, b.constSInt(b.i64, 0), "rec_is_stop"), stop_block, probe_block);

    b.positionAtEnd(stop_block);
    resume_vals[n_resume] = b.sub(b.constSInt(b.i64, -1), cand, "rec_stop_pos");
    resume_blocks[n_resume] = stop_block;
    n_resume += 1;
    _ = b.br(make_block);

    // Does the element match here, or start to? (how far a failed attempt
    // got is the furthest-failure mark, restarted for it)
    b.positionAtEnd(probe_block);
    const probe_fail = try b.newBlock("rec_probe_fail");
    const hwm_saved = hwmRestart(cg, cand);
    _ = try emitProbe(cg, sub, cand, probe_fail);
    _ = hwmRestore(cg, hwm_saved);
    resume_vals[n_resume] = cand;
    resume_blocks[n_resume] = b.getCurrentBlock();
    n_resume += 1;
    _ = b.br(make_block);

    b.positionAtEnd(probe_fail);
    const probe_reach = hwmRestore(cg, hwm_saved);
    const started = b.@"and"(
        b.icmp(.ugt, probe_reach, cand, "rec_started"),
        b.icmp(.ugt, cand, start, "rec_after_start"),
        "rec_resume_at_start",
    );
    // A broken element starting here: resume here too (the loop stops at it,
    // and the next round, knowing its error, skips it)
    const started_block = try b.newBlock("rec_started_here");
    _ = b.condBr(started, started_block, elem_scan);
    b.positionAtEnd(started_block);
    _ = b.br(make_block);
    // (the same position as a match: add it to the phi below)
    const started_val = cand;

    b.positionAtEnd(make_block);
    const resume_pos = b.phi(b.i64, "rec_resume");
    b.addIncoming(resume_pos, resume_vals[0..n_resume], resume_blocks[0..n_resume]);
    b.addIncoming(resume_pos, &.{started_val}, &.{started_block});
    // No progress: the loop ends here, with no error node (skipping nothing
    // would loop forever)
    const node_block = try b.newBlock("rec_node");
    _ = b.condBr(b.icmp(.ugt, resume_pos, start, "rec_progressed"), node_block, loop_exit);

    b.positionAtEnd(node_block);
    const rule_id: u32 = if (elementRule(cg, sub)) |r| @intCast(r) else abi.NO_RULE;
    const made = b.call(cg.helper_error_node_type, cg.helper_error_node, &.{ cg.output_ptr, start, resume_pos, b.constInt(b.i32, rule_id) }, "rec_node");
    const count_block = try b.newBlock("rec_count");
    // No memory for the node: the loop ends (the parse reports it)
    _ = b.condBr(b.icmp(.ne, made, b.constInt(b.i32, 0), "rec_node_ok"), count_block, loop_exit);

    // The error node is one more child of the rule the repetition is in
    b.positionAtEnd(count_block);
    if (cg.child_count_ptr != null) {
        const cc = b.load(b.i32, cg.child_count_ptr, 4, "rec_cc");
        _ = b.store(b.add(cc, b.constInt(b.i32, 1), "rec_cc1"), cg.child_count_ptr, 4);
    }
    return .{ resume_pos, count_block };
}

/// Emit not predicate: !expr — succeeds if expr fails, consumes nothing.
fn emitNotPredicate(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const sub = expr.pred_expr orelse return CodegenError.InvalidGrammar;
    const needs_save = exprAllocatesNodes(cg, sub);

    var nc_ptr: LB.Value = undefined;
    var saved_nc: LB.Value = undefined;
    var saved_cc: ?LB.Value = null;
    if (needs_save) {
        nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "not_nc_ptr");
        saved_nc = b.load(b.i32, nc_ptr, 4, "not_saved");
        saved_cc = saveChildCount(cg);
    }

    const pred_fail = try b.newBlock("not_fail");

    // Try the sub-expression
    _ = try emitExpr(cg, sub, pos, pred_fail);

    // Sub-expression matched — NOT predicate fails
    if (needs_save) restoreState(cg, nc_ptr, saved_nc, saved_cc);
    _ = b.br(fail_block);

    // Sub-expression failed — NOT predicate succeeds
    b.positionAtEnd(pred_fail);
    if (needs_save) restoreState(cg, nc_ptr, saved_nc, saved_cc);

    return pos;
}

/// Emit and predicate: &expr — succeeds if expr succeeds, consumes nothing.
fn emitAndPredicate(cg: *Codegen, expr: *const gp.Expr, pos: LB.Value, fail_block: LB.Block) CodegenError!LB.Value {
    const b = cg.b;
    const sub = expr.pred_expr orelse return CodegenError.InvalidGrammar;
    const needs_save = exprAllocatesNodes(cg, sub);

    var nc_ptr: LB.Value = undefined;
    var saved_nc: LB.Value = undefined;
    var saved_cc: ?LB.Value = null;
    if (needs_save) {
        nc_ptr = b.gep(b.i8, cg.output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "and_nc_ptr");
        saved_nc = b.load(b.i32, nc_ptr, 4, "and_saved");
        saved_cc = saveChildCount(cg);
    }

    // Try the sub-expression. If it fails part-way it may have added nodes,
    // so restore on the failure path too (callers rely on predicates never
    // leaving nodes behind).
    const sub_fail = if (needs_save) try b.newBlock("and_fail") else fail_block;
    _ = try emitExpr(cg, sub, pos, sub_fail);

    // Sub-expression matched — restore node_count
    if (needs_save) restoreState(cg, nc_ptr, saved_nc, saved_cc);

    if (needs_save) {
        const ok = try b.newBlock("and_ok");
        _ = b.br(ok);
        b.positionAtEnd(sub_fail);
        restoreState(cg, nc_ptr, saved_nc, saved_cc);
        _ = b.br(fail_block);
        b.positionAtEnd(ok);
    }

    return pos;
}

/// Emit the zgram_parse entry point:
///   i32 @zgram_parse(ptr input, i64 len, ptr output, i32 start_rule, i32 flags)
/// Any rule can be the start rule. Returns 0 (status/error in output), or -1
/// for an out-of-range start_rule.
fn emitParseEntryPoint(
    b: *LB.Builder,
    rule_fns: []const LB.Value,
    rule_fn_type: LB.Type,
    silent_flags: []const bool,
    helper_set_error_trailing: LB.Value,
    helper_set_error_trailing_type: LB.Type,
    helper_set_error_at_hwm: LB.Value,
    helper_set_error_at_hwm_type: LB.Type,
) CodegenError!void {
    const parse_fn_type = b.fnType(b.i32, &.{ b.ptr, b.i64, b.ptr, b.i32, b.i32 });
    const func = b.addFunction("zgram_parse", parse_fn_type);
    b.addFnAttr(func, "nounwind");
    b.setCurrentFn(func);

    const entry = b.appendBlock("entry");
    b.positionAtEnd(entry);

    const input_ptr = b.param(func, 0);
    const input_len = b.param(func, 1);
    const output_ptr = b.param(func, 2);
    const start_rule = b.param(func, 3);
    const flags = b.param(func, 4);

    // Reset output: status, error_kind, node_count, max_pos
    _ = b.store(b.constInt(b.i8, 0), output_ptr, 1);
    const ek_ptr = b.gep(b.i8, output_ptr, &.{b.constInt(b.i64, @offsetOf(abi.ParseOutput, "error_kind"))}, "ek_ptr");
    _ = b.store(b.constInt(b.i8, 0), ek_ptr, 1);
    const nc_ptr = b.gep(b.i8, output_ptr, &.{b.constInt(b.i64, abi.OFF_NODE_COUNT)}, "nc_ptr");
    _ = b.store(b.constInt(b.i32, 0), nc_ptr, 4);
    const hwm_ptr = b.gep(b.i8, output_ptr, &.{b.constInt(b.i64, abi.OFF_MAX_POS)}, "hwm_reset_ptr");
    _ = b.store(b.constInt(b.i32, 0), hwm_ptr, 4);

    // Dispatch to the start rule
    const bad_rule = try b.newBlock("bad_rule");
    const dispatched = try b.newBlock("dispatched");
    const sw = b.@"switch"(start_rule, bad_rule, @intCast(rule_fns.len));

    const raw_vals = b.allocator.alloc(LB.Value, rule_fns.len) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(raw_vals);
    const pos_vals = b.allocator.alloc(LB.Value, rule_fns.len) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(pos_vals);
    const case_blocks = b.allocator.alloc(LB.Block, rule_fns.len) catch return CodegenError.OutOfMemory;
    defer b.allocator.free(case_blocks);

    const zero_pos = b.constInt(b.i64, 0);
    for (rule_fns, 0..) |rule_fn, i| {
        const case_block = try b.newBlock("start_rule");
        b.addCase(sw, b.constInt(b.i32, i), case_block);
        b.positionAtEnd(case_block);
        const result = b.call(rule_fn_type, rule_fn, &.{ input_ptr, input_len, output_ptr, zero_pos }, "result");
        // Only the default start rule may be inlined here: inlining every
        // rule into the dispatch multiplies code size and compile time.
        if (i != 0) b.addCallAttr(result, "noinline");
        // A silent rule packs its child count in the upper 32 bits
        raw_vals[i] = result;
        pos_vals[i] = if (silent_flags[i]) b.@"and"(result, b.constInt(b.i64, 0xFFFFFFFF), "root_pos") else result;
        case_blocks[i] = b.getCurrentBlock();
        _ = b.br(dispatched);
    }

    b.positionAtEnd(bad_rule);
    _ = b.ret(b.constSInt(b.i32, -1));

    b.positionAtEnd(dispatched);
    const result = b.phi(b.i64, "raw_result");
    b.addIncoming(result, raw_vals, case_blocks);
    const result_pos = b.phi(b.i64, "result_pos");
    b.addIncoming(result_pos, pos_vals, case_blocks);

    // Failed: -1 (packed silent results are always non-negative)
    const failed = b.icmp(.slt, result, b.constSInt(b.i64, 0), "failed");
    const check_end = try b.newBlock("check_end");
    const parse_failed = try b.newBlock("parse_failed");
    _ = b.condBr(failed, parse_failed, check_end);

    // Success needs the whole input consumed, unless FLAG_PREFIX
    b.positionAtEnd(check_end);
    const fully_consumed = b.icmp(.eq, result_pos, input_len, "full");
    const prefix_bit = b.@"and"(flags, b.constInt(b.i32, abi.FLAG_PREFIX), "prefix_bit");
    const is_prefix = b.icmp(.ne, prefix_bit, b.constInt(b.i32, 0), "is_prefix");
    const accept = b.@"or"(fully_consumed, is_prefix, "accept");
    const success_block = try b.newBlock("success");
    const partial_block = try b.newBlock("partial");
    _ = b.condBr(accept, success_block, partial_block);

    b.positionAtEnd(success_block);
    const end_ptr = b.gep(b.i8, output_ptr, &.{b.constInt(b.i64, abi.OFF_END_POS)}, "end_ptr");
    _ = b.store(result_pos, end_ptr, 8);
    _ = b.store(b.constInt(b.i8, 1), output_ptr, 1);
    _ = b.ret(b.constInt(b.i32, 0));

    b.positionAtEnd(partial_block);
    _ = b.call(helper_set_error_trailing_type, helper_set_error_trailing, &.{ output_ptr, input_ptr, input_len, result_pos }, "");
    _ = b.ret(b.constInt(b.i32, 0));

    // Parse failed: report the high-water mark
    b.positionAtEnd(parse_failed);
    _ = b.call(helper_set_error_at_hwm_type, helper_set_error_at_hwm, &.{ output_ptr, input_ptr, input_len }, "");
    _ = b.ret(b.constInt(b.i32, 0));
}

/// Save the current rule's direct child count (null when not tracked), to
/// restore together with node_count when backtracking.
fn saveChildCount(cg: *Codegen) ?LB.Value {
    if (cg.child_count_ptr == null) return null;
    return cg.b.load(cg.b.i32, cg.child_count_ptr, 4, "saved_cc");
}

/// Roll back node_count and the direct child count to a saved checkpoint.
fn restoreState(cg: *Codegen, nc_ptr: LB.Value, saved_nc: LB.Value, saved_cc: ?LB.Value) void {
    _ = cg.b.store(saved_nc, nc_ptr, 4);
    if (saved_cc) |cc| _ = cg.b.store(cc, cg.child_count_ptr, 4);
}

/// Which rules can add nodes when called: every non-silent rule, plus silent
/// rules that (transitively) reference one. Computed as a fixed point.
fn computeAllocFlags(allocator: Allocator, grammar: *const gp.Grammar, silent_flags: []const bool) CodegenError![]bool {
    const flags = allocator.alloc(bool, grammar.rules.len) catch return CodegenError.OutOfMemory;
    for (flags, silent_flags) |*f, silent| f.* = !silent;
    var changed = true;
    while (changed) {
        changed = false;
        for (grammar.rules, 0..) |rule, i| {
            if (flags[i]) continue;
            if (exprRefsAllocating(grammar, flags, rule.expr)) {
                flags[i] = true;
                changed = true;
            }
        }
    }
    return flags;
}

fn exprRefsAllocating(grammar: *const gp.Grammar, flags: []const bool, expr: *const gp.Expr) bool {
    switch (expr.tag) {
        .literal, .char_class, .any_char => return false,
        // Predicates restore node_count themselves
        .not_predicate, .and_predicate => return false,
        .reference => {
            const name = expr.ref_name orelse return true;
            for (grammar.rules, 0..) |rule, i| {
                if (std.mem.eql(u8, rule.name, name)) return flags[i];
            }
            return true;
        },
        .sequence, .alternative => {
            for (expr.children orelse return false) |child| {
                if (exprRefsAllocating(grammar, flags, child)) return true;
            }
            return false;
        },
        .repetition => return exprRefsAllocating(grammar, flags, expr.rep_expr orelse return false),
    }
}

/// Rules that must stay real functions so the rest can be inlined: the
/// targets of back edges in a depth-first walk of the reference graph, which
/// together break every cycle.
fn computeCycleBreakers(allocator: Allocator, grammar: *const gp.Grammar) CodegenError![]bool {
    const n = grammar.rules.len;
    const breakers = allocator.alloc(bool, n) catch return CodegenError.OutOfMemory;
    @memset(breakers, false);
    // 0 = unvisited, 1 = on the DFS path, 2 = done
    const state = allocator.alloc(u8, n) catch return CodegenError.OutOfMemory;
    defer allocator.free(state);
    @memset(state, 0);

    const Frame = struct { rule: usize, refs: []usize, next: usize };
    var stack: std.ArrayList(Frame) = .empty;
    defer {
        for (stack.items) |f| allocator.free(f.refs);
        stack.deinit(allocator);
    }

    for (0..n) |root| {
        if (state[root] != 0) continue;
        state[root] = 1;
        stack.append(allocator, .{ .rule = root, .refs = try refsOf(allocator, grammar, root), .next = 0 }) catch return CodegenError.OutOfMemory;
        while (stack.items.len > 0) {
            const top = &stack.items[stack.items.len - 1];
            if (top.next == top.refs.len) {
                state[top.rule] = 2;
                allocator.free(top.refs);
                _ = stack.pop();
                continue;
            }
            const r = top.refs[top.next];
            top.next += 1;
            switch (state[r]) {
                0 => {
                    state[r] = 1;
                    stack.append(allocator, .{ .rule = r, .refs = try refsOf(allocator, grammar, r), .next = 0 }) catch return CodegenError.OutOfMemory;
                },
                1 => breakers[r] = true, // back edge: r closes a cycle
                else => {},
            }
        }
    }
    return breakers;
}

/// How big a validator function may get with the rules it uses inlined into
/// it, in grammar expressions. Inlining every rule is exponential on some
/// grammars: precedence levels that each use the next one twice (`and_exp =
/// cmp_exp ('and' cmp_exp)*`) double per level, Lua's `exp` expanded to
/// 94,000 expressions, and LLVM's optimizer, superlinear in a function's
/// size, took minutes. JSON's and the expression benchmark's largest
/// functions (123 and 235) are inlined completely.
const INLINE_MAX_FUNCTION = 400;

/// Which validator rules to always inline: never the cycle breakers; the
/// others smallest first, each unless it would make a function bigger than
/// INLINE_MAX_FUNCTION.
fn computeInlined(allocator: Allocator, grammar: *const gp.Grammar, breakers: []const bool) CodegenError![]bool {
    const n = grammar.rules.len;
    var sizer = Sizer{
        .grammar = grammar,
        .expand = allocator.alloc(bool, n) catch return CodegenError.OutOfMemory,
        .sizes = allocator.alloc(usize, n) catch return CodegenError.OutOfMemory,
    };
    defer allocator.free(sizer.sizes);
    errdefer allocator.free(sizer.expand);
    defer sizer.index.deinit(allocator);
    for (grammar.rules, 0..) |rule, i| sizer.index.put(allocator, rule.name, i) catch return CodegenError.OutOfMemory;

    // Smallest first, by size with everything that can be inlined inlined
    for (sizer.expand, breakers) |*e, b| e.* = !b;
    sizer.reset(std.math.maxInt(usize) / 2);
    const Candidate = struct { rule: usize, size: usize };
    var candidates: std.ArrayList(Candidate) = .empty;
    defer candidates.deinit(allocator);
    for (0..n) |i| {
        if (!breakers[i]) candidates.append(allocator, .{ .rule = i, .size = sizer.ruleSize(i) }) catch return CodegenError.OutOfMemory;
    }
    std.sort.pdq(Candidate, candidates.items, {}, struct {
        fn lt(_: void, a: Candidate, b: Candidate) bool {
            return if (a.size != b.size) a.size < b.size else a.rule < b.rule;
        }
    }.lt);

    @memset(sizer.expand, false);
    for (candidates.items) |c| {
        if (c.size > INLINE_MAX_FUNCTION) break;
        sizer.expand[c.rule] = true;
        sizer.reset(INLINE_MAX_FUNCTION);
        if (sizer.largest() > INLINE_MAX_FUNCTION) sizer.expand[c.rule] = false;
    }
    return sizer.expand;
}

/// Sizes of validator rules, in grammar expressions, with the rules in
/// `expand` inlined where they're referenced (a reference to any other rule
/// is a call: size 1). Saturates just past `cap`.
const Sizer = struct {
    grammar: *const gp.Grammar,
    index: std.StringHashMapUnmanaged(usize) = .empty,
    expand: []bool,
    /// Per rule, 0 = not computed yet (a size is at least 1)
    sizes: []usize,
    cap: usize = 0,

    fn reset(self: *Sizer, cap: usize) void {
        @memset(self.sizes, 0);
        self.cap = cap;
    }

    /// The size of the biggest function: every rule not inlined is one
    fn largest(self: *Sizer) usize {
        var max: usize = 0;
        for (0..self.sizes.len) |i| {
            if (!self.expand[i]) max = @max(max, self.ruleSize(i));
        }
        return max;
    }

    /// (The expanded references form no cycle: only rules that aren't cycle
    /// breakers are ever expanded, and the breakers cut every cycle.)
    fn ruleSize(self: *Sizer, rule: usize) usize {
        if (self.sizes[rule] == 0) self.sizes[rule] = self.exprSize(self.grammar.rules[rule].expr);
        return self.sizes[rule];
    }

    fn exprSize(self: *Sizer, expr: *const gp.Expr) usize {
        var size: usize = 1;
        switch (expr.tag) {
            .reference => if (self.index.get(expr.ref_name orelse "")) |r| {
                if (self.expand[r]) size += self.ruleSize(r);
            },
            .sequence, .alternative => for (expr.children orelse &.{}) |child| {
                size += self.exprSize(child);
                if (size > self.cap) break;
            },
            .repetition => if (expr.rep_expr) |sub| {
                size += self.exprSize(sub);
            },
            .not_predicate, .and_predicate => if (expr.pred_expr) |sub| {
                size += self.exprSize(sub);
            },
            else => {},
        }
        return @min(size, self.cap + 1);
    }
};

fn refsOf(allocator: Allocator, grammar: *const gp.Grammar, rule: usize) CodegenError![]usize {
    var refs: std.ArrayList(usize) = .empty;
    collectRefs(grammar, grammar.rules[rule].expr, &refs, allocator) catch return CodegenError.OutOfMemory;
    return refs.toOwnedSlice(allocator) catch return CodegenError.OutOfMemory;
}

/// Which rules can reach themselves through references (directly or not).
fn computeRecursive(allocator: Allocator, grammar: *const gp.Grammar) CodegenError![]bool {
    const n = grammar.rules.len;
    const result = allocator.alloc(bool, n) catch return CodegenError.OutOfMemory;
    @memset(result, false);
    const seen = allocator.alloc(bool, n) catch return CodegenError.OutOfMemory;
    defer allocator.free(seen);
    var stack: std.ArrayList(usize) = .empty;
    defer stack.deinit(allocator);
    for (0..n) |start| {
        @memset(seen, false);
        stack.clearRetainingCapacity();
        collectRefs(grammar, grammar.rules[start].expr, &stack, allocator) catch return CodegenError.OutOfMemory;
        while (stack.pop()) |r| {
            if (r == start) {
                result[start] = true;
                break;
            }
            if (seen[r]) continue;
            seen[r] = true;
            collectRefs(grammar, grammar.rules[r].expr, &stack, allocator) catch return CodegenError.OutOfMemory;
        }
    }
    return result;
}

fn collectRefs(grammar: *const gp.Grammar, expr: *const gp.Expr, out: *std.ArrayList(usize), allocator: Allocator) !void {
    switch (expr.tag) {
        .literal, .char_class, .any_char => {},
        .reference => {
            const name = expr.ref_name orelse return;
            for (grammar.rules, 0..) |rule, i| {
                if (std.mem.eql(u8, rule.name, name)) return out.append(allocator, i);
            }
        },
        .sequence, .alternative => for (expr.children orelse return) |c| try collectRefs(grammar, c, out, allocator),
        .repetition => try collectRefs(grammar, expr.rep_expr orelse return, out, allocator),
        .not_predicate, .and_predicate => try collectRefs(grammar, expr.pred_expr orelse return, out, allocator),
    }
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
