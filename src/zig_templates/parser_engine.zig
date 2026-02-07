//! Comptime PEG parser engine with lookup-table-accelerated character matching
//! and SIMD bulk scanning.

const std = @import("std");
const types = @import("grammar_types.zig");
const abi = @import("parse_abi.zig");

const Expr = types.Expr;
const ExprTag = types.ExprTag;
const Rule = types.Rule;
const Grammar = types.Grammar;

/// Result of a parse attempt at a given position
const MatchResult = struct {
    matched: bool,
    end_pos: usize,
};

// ============================================================================
// SIMD helpers
// ============================================================================

const VEC_LEN = std.simd.suggestVectorLength(u8) orelse 16;
const VecU8 = @Vector(VEC_LEN, u8);

// ============================================================================
// CharLookup — 256-bit bitmap for O(1) character class membership
// ============================================================================

const CharLookup = struct {
    bits: [256 / 8]u8,

    inline fn contains(self: *const CharLookup, ch: u8) bool {
        return (self.bits[ch >> 3] & (@as(u8, 1) << @as(u3, @truncate(ch)))) != 0;
    }

    fn fromRanges(comptime ranges: []const types.CharRange, comptime negated: bool) CharLookup {
        @setEvalBranchQuota(std.math.maxInt(u32));
        comptime {
            var bits = [_]u8{0} ** 32;
            for (ranges) |r| {
                var c: u16 = r.start;
                while (c <= r.end) : (c += 1) {
                    bits[c >> 3] |= @as(u8, 1) << @as(u3, @truncate(c));
                }
            }
            if (negated) {
                for (&bits) |*b| {
                    b.* = ~b.*;
                }
            }
            return .{ .bits = bits };
        }
    }

    /// Scan input starting at pos, return the position of the first non-matching byte.
    /// Uses true SIMD vector comparisons when the char class can be expressed as
    /// range checks, otherwise falls back to vectorized bitmap gather.
    inline fn scanWhileMatch(self: *const CharLookup, input: []const u8, start: usize) usize {
        var pos = start;

        // SIMD bulk path: check VEC_LEN bytes at a time using vectorized bitmap lookup
        while (pos + VEC_LEN <= input.len) {
            const chunk: VecU8 = input[pos..][0..VEC_LEN].*;
            if (!self.vecAllMatch(chunk)) break;
            pos += VEC_LEN;
        }

        // Scalar tail
        while (pos < input.len and self.contains(input[pos])) {
            pos += 1;
        }
        return pos;
    }

    /// Check if ALL bytes in a chunk are in the character class.
    /// Unrolled bitmap lookups with early exit — faster than vectorized gather
    /// because early exit avoids checking all lanes when a mismatch is found early.
    inline fn vecAllMatch(self: *const CharLookup, vec: VecU8) bool {
        comptime var i = 0;
        inline while (i < VEC_LEN) : (i += 1) {
            if (!self.contains(vec[i])) return false;
        }
        return true;
    }
};

/// Comptime check if a char class is a single contiguous range (e.g. [a-z], [0-9]).
/// If so, we can use pure SIMD vector comparisons instead of bitmap lookup.
fn isSingleRange(comptime ranges: []const types.CharRange, comptime negated: bool) bool {
    return !negated and ranges.len == 1;
}

/// SIMD scan for a single contiguous range [lo-hi]. Uses vector >= and <= comparisons
/// which map directly to SSE/AVX pcmpgtb/pcmpeqb instructions.
inline fn scanSingleRange(input: []const u8, start: usize, comptime lo: u8, comptime hi: u8) usize {
    var pos = start;
    const lo_vec: VecU8 = @splat(lo);
    const hi_vec: VecU8 = @splat(hi);

    while (pos + VEC_LEN <= input.len) {
        const chunk: VecU8 = input[pos..][0..VEC_LEN].*;
        // True SIMD: two vector comparisons + AND + horizontal reduce
        const ge_lo = chunk >= lo_vec;
        const le_hi = chunk <= hi_vec;
        const in_range = @as(@Vector(VEC_LEN, u1), @bitCast(ge_lo)) & @as(@Vector(VEC_LEN, u1), @bitCast(le_hi));
        if (@reduce(.Min, in_range) == 0) break;
        pos += VEC_LEN;
    }

    // Scalar tail
    while (pos < input.len and input[pos] >= lo and input[pos] <= hi) {
        pos += 1;
    }
    return pos;
}

/// SIMD scan for negated single range [^lo-hi] (everything EXCEPT lo..hi).
inline fn scanSingleRangeNeg(input: []const u8, start: usize, comptime lo: u8, comptime hi: u8) usize {
    var pos = start;
    const lo_vec: VecU8 = @splat(lo);
    const hi_vec: VecU8 = @splat(hi);

    while (pos + VEC_LEN <= input.len) {
        const chunk: VecU8 = input[pos..][0..VEC_LEN].*;
        const ge_lo = chunk >= lo_vec;
        const le_hi = chunk <= hi_vec;
        const in_range = @as(@Vector(VEC_LEN, u1), @bitCast(ge_lo)) & @as(@Vector(VEC_LEN, u1), @bitCast(le_hi));
        // Negated: we want all lanes to be OUT of range (i.e. in_range == 0)
        if (@reduce(.Max, in_range) != 0) break;
        pos += VEC_LEN;
    }

    while (pos < input.len and (input[pos] < lo or input[pos] > hi)) {
        pos += 1;
    }
    return pos;
}

// ============================================================================
// ParseState
// ============================================================================

const ParseState = struct {
    input: []const u8,
    output: *abi.ParseOutput,
    depth: usize,
    direct_child_count: u32 = 0,

    const MAX_DEPTH = 256;

    /// Maximum node capacity to prevent unbounded growth (256 MB worth of nodes)
    const MAX_NODE_CAPACITY: u32 = 16 * 1024 * 1024;

    fn ensureCapacity(self: *ParseState, needed: u32) bool {
        if (needed <= self.output.node_capacity) return true;
        if (needed > MAX_NODE_CAPACITY) return false;

        var new_cap = if (self.output.node_capacity == 0)
            abi.INITIAL_NODE_CAPACITY
        else
            self.output.node_capacity;

        while (new_cap < needed) {
            const doubled = @as(u64, new_cap) * 2;
            new_cap = if (doubled > MAX_NODE_CAPACITY)
                MAX_NODE_CAPACITY
            else
                @intCast(doubled);
        }

        if (self.output.nodes_ptr) |old_ptr| {
            if (self.output.node_capacity == 0) {
                // Pointer set but capacity 0 is invalid — treat as fresh allocation
                const new_slice = std.heap.c_allocator.alloc(abi.FlatNode, new_cap) catch return false;
                self.output.nodes_ptr = new_slice.ptr;
                self.output.node_capacity = new_cap;
                return true;
            }
            const old_slice = old_ptr[0..self.output.node_capacity];
            if (std.heap.c_allocator.resize(old_slice, new_cap)) {
                self.output.node_capacity = new_cap;
                return true;
            }
            const new_slice = std.heap.c_allocator.alloc(abi.FlatNode, new_cap) catch return false;
            @memcpy(new_slice[0..self.output.node_count], old_ptr[0..self.output.node_count]);
            std.heap.c_allocator.free(old_slice);
            self.output.nodes_ptr = new_slice.ptr;
            self.output.node_capacity = new_cap;
        } else {
            const new_slice = std.heap.c_allocator.alloc(abi.FlatNode, new_cap) catch return false;
            self.output.nodes_ptr = new_slice.ptr;
            self.output.node_capacity = new_cap;
        }
        return true;
    }

    inline fn reserveNode(self: *ParseState) ?u32 {
        if (self.output.node_count >= MAX_NODE_CAPACITY) return null;
        if (!self.ensureCapacity(self.output.node_count + 1)) return null;
        const idx = self.output.node_count;
        self.output.node_count += 1;
        return idx;
    }

    inline fn fillNode(self: *ParseState, idx: u32, rule_id: u16, start: u32, end: u32, subtree_sz: u32, child_cnt: u16) void {
        const nodes = self.output.nodes_ptr orelse return;
        nodes[idx] = .{
            .text_start = start,
            .text_end = end,
            .subtree_size = subtree_sz,
            .child_count_and_rule = abi.FlatNode.setChildCountAndRule(child_cnt, rule_id),
        };
    }

    fn setError(self: *ParseState, pos: usize, msg: []const u8) void {
        self.output.status = 0;
        self.output.error_offset = @intCast(pos);

        var line: u32 = 1;
        var col: u32 = 1;
        for (self.input[0..@min(pos, self.input.len)]) |c| {
            if (c == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        self.output.error_line = line;
        self.output.error_col = col;

        const mlen = @min(msg.len, 256);
        @memcpy(self.output.error_message[0..mlen], msg[0..mlen]);
        self.output.error_message_len = @intCast(mlen);
    }
};

// ============================================================================
// Parser generator
// ============================================================================

pub fn Parser(comptime grammar: Grammar) type {
    const rule_count = grammar.rules.len;

    // Detect silent rules at comptime using adjacency-based propagation.
    // A rule is silent if it only serves as a character-level helper —
    // i.e. it is never structurally referenced by a non-silent rule.
    //
    // "Structural" references are those in sequences, alternatives, and
    // optional (?) repetitions. References inside */+ repetitions and
    // predicates are NOT structural (they're character-level loops or
    // lookahead that don't produce meaningful tree nodes).
    //
    // Algorithm (no recursion — flat loops only):
    // 1. Build adjacency: for each rule, collect which rule indices it
    //    structurally references (using an explicit expression stack).
    // 2. Propagate non-silence from the start rule (index 0) through
    //    the adjacency edges until stable.
    const MAX_EXPR_STACK = 512;
    const MAX_EDGES_PER_RULE = 64;

    const silent_flags = comptime blk: {
        @setEvalBranchQuota(std.math.maxInt(u32));
        // Step 1: Build adjacency table — edges[i] = rule indices structurally
        // referenced by rule i.
        var edge_counts: [rule_count]usize = [_]usize{0} ** rule_count;
        var edges: [rule_count][MAX_EDGES_PER_RULE]usize = undefined;

        for (grammar.rules, 0..) |rule, ri| {
            // Explicit stack to walk the expression tree without recursion.
            // Each entry is {expr_ptr, in_star_plus} where in_star_plus
            // tracks whether we're inside a */+ repetition.
            var stack: [MAX_EXPR_STACK]struct { expr: *const Expr, blocked: bool } = undefined;
            var sp: usize = 1;
            stack[0] = .{ .expr = &rule.expr, .blocked = false };

            while (sp > 0) {
                sp -= 1;
                const item = stack[sp];
                const expr = item.expr;
                const blocked = item.blocked;

                switch (expr.tag) {
                    .reference => {
                        if (!blocked) {
                            // Record this structural edge (deduplicate)
                            const idx = expr.ref_index;
                            var found = false;
                            for (edges[ri][0..edge_counts[ri]]) |existing| {
                                if (existing == idx) {
                                    found = true;
                                    break;
                                }
                            }
                            if (!found and edge_counts[ri] < MAX_EDGES_PER_RULE) {
                                edges[ri][edge_counts[ri]] = idx;
                                edge_counts[ri] += 1;
                            }
                        }
                    },
                    .sequence, .alternative => {
                        for (expr.children) |*child| {
                            if (sp < MAX_EXPR_STACK) {
                                stack[sp] = .{ .expr = child, .blocked = blocked };
                                sp += 1;
                            }
                        }
                    },
                    .repetition => {
                        if (expr.rep_expr) |sub| {
                            if (sp < MAX_EXPR_STACK) {
                                // ? (optional) is structural; * and + are not
                                const rep_blocked = blocked or (expr.rep_kind != '?');
                                stack[sp] = .{ .expr = sub, .blocked = rep_blocked };
                                sp += 1;
                            }
                        }
                    },
                    .not_predicate, .and_predicate => {
                        // Predicates don't produce output — block their refs
                    },
                    .literal, .char_class, .any_char => {},
                }
            }
        }

        // Step 2: Propagate non-silence from start rule through edges.
        var flags: [rule_count]bool = [_]bool{true} ** rule_count;
        flags[0] = false; // start rule is never silent

        var changed = true;
        var iterations: usize = 0;
        while (changed and iterations < rule_count + 1) {
            changed = false;
            iterations += 1;
            for (0..rule_count) |ri| {
                if (!flags[ri]) {
                    for (edges[ri][0..edge_counts[ri]]) |target| {
                        if (flags[target]) {
                            flags[target] = false;
                            changed = true;
                        }
                    }
                }
            }
        }
        break :blk flags;
    };

    const RuleMatchFn = *const fn (*ParseState, usize) MatchResult;

    return struct {
        const rule_fns: [rule_count]RuleMatchFn = blk: {
            var fns: [rule_count]RuleMatchFn = undefined;
            for (0..rule_count) |i| {
                fns[i] = genRuleMatchFn(i);
            }
            break :blk fns;
        };

        fn genRuleMatchFn(comptime rule_id: usize) RuleMatchFn {
            return struct {
                fn match(state: *ParseState, pos: usize) MatchResult {
                    if (state.depth >= ParseState.MAX_DEPTH) {
                        state.setError(pos, "maximum parse depth exceeded");
                        return .{ .matched = false, .end_pos = pos };
                    }
                    state.depth += 1;
                    defer state.depth -= 1;

                    const rule = &grammar.rules[rule_id];

                    if (comptime silent_flags[rule_id]) {
                        return matchExpr(state, &rule.expr, pos);
                    }

                    const parent_idx = state.reserveNode() orelse {
                        return .{ .matched = false, .end_pos = pos };
                    };

                    // Save parent's direct child counter, reset for our scope
                    const saved_child_count = state.direct_child_count;
                    state.direct_child_count = 0;

                    const result = matchExpr(state, &rule.expr, pos);

                    if (result.matched) {
                        const cc = state.direct_child_count;
                        const subtree = state.output.node_count - parent_idx - 1;
                        state.fillNode(parent_idx, @intCast(rule_id), @intCast(pos), @intCast(result.end_pos), subtree, @intCast(cc));
                        // Restore parent's counter + 1 (we are a direct child of parent)
                        state.direct_child_count = saved_child_count + 1;
                    } else {
                        state.output.node_count = parent_idx;
                        state.direct_child_count = saved_child_count;
                    }

                    return result;
                }
            }.match;
        }

        fn matchExpr(state: *ParseState, comptime expr: *const Expr, pos: usize) MatchResult {
            switch (comptime expr.tag) {
                .literal => {
                    const lit = comptime expr.literal_value;
                    if (pos + lit.len > state.input.len) {
                        return .{ .matched = false, .end_pos = pos };
                    }
                    if (comptime lit.len == 1) {
                        if (state.input[pos] == lit[0]) {
                            return .{ .matched = true, .end_pos = pos + 1 };
                        }
                        return .{ .matched = false, .end_pos = pos };
                    }
                    if (std.mem.eql(u8, state.input[pos .. pos + lit.len], lit)) {
                        return .{ .matched = true, .end_pos = pos + lit.len };
                    }
                    return .{ .matched = false, .end_pos = pos };
                },
                .char_class => {
                    if (pos >= state.input.len) {
                        return .{ .matched = false, .end_pos = pos };
                    }
                    const lookup = comptime CharLookup.fromRanges(expr.char_ranges, expr.char_negated);
                    if (lookup.contains(state.input[pos])) {
                        return .{ .matched = true, .end_pos = pos + 1 };
                    }
                    return .{ .matched = false, .end_pos = pos };
                },
                .reference => {
                    const idx = comptime expr.ref_index;
                    return rule_fns[idx](state, pos);
                },
                .sequence => {
                    var cur_pos = pos;
                    inline for (expr.children) |*child| {
                        const result = matchExpr(state, child, cur_pos);
                        if (!result.matched) {
                            return .{ .matched = false, .end_pos = pos };
                        }
                        cur_pos = result.end_pos;
                    }
                    return .{ .matched = true, .end_pos = cur_pos };
                },
                .alternative => {
                    inline for (expr.children) |*child| {
                        const saved_count = state.output.node_count;
                        const saved_cc = state.direct_child_count;
                        const result = matchExpr(state, child, pos);
                        if (result.matched) {
                            return result;
                        }
                        state.output.node_count = @intCast(saved_count);
                        state.direct_child_count = saved_cc;
                    }
                    return .{ .matched = false, .end_pos = pos };
                },
                .repetition => {
                    const sub = comptime expr.rep_expr orelse return .{ .matched = false, .end_pos = pos };
                    const kind = comptime expr.rep_kind;

                    // SIMD bulk scan for char_class repetitions
                    if (comptime sub.tag == .char_class) {
                        // Choose scan strategy at comptime based on char class structure
                        const scanned = if (comptime sub.char_ranges.len == 1 and !sub.char_negated)
                            // Single range [lo-hi]: pure SIMD vector comparisons
                            scanSingleRange(state.input, pos, comptime sub.char_ranges[0].start, comptime sub.char_ranges[0].end)
                        else if (comptime sub.char_ranges.len == 1 and sub.char_negated)
                            // Negated single range [^lo-hi]: SIMD negated comparisons
                            scanSingleRangeNeg(state.input, pos, comptime sub.char_ranges[0].start, comptime sub.char_ranges[0].end)
                        else
                            // General: bitmap lookup with vectorized gather
                            (comptime CharLookup.fromRanges(sub.char_ranges, sub.char_negated)).scanWhileMatch(state.input, pos);

                        const count = scanned - pos;
                        if (kind == '+' and count == 0) {
                            return .{ .matched = false, .end_pos = pos };
                        }
                        if (kind == '?') {
                            return .{ .matched = true, .end_pos = if (count > 0) pos + 1 else pos };
                        }
                        return .{ .matched = true, .end_pos = scanned };
                    }

                    var cur_pos = pos;
                    var count: usize = 0;

                    while (true) {
                        const saved_count = state.output.node_count;
                        const saved_cc = state.direct_child_count;
                        const result = matchExpr(state, sub, cur_pos);
                        if (!result.matched or result.end_pos == cur_pos) {
                            state.output.node_count = @intCast(saved_count);
                            state.direct_child_count = saved_cc;
                            break;
                        }
                        cur_pos = result.end_pos;
                        count += 1;
                        if (kind == '?') break;
                    }

                    if (kind == '+' and count == 0) {
                        return .{ .matched = false, .end_pos = pos };
                    }
                    return .{ .matched = true, .end_pos = cur_pos };
                },
                .not_predicate => {
                    const sub = comptime expr.pred_expr orelse return .{ .matched = false, .end_pos = pos };
                    const saved_count = state.output.node_count;
                    const saved_cc = state.direct_child_count;
                    const result = matchExpr(state, sub, pos);
                    state.output.node_count = @intCast(saved_count);
                    state.direct_child_count = saved_cc;
                    if (result.matched) {
                        return .{ .matched = false, .end_pos = pos };
                    }
                    return .{ .matched = true, .end_pos = pos };
                },
                .and_predicate => {
                    const sub = comptime expr.pred_expr orelse return .{ .matched = false, .end_pos = pos };
                    const saved_count = state.output.node_count;
                    const saved_cc = state.direct_child_count;
                    const result = matchExpr(state, sub, pos);
                    state.output.node_count = @intCast(saved_count);
                    state.direct_child_count = saved_cc;
                    if (result.matched) {
                        return .{ .matched = true, .end_pos = pos };
                    }
                    return .{ .matched = false, .end_pos = pos };
                },
                .any_char => {
                    if (pos < state.input.len) {
                        return .{ .matched = true, .end_pos = pos + 1 };
                    }
                    return .{ .matched = false, .end_pos = pos };
                },
            }
        }

        /// Comptime-built rule name table — copied once on first parse, not every call.
        const static_rule_names: [abi.MAX_RULES]abi.RuleNameEntry = blk: {
            var names: [abi.MAX_RULES]abi.RuleNameEntry = [_]abi.RuleNameEntry{.{}} ** abi.MAX_RULES;
            for (grammar.rules, 0..) |rule, i| {
                const nlen = @min(rule.name.len, abi.MAX_RULE_NAME);
                for (0..nlen) |j| {
                    names[i].name[j] = rule.name[j];
                }
                names[i].name_len = @intCast(nlen);
            }
            break :blk names;
        };

        pub fn parse(input: []const u8, output: *abi.ParseOutput) i32 {
            output.status = 0;
            output.node_count = 0;
            output.error_offset = 0;
            output.error_line = 0;
            output.error_col = 0;
            output.error_message_len = 0;

            // Copy rule name table only on first call (rule_count==0 means uninitialized)
            if (output.rule_count == 0) {
                output.rule_names = static_rule_names;
                output.rule_count = @intCast(rule_count);
            }

            var state = ParseState{
                .input = input,
                .output = output,
                .depth = 0,
            };

            const result = rule_fns[0](&state, 0);

            if (result.matched and result.end_pos == input.len) {
                output.status = 1;
                return 0;
            } else if (result.matched) {
                state.setError(result.end_pos, "unexpected input after match");
                return 0;
            } else {
                if (output.error_message_len == 0) {
                    state.setError(0, "failed to match start rule");
                }
                return 0;
            }
        }
    };
}
