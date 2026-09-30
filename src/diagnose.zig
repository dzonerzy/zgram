//! Error diagnosis: a plain PEG interpreter that re-runs a failed parse to
//! find the furthest failure and what was expected there: the terminals
//! (literal, character class, `.`) that failed at that position, or the name
//! of a rule when the rule as a whole failed to start there.
//!
//! The JIT-compiled parser only tracks which rule failed furthest, at the
//! position the rule started: enough for "expected expr", but a missing `;`
//! at the end of a statement is reported at the statement's start. Tracking
//! every terminal in the generated code would slow down every parse, so the
//! detail is recovered here instead, only after a parse has failed.
//!
//! It matches exactly like the generated code but builds no nodes. Failures
//! are not recorded inside predicates (`!e`, `&e`), nor inside @silent rules
//! that can match nothing (whitespace and comment skippers): those can't be
//! what the input is missing.
//!
//! When a rule that makes a node fails without getting past its own start,
//! whatever was expected inside it at that position is replaced by the
//! rule's name: `1 + ;` expects "product", not "'-', [0-9], '\"', '('".
//! The outermost such rule wins, so a grammar names its errors by how it
//! names its rules. A rule that got further before failing keeps the detail
//! ("expected ')'").
//!
//! When a token matches (a node-making rule without child nodes: a number,
//! an identifier), what could have made it longer is forgotten: after `1`
//! nobody is missing another digit or a '.'.

const std = @import("std");
const Allocator = std.mem.Allocator;
const gp = @import("grammar_parser.zig");

/// Expression evaluations before giving up (exponential backtracking)
const MAX_STEPS: u64 = 20_000_000;
/// Nested expressions before giving up (native stack)
const MAX_DEPTH: u32 = 2_500;

pub const MAX_EXPECTED = 16;

/// What was expected at the furthest failure: `'lit'`, `[a-z]`,
/// "any character" or a rule name
pub const Expected = struct {
    buf: [48]u8 = undefined,
    len: u8 = 0,
    /// A literal or a rule name: shown in preference to character classes
    is_literal: bool = false,

    pub fn text(self: *const Expected) []const u8 {
        return self.buf[0..self.len];
    }
};

pub const Result = struct {
    /// Byte offset of the furthest terminal failure
    pos: usize = 0,
    expected: [MAX_EXPECTED]Expected = undefined,
    count: usize = 0,

    /// Is the literal `lit` among what was expected?
    pub fn expects(self: *const Result, lit: []const u8) bool {
        var buf: [48]u8 = undefined;
        var w = Writer{ .buf = &buf };
        w.put("'");
        for (lit) |ch| w.putChar(ch, '\'');
        w.put("'");
        for (self.expected[0..self.count]) |*e| {
            if (e.is_literal and std.mem.eql(u8, e.text(), w.buf[0..w.len])) return true;
        }
        return false;
    }

    /// "expected ';'", "expected ',' or ')'", "expected expr, ',' or ';'".
    /// Character classes are left out when a literal or a rule is expected too.
    pub fn message(self: *const Result, buf: []u8) []const u8 {
        var any_literal = false;
        for (self.expected[0..self.count]) |e| any_literal = any_literal or e.is_literal;
        var shown: usize = 0;
        for (self.expected[0..self.count]) |e| shown += @intFromBool(e.is_literal or !any_literal);

        var w = Writer{ .buf = buf };
        w.put("expected ");
        var i: usize = 0;
        for (self.expected[0..self.count]) |*e| {
            if (any_literal and !e.is_literal) continue;
            if (i != 0) w.put(if (i + 1 == shown) " or " else ", ");
            w.put(e.text());
            i += 1;
        }
        return w.buf[0..w.len];
    }
};

/// "expected '}'": the message for one missing literal.
pub fn expectedLiteral(literal: []const u8, buf: []u8) []const u8 {
    var w = Writer{ .buf = buf };
    w.put("expected '");
    for (literal) |ch| w.putChar(ch, '\'');
    w.put("'");
    return w.buf[0..w.len];
}

const Writer = struct {
    buf: []u8,
    len: usize = 0,

    fn put(self: *Writer, s: []const u8) void {
        const n = @min(s.len, self.buf.len - self.len);
        @memcpy(self.buf[self.len..][0..n], s[0..n]);
        self.len += n;
    }

    /// One byte of a literal or class, escaped so the result reads like
    /// grammar source. `special` is the delimiter to escape (`'` or `]`).
    fn putChar(self: *Writer, ch: u8, special: u8) void {
        switch (ch) {
            '\n' => self.put("\\n"),
            '\r' => self.put("\\r"),
            '\t' => self.put("\\t"),
            '\\' => self.put("\\\\"),
            else => {
                if (ch == special) {
                    self.put(&.{ '\\', ch });
                } else if (ch < 0x20 or ch == 0x7F) {
                    const hex = "0123456789abcdef";
                    self.put(&.{ '\\', 'x', hex[ch >> 4], hex[ch & 15] });
                } else {
                    self.put(&.{ch});
                }
            },
        }
    }
};

fn describe(expr: *const gp.Expr) Expected {
    var e = Expected{};
    // Leave room for the closing delimiter
    var w = Writer{ .buf = e.buf[0 .. e.buf.len - 1] };
    switch (expr.tag) {
        .literal => {
            e.is_literal = true;
            w.put("'");
            for (expr.literal_value orelse "") |ch| w.putChar(ch, '\'');
            w.buf = &e.buf;
            w.put("'");
        },
        .char_class => {
            w.put(if (expr.char_negated) "[^" else "[");
            for (expr.char_ranges orelse &.{}) |r| {
                w.putChar(r.start, ']');
                if (r.end != r.start) {
                    w.put("-");
                    w.putChar(r.end, ']');
                }
            }
            w.buf = &e.buf;
            w.put("]");
        },
        else => w.put("any character"),
    }
    e.len = @intCast(w.len);
    return e;
}

const Abort = error{Abort};

const Interp = struct {
    allocator: Allocator,
    grammar: *const gp.Grammar,
    input: []const u8,
    index: std.StringHashMapUnmanaged(usize) = .empty,
    /// Per rule: a @silent rule that can match nothing
    skip: []bool,
    /// Per rule: makes a node that can't have children
    token: []bool,
    /// Results of @memo rules: (rule, pos, recorded?) -> end position or -1
    memo: std.AutoHashMapUnmanaged(u64, i64) = .empty,
    /// > 0 while failures aren't recorded
    quiet: u32 = 0,
    steps: u64 = 0,
    depth: u32 = 0,
    /// Lowest address a frame may be at (see stack.zig)
    stack_limit: usize = 0,
    result: Result = .{},

    fn fail(self: *Interp, expr: *const gp.Expr, pos: usize) void {
        if (self.quiet != 0 or pos < self.result.pos) return;
        if (pos > self.result.pos) self.result = .{ .pos = pos };
        self.expect(describe(expr));
    }

    fn expect(self: *Interp, e: Expected) void {
        for (self.result.expected[0..self.result.count]) |*seen| {
            if (std.mem.eql(u8, seen.text(), e.text())) return;
        }
        if (self.result.count == MAX_EXPECTED) return;
        self.result.expected[self.result.count] = e;
        self.result.count += 1;
    }

    fn rule(self: *Interp, idx: usize, pos: usize) Abort!?usize {
        const r = self.grammar.rules[idx];
        // What was expected at `pos` before this rule was tried
        const kept = if (self.result.pos == pos) self.result.count else 0;
        const before = .{ .pos = self.result.pos, .count = self.result.count };
        const end = try self.ruleBody(idx, pos);
        if (end != null and self.token[idx] and self.quiet == 0 and self.result.pos == end.?) {
            // Expectations at the token's end were added inside it
            self.result.count = if (before.pos == end.?) before.count else 0;
        }
        if (end == null and !r.silent and self.quiet == 0 and self.result.pos <= pos) {
            // The rule failed right where it started: expect the rule
            // itself rather than whatever it tried first
            if (self.result.pos < pos) self.result = .{ .pos = pos } else self.result.count = kept;
            var e = Expected{ .is_literal = true };
            const name = gp.displayName(r);
            e.len = @intCast(@min(name.len, e.buf.len));
            @memcpy(e.buf[0..e.len], name[0..e.len]);
            self.expect(e);
        }
        return end;
    }

    fn ruleBody(self: *Interp, idx: usize, pos: usize) Abort!?usize {
        const r = self.grammar.rules[idx];
        const quiet = self.skip[idx];
        if (quiet) self.quiet += 1;
        defer {
            if (quiet) self.quiet -= 1;
        }
        if (!r.memo) return self.match(r.expr, pos);

        // A result found while failures were being recorded can be reused
        // anywhere; one found while quiet only where nothing is recorded.
        const base = (@as(u64, idx) << 33) | (@as(u64, pos) << 1);
        if (self.memo.get(base)) |end| return if (end < 0) null else @intCast(end);
        if (self.quiet != 0) {
            if (self.memo.get(base | 1)) |end| return if (end < 0) null else @intCast(end);
        }
        const end = try self.match(r.expr, pos);
        self.memo.put(self.allocator, base | @intFromBool(self.quiet != 0), if (end) |e| @intCast(e) else -1) catch return error.Abort;
        return end;
    }

    fn match(self: *Interp, expr: *const gp.Expr, pos: usize) Abort!?usize {
        self.steps += 1;
        if (self.steps > MAX_STEPS or self.depth >= MAX_DEPTH or @frameAddress() < self.stack_limit) return error.Abort;
        self.depth += 1;
        defer self.depth -= 1;

        const s = self.input;
        switch (expr.tag) {
            .literal => {
                const lit = expr.literal_value orelse return error.Abort;
                if (std.mem.startsWith(u8, s[pos..], lit)) return pos + lit.len;
                self.fail(expr, pos);
                return null;
            },
            .char_class => {
                if (pos < s.len) {
                    var in_class = false;
                    for (expr.char_ranges orelse &.{}) |r| {
                        if (s[pos] >= r.start and s[pos] <= r.end) in_class = true;
                    }
                    if (in_class != expr.char_negated) return pos + 1;
                }
                self.fail(expr, pos);
                return null;
            },
            .any_char => {
                if (pos < s.len) return pos + 1;
                self.fail(expr, pos);
                return null;
            },
            .reference => {
                const idx = self.index.get(expr.ref_name orelse return error.Abort) orelse return error.Abort;
                return self.rule(idx, pos);
            },
            .sequence => {
                var p = pos;
                for (expr.children orelse return error.Abort) |child| {
                    p = try self.match(child, p) orelse return null;
                }
                return p;
            },
            .alternative => {
                for (expr.children orelse return error.Abort) |child| {
                    if (try self.match(child, pos)) |end| return end;
                }
                return null;
            },
            .repetition => {
                const sub = expr.rep_expr orelse return error.Abort;
                if (expr.rep_kind == '?') return try self.match(sub, pos) orelse pos;
                var p = pos;
                var first = true;
                while (true) : (first = false) {
                    const end = try self.match(sub, p) orelse {
                        if (first and expr.rep_kind == '+') return null;
                        return p;
                    };
                    if (end == p) return p;
                    p = end;
                }
            },
            .not_predicate, .and_predicate => {
                self.quiet += 1;
                defer self.quiet -= 1;
                const matched = try self.match(expr.pred_expr orelse return error.Abort, pos) != null;
                return if (matched == (expr.tag == .and_predicate)) pos else null;
            },
        }
    }
};

/// Can `expr` match without consuming input? `state` holds the rules'
/// answers: 0 unknown, 1 being computed (taken as "no"), 2 no, 3 yes.
fn nullable(grammar: *const gp.Grammar, index: *const std.StringHashMapUnmanaged(usize), state: []u8, expr: *const gp.Expr) bool {
    switch (expr.tag) {
        .literal, .char_class, .any_char => return false,
        .not_predicate, .and_predicate => return true,
        .reference => {
            const idx = index.get(expr.ref_name orelse return false) orelse return false;
            if (state[idx] == 0) {
                state[idx] = 1;
                state[idx] = if (nullable(grammar, index, state, grammar.rules[idx].expr)) 3 else 2;
            }
            return state[idx] == 3;
        },
        .sequence => {
            for (expr.children orelse return true) |child| {
                if (!nullable(grammar, index, state, child)) return false;
            }
            return true;
        },
        .alternative => {
            for (expr.children orelse return false) |child| {
                if (nullable(grammar, index, state, child)) return true;
            }
            return false;
        },
        .repetition => return expr.rep_kind != '+' or nullable(grammar, index, state, expr.rep_expr orelse return true),
    }
}

/// Re-run a failed parse of `input` from `start_rule` and return the furthest
/// failure. null if the input matches after all, nothing was
/// recorded, or the run was abandoned (too deep, too slow, out of memory).
pub fn diagnose(allocator: Allocator, grammar: *const gp.Grammar, input: []const u8, start_rule: usize) ?Result {
    return run(allocator, grammar, input, start_rule, 0, true);
}

/// What `rule`, matched from `pos` (not the start of the input), expected
/// at the furthest place it failed: for the message of an error that
/// recovery met inside that rule. The rule may still match (a repetition in
/// it stopping at the error); the caller checks the position is the error's.
pub fn diagnoseAt(allocator: Allocator, grammar: *const gp.Grammar, input: []const u8, rule: usize, pos: usize) ?Result {
    return run(allocator, grammar, input, rule, pos, false);
}

fn run(allocator: Allocator, grammar: *const gp.Grammar, input: []const u8, start_rule: usize, pos: usize, whole: bool) ?Result {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const n = grammar.rules.len;
    if (start_rule >= n or pos > input.len) return null;
    var interp = Interp{
        .allocator = alloc,
        .grammar = grammar,
        .input = input,
        .skip = alloc.alloc(bool, n) catch return null,
        .token = alloc.alloc(bool, n) catch return null,
        .stack_limit = @import("stack.zig").limit(@frameAddress()),
    };
    interp.index.ensureTotalCapacity(alloc, @intCast(n)) catch return null;
    for (grammar.rules, 0..) |r, i| interp.index.putAssumeCapacity(r.name, i);

    const state = alloc.alloc(u8, n) catch return null;
    @memset(state, 0);
    for (grammar.rules, 0..) |r, i| {
        const ref = gp.Expr{ .tag = .reference, .ref_name = r.name };
        interp.skip[i] = r.silent and nullable(grammar, &interp.index, state, &ref);
        interp.token[i] = !r.silent and !gp.ruleHasChildren(grammar, r);
    }

    const end = interp.rule(start_rule, pos) catch return null;
    // The whole input must be matched (a rule from the middle is diagnosed
    // either way: see diagnoseAt)
    if (whole and end != null and end.? == input.len) return null;
    if (interp.result.count == 0) return null;
    return interp.result;
}
