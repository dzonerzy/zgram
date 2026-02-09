//! Grammar string parser: converts PEG-like grammar text into a runtime IR.
//!
//! Grammar syntax:
//!     rule_name = expression
//!     expression = sequence ('|' sequence)*      # ordered choice
//!     sequence   = prefix+
//!     prefix     = ('!' | '&')? suffix           # predicates
//!     suffix     = primary ('*' | '+' | '?')?    # repetition
//!     primary    = reference | literal | char_class | '(' expression ')' | '.'
//!     literal    = "'" [^']* "'" | '"' [^"]* '"'
//!     char_class = '[' '^'? (range | char)+ ']'

const std = @import("std");
const Allocator = std.mem.Allocator;

// ============================================================================
// IR types (runtime, heap-allocated)
// ============================================================================

pub const ExprType = enum {
    literal,
    char_class,
    reference,
    sequence,
    alternative,
    repetition,
    not_predicate,
    and_predicate,
    any_char,
};

pub const CharRange = struct {
    start: u8,
    end: u8,
};

pub const Expr = struct {
    tag: ExprType,

    // literal
    literal_value: ?[]const u8 = null,

    // char_class
    char_ranges: ?[]CharRange = null,
    char_negated: bool = false,

    // reference
    ref_name: ?[]const u8 = null,

    // sequence / alternative
    children: ?[]const *Expr = null,

    // repetition
    rep_expr: ?*Expr = null,
    rep_kind: u8 = 0, // '*', '+', '?'

    // not_predicate / and_predicate
    pred_expr: ?*Expr = null,
};

pub const Rule = struct {
    name: []const u8,
    expr: *Expr,
    action: ?[]const u8 = null,
    /// Explicit @silent annotation — forces rule to be silent (no parse tree node).
    silent: bool = false,
};

pub const Grammar = struct {
    rules: []const *Rule,

    pub fn deinit(self: *const Grammar, allocator: Allocator) void {
        for (self.rules) |rule| {
            freeExpr(allocator, rule.expr);
            if (rule.action) |a| allocator.free(a);
            allocator.free(rule.name);
            allocator.destroy(rule);
        }
        allocator.free(self.rules);
    }
};

fn freeExpr(allocator: Allocator, expr: *Expr) void {
    switch (expr.tag) {
        .literal => {
            if (expr.literal_value) |v| allocator.free(v);
        },
        .char_class => {
            if (expr.char_ranges) |r| allocator.free(r);
        },
        .reference => {
            if (expr.ref_name) |n| allocator.free(n);
        },
        .sequence, .alternative => {
            if (expr.children) |children| {
                for (children) |child| {
                    freeExpr(allocator, child);
                }
                allocator.free(children);
            }
        },
        .repetition => {
            if (expr.rep_expr) |sub| freeExpr(allocator, sub);
        },
        .not_predicate, .and_predicate => {
            if (expr.pred_expr) |sub| freeExpr(allocator, sub);
        },
        .any_char => {},
    }
    allocator.destroy(expr);
}

// ============================================================================
// Parser implementation
// ============================================================================

const abi = @import("parse_abi.zig");

const ParseErr = error{
    EmptyGrammar,
    InvalidExpr,
    UndefinedRule,
    ExpectedRuleName,
    ExpectedEquals,
    ExpectedActionName,
    ExpectedExpression,
    ExpectedExprAfterPredicate,
    ExpectedCloseParen,
    UnterminatedString,
    UnterminatedCharClass,
    OutOfMemory,
    DuplicateRule,
    InvalidCharRange,
    NestingTooDeep,
    RuleNameTooLong,
    EmptyLiteral,
    TooManyRules,
    LeftRecursion,
};

const MAX_NESTING_DEPTH = 128;

const GrammarParserImpl = struct {
    text: []const u8,
    pos: usize,
    line: usize,
    col: usize,
    depth: usize,
    allocator: Allocator,

    fn init(allocator: Allocator, text: []const u8) GrammarParserImpl {
        return .{
            .text = text,
            .pos = 0,
            .line = 1,
            .col = 1,
            .depth = 0,
            .allocator = allocator,
        };
    }

    fn parse(self: *GrammarParserImpl) ParseErr!*Grammar {
        var rules_list: std.ArrayList(*Rule) = .empty;
        defer rules_list.deinit(self.allocator);

        self.skipWs();
        while (self.pos < self.text.len) {
            if (try self.parseRule()) |rule| {
                // Check for duplicate rule names
                for (rules_list.items) |existing| {
                    if (std.mem.eql(u8, existing.name, rule.name)) {
                        return error.DuplicateRule;
                    }
                }
                try rules_list.append(self.allocator, rule);
            }
            self.skipWs();
        }

        if (rules_list.items.len == 0) {
            return error.EmptyGrammar;
        }

        if (rules_list.items.len > abi.MAX_RULES) {
            return error.TooManyRules;
        }

        // Validate references
        for (rules_list.items) |rule| {
            try self.validateRefs(rule.expr, rules_list.items);
        }

        // Detect left recursion (rule can reach itself without consuming input)
        try detectLeftRecursion(self.allocator, rules_list.items);

        const grammar = try self.allocator.create(Grammar);
        grammar.* = .{
            .rules = try self.allocator.dupe(*Rule, rules_list.items),
        };
        return grammar;
    }

    fn validateRefs(self: *GrammarParserImpl, expr: *const Expr, rules: []const *Rule) ParseErr!void {
        switch (expr.tag) {
            .reference => {
                const name = expr.ref_name orelse return error.InvalidExpr;
                var found = false;
                for (rules) |rule| {
                    if (std.mem.eql(u8, rule.name, name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) return error.UndefinedRule;
            },
            .sequence, .alternative => {
                if (expr.children) |children| {
                    for (children) |child| {
                        try self.validateRefs(child, rules);
                    }
                }
            },
            .repetition => {
                if (expr.rep_expr) |sub| try self.validateRefs(sub, rules);
            },
            .not_predicate, .and_predicate => {
                if (expr.pred_expr) |sub| try self.validateRefs(sub, rules);
            },
            else => {},
        }
    }

    fn parseRule(self: *GrammarParserImpl) ParseErr!?*Rule {
        self.skipWs();
        if (self.pos >= self.text.len) return null;

        // Skip comment lines
        if (self.peek() == '#') {
            self.skipLine();
            return self.parseRule();
        }

        // Check for @silent annotation
        var is_silent = false;
        if (self.peek() == '@') {
            const saved_pos = self.pos;
            const saved_line = self.line;
            const saved_col = self.col;
            self.advance(); // skip '@'
            if (self.parseIdentifier()) |annotation| {
                if (std.mem.eql(u8, annotation, "silent")) {
                    is_silent = true;
                    self.skipWs();
                } else {
                    // Unknown annotation — restore position
                    self.pos = saved_pos;
                    self.line = saved_line;
                    self.col = saved_col;
                }
            } else {
                self.pos = saved_pos;
                self.line = saved_line;
                self.col = saved_col;
            }
        }

        // Parse rule name
        const name = self.parseIdentifier() orelse return error.ExpectedRuleName;
        if (name.len > abi.MAX_RULE_NAME) return error.RuleNameTooLong;
        const owned_name = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned_name);

        self.skipWs();
        if (!self.match('=')) {
            return error.ExpectedEquals;
        }

        // Parse expression
        const expr = try self.parseExpression();
        errdefer freeExpr(self.allocator, expr);

        // Optional semantic action
        var action: ?[]const u8 = null;
        self.skipWs();
        if (self.matchStr("->")) {
            self.skipWs();
            const action_name = self.parseIdentifier() orelse {
                return error.ExpectedActionName;
            };
            action = try self.allocator.dupe(u8, action_name);
        }

        const rule = try self.allocator.create(Rule);
        rule.* = .{
            .name = owned_name,
            .expr = expr,
            .action = action,
            .silent = is_silent,
        };
        return rule;
    }

    fn parseExpression(self: *GrammarParserImpl) ParseErr!*Expr {
        self.skipWs();
        const first = try self.parseSequence();

        var options: std.ArrayList(*Expr) = .empty;
        defer options.deinit(self.allocator);
        try options.append(self.allocator, first);

        while (true) {
            self.skipWs();
            if (self.match('|')) {
                self.skipWs();
                const opt = try self.parseSequence();
                try options.append(self.allocator, opt);
            } else {
                break;
            }
        }

        if (options.items.len == 1) {
            return options.items[0];
        }

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .tag = .alternative,
            .children = try self.allocator.dupe(*Expr, options.items),
        };
        return expr;
    }

    fn parseSequence(self: *GrammarParserImpl) ParseErr!*Expr {
        var items: std.ArrayList(*Expr) = .empty;
        defer items.deinit(self.allocator);

        while (true) {
            self.skipWs();
            if (self.atSequenceEnd()) break;
            const item = try self.parsePrefix() orelse break;
            try items.append(self.allocator, item);
        }

        if (items.items.len == 0) {
            return error.ExpectedExpression;
        }
        if (items.items.len == 1) {
            return items.items[0];
        }

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .tag = .sequence,
            .children = try self.allocator.dupe(*Expr, items.items),
        };
        return expr;
    }

    fn atSequenceEnd(self: *GrammarParserImpl) bool {
        if (self.pos >= self.text.len) return true;
        const c = self.peek();
        if (c == '|' or c == ')') return true;
        if (self.peekStr("->")) return true;

        // Check if we hit a new rule (identifier followed by =)
        if (std.ascii.isAlphabetic(c) or c == '_') {
            const saved_pos = self.pos;
            const saved_line = self.line;
            const saved_col = self.col;

            const ident = self.parseIdentifier();
            self.skipWs();
            const is_rule = (self.pos < self.text.len and self.peek() == '=' and !self.peekStr("=="));

            // Restore position
            self.pos = saved_pos;
            self.line = saved_line;
            self.col = saved_col;

            if (ident != null and is_rule) return true;
        }
        return false;
    }

    fn parsePrefix(self: *GrammarParserImpl) ParseErr!?*Expr {
        self.skipWs();
        if (self.match('!')) {
            const sub = try self.parsePrefix() orelse return error.ExpectedExprAfterPredicate;
            errdefer freeExpr(self.allocator, sub);
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .tag = .not_predicate,
                .pred_expr = sub,
            };
            return expr;
        }
        if (self.match('&')) {
            const sub = try self.parsePrefix() orelse return error.ExpectedExprAfterPredicate;
            errdefer freeExpr(self.allocator, sub);
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .tag = .and_predicate,
                .pred_expr = sub,
            };
            return expr;
        }
        return self.parseSuffix();
    }

    fn parseSuffix(self: *GrammarParserImpl) ParseErr!?*Expr {
        const primary = try self.parsePrimary() orelse return null;
        errdefer freeExpr(self.allocator, primary);
        self.skipWsInline();

        if (self.match('*')) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .tag = .repetition, .rep_expr = primary, .rep_kind = '*' };
            return expr;
        }
        if (self.match('+')) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .tag = .repetition, .rep_expr = primary, .rep_kind = '+' };
            return expr;
        }
        if (self.match('?')) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .tag = .repetition, .rep_expr = primary, .rep_kind = '?' };
            return expr;
        }
        return primary;
    }

    fn parsePrimary(self: *GrammarParserImpl) ParseErr!?*Expr {
        self.skipWs();
        if (self.pos >= self.text.len) return null;

        const c = self.peek();

        // Grouped expression
        if (c == '(') {
            if (self.depth >= MAX_NESTING_DEPTH) return error.NestingTooDeep;
            self.depth += 1;
            defer self.depth -= 1;
            self.advance();
            const inner = try self.parseExpression();
            self.skipWs();
            if (!self.match(')')) return error.ExpectedCloseParen;
            return inner;
        }

        // String literal
        if (c == '"' or c == '\'') {
            return self.parseLiteral();
        }

        // Character class
        if (c == '[') {
            return self.parseCharClass();
        }

        // Any char
        if (c == '.') {
            self.advance();
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .tag = .any_char };
            return expr;
        }

        // Reference
        if (std.ascii.isAlphabetic(c) or c == '_') {
            const name = self.parseIdentifier() orelse return null;
            const owned = try self.allocator.dupe(u8, name);
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .tag = .reference,
                .ref_name = owned,
            };
            return expr;
        }

        return null;
    }

    fn parseLiteral(self: *GrammarParserImpl) ParseErr!*Expr {
        const quote = self.peek();
        self.advance();
        const start = self.pos;

        while (self.pos < self.text.len and self.peek() != quote) {
            if (self.peek() == '\\') {
                self.advance(); // skip escape char
            }
            self.advance();
        }
        if (self.pos >= self.text.len) return error.UnterminatedString;

        const raw = self.text[start..self.pos];
        if (raw.len == 0) return error.EmptyLiteral;
        const unescaped = try self.unescape(raw);
        self.advance(); // skip closing quote

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .tag = .literal,
            .literal_value = unescaped,
        };
        return expr;
    }

    fn parseCharClass(self: *GrammarParserImpl) ParseErr!*Expr {
        self.advance(); // skip '['
        var negated = false;
        if (self.pos < self.text.len and self.peek() == '^') {
            negated = true;
            self.advance();
        }

        var ranges: std.ArrayList(CharRange) = .empty;
        defer ranges.deinit(self.allocator);

        while (self.pos < self.text.len and self.peek() != ']') {
            const c1 = try self.readClassChar();
            if (self.pos < self.text.len and self.peek() == '-') {
                self.advance(); // skip '-'
                if (self.pos < self.text.len and self.peek() != ']') {
                    const c2 = try self.readClassChar();
                    if (c1 > c2) return error.InvalidCharRange;
                    try ranges.append(self.allocator, .{ .start = c1, .end = c2 });
                } else {
                    try ranges.append(self.allocator, .{ .start = c1, .end = c1 });
                    try ranges.append(self.allocator, .{ .start = '-', .end = '-' });
                }
            } else {
                try ranges.append(self.allocator, .{ .start = c1, .end = c1 });
            }
        }

        if (self.pos >= self.text.len) return error.UnterminatedCharClass;
        self.advance(); // skip ']'

        const expr = try self.allocator.create(Expr);
        expr.* = .{
            .tag = .char_class,
            .char_ranges = try self.allocator.dupe(CharRange, ranges.items),
            .char_negated = negated,
        };
        return expr;
    }

    fn readClassChar(self: *GrammarParserImpl) ParseErr!u8 {
        if (self.peek() == '\\') {
            self.advance();
            return self.readEscape();
        }
        const c = self.peek();
        self.advance();
        return c;
    }

    fn readEscape(self: *GrammarParserImpl) u8 {
        if (self.pos >= self.text.len) return '\\';
        const c = self.peek();
        self.advance();
        return switch (c) {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            else => c,
        };
    }

    fn unescape(self: *GrammarParserImpl, s: []const u8) ParseErr![]const u8 {
        var result: std.ArrayList(u8) = .empty;
        defer result.deinit(self.allocator);

        var i: usize = 0;
        while (i < s.len) {
            if (s[i] == '\\' and i + 1 < s.len) {
                const next = s[i + 1];
                const ch: u8 = switch (next) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '\'' => '\'',
                    '"' => '"',
                    else => next,
                };
                try result.append(self.allocator, ch);
                i += 2;
            } else {
                try result.append(self.allocator, s[i]);
                i += 1;
            }
        }

        return try self.allocator.dupe(u8, result.items);
    }

    fn parseIdentifier(self: *GrammarParserImpl) ?[]const u8 {
        const start = self.pos;
        if (self.pos < self.text.len and (std.ascii.isAlphabetic(self.peek()) or self.peek() == '_')) {
            self.advance();
            while (self.pos < self.text.len and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_')) {
                self.advance();
            }
            return self.text[start..self.pos];
        }
        return null;
    }

    // ---- Low-level helpers ----

    fn peek(self: *const GrammarParserImpl) u8 {
        if (self.pos < self.text.len) return self.text[self.pos];
        return 0;
    }

    fn peekStr(self: *const GrammarParserImpl, s: []const u8) bool {
        if (self.pos + s.len > self.text.len) return false;
        return std.mem.eql(u8, self.text[self.pos..][0..s.len], s);
    }

    fn advance(self: *GrammarParserImpl) void {
        if (self.pos < self.text.len) {
            if (self.text[self.pos] == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn match(self: *GrammarParserImpl, c: u8) bool {
        if (self.pos < self.text.len and self.text[self.pos] == c) {
            self.advance();
            return true;
        }
        return false;
    }

    fn matchStr(self: *GrammarParserImpl, s: []const u8) bool {
        if (!self.peekStr(s)) return false;
        for (0..s.len) |_| {
            self.advance();
        }
        return true;
    }

    fn skipWs(self: *GrammarParserImpl) void {
        while (self.pos < self.text.len) {
            const c = self.text[self.pos];
            if (c == ' ' or c == '\t' or c == '\r' or c == '\n') {
                self.advance();
            } else if (c == '#') {
                self.skipLine();
            } else {
                break;
            }
        }
    }

    fn skipWsInline(self: *GrammarParserImpl) void {
        while (self.pos < self.text.len and (self.text[self.pos] == ' ' or self.text[self.pos] == '\t')) {
            self.advance();
        }
    }

    fn skipLine(self: *GrammarParserImpl) void {
        while (self.pos < self.text.len and self.text[self.pos] != '\n') {
            self.advance();
        }
        if (self.pos < self.text.len) {
            self.advance();
        }
    }
};

// ============================================================================
// Left-recursion detection
// ============================================================================

/// Detect left recursion: a rule that can reach itself at the "first position"
/// (i.e., without consuming any input first). This would cause infinite
/// loops or stack overflow in PEG parsing.
fn detectLeftRecursion(allocator: Allocator, rules: []const *Rule) ParseErr!void {
    const n = rules.len;
    if (n == 0) return;

    // Build "can-start-with" adjacency: edges[i] contains rule indices
    // that rule i can invoke at first position without consuming input.
    const max_edges = 64;
    const edges = allocator.alloc([max_edges]usize, n) catch return error.OutOfMemory;
    defer allocator.free(edges);
    const edge_counts = allocator.alloc(usize, n) catch return error.OutOfMemory;
    defer allocator.free(edge_counts);
    @memset(edge_counts, 0);

    for (rules, 0..) |rule, i| {
        collectFirstRefs(rule.expr, rules, &edges[i], &edge_counts[i]);
    }

    // DFS cycle detection: for each rule, check if it can reach itself
    // through first-position references.
    const state = allocator.alloc(Color, n) catch return error.OutOfMemory;
    defer allocator.free(state);
    @memset(state, .white);

    for (0..n) |i| {
        if (state[i] == .white) {
            if (hasCycle(edges, edge_counts, state, i))
                return error.LeftRecursion;
        }
    }
}

const Color = enum { white, gray, black };

/// DFS cycle detection. Returns true if a cycle is found from node `u`.
fn hasCycle(
    edges: [][64]usize,
    edge_counts: []const usize,
    state: []Color,
    u: usize,
) bool {
    state[u] = .gray;
    for (edges[u][0..edge_counts[u]]) |v| {
        if (state[v] == .gray) return true; // back edge → cycle
        if (state[v] == .white and hasCycle(edges, edge_counts, state, v)) return true;
    }
    state[u] = .black;
    return false;
}

/// Collect rule indices that `expr` can invoke at first position
/// (before consuming any input).
fn collectFirstRefs(expr: *const Expr, rules: []const *Rule, out: *[64]usize, count: *usize) void {
    switch (expr.tag) {
        .reference => {
            const name = expr.ref_name orelse return;
            for (rules, 0..) |rule, i| {
                if (std.mem.eql(u8, rule.name, name)) {
                    // Avoid duplicates
                    for (out.*[0..count.*]) |existing| {
                        if (existing == i) return;
                    }
                    if (count.* < 64) {
                        out.*[count.*] = i;
                        count.* += 1;
                    }
                    return;
                }
            }
        },
        .sequence => {
            // First position of a sequence is the first child
            if (expr.children) |children| {
                if (children.len > 0) {
                    collectFirstRefs(children[0], rules, out, count);
                }
            }
        },
        .alternative => {
            // First position of an alternative is ANY of its branches
            if (expr.children) |children| {
                for (children) |child| {
                    collectFirstRefs(child, rules, out, count);
                }
            }
        },
        .repetition => {
            // For ?, *, + the sub-expression is at first position
            if (expr.rep_expr) |sub| {
                collectFirstRefs(sub, rules, out, count);
            }
        },
        .not_predicate, .and_predicate => {
            // Predicates don't consume input, so what follows them
            // is also at first position. But predicates themselves
            // don't "call" rules in a way that produces left recursion
            // since they don't advance. Skip them.
        },
        .literal, .char_class, .any_char => {
            // Terminal expressions — they consume input, no first-position refs
        },
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Parse a PEG grammar string into a runtime IR.
pub fn parseGrammar(allocator: Allocator, text: []const u8) ParseErr!*Grammar {
    var parser = GrammarParserImpl.init(allocator, text);
    return parser.parse();
}
