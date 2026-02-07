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
};

const GrammarParserImpl = struct {
    text: []const u8,
    pos: usize,
    line: usize,
    col: usize,
    allocator: Allocator,

    fn init(allocator: Allocator, text: []const u8) GrammarParserImpl {
        return .{
            .text = text,
            .pos = 0,
            .line = 1,
            .col = 1,
            .allocator = allocator,
        };
    }

    fn parse(self: *GrammarParserImpl) ParseErr!*Grammar {
        var rules_list: std.ArrayList(*Rule) = .empty;
        defer rules_list.deinit(self.allocator);

        self.skipWs();
        while (self.pos < self.text.len) {
            if (try self.parseRule()) |rule| {
                try rules_list.append(self.allocator, rule);
            }
            self.skipWs();
        }

        if (rules_list.items.len == 0) {
            return error.EmptyGrammar;
        }

        // Validate references
        for (rules_list.items) |rule| {
            try self.validateRefs(rule.expr, rules_list.items);
        }

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

        // Parse rule name
        const name = self.parseIdentifier() orelse return error.ExpectedRuleName;
        const owned_name = try self.allocator.dupe(u8, name);

        self.skipWs();
        if (!self.match('=')) {
            self.allocator.free(owned_name);
            return error.ExpectedEquals;
        }

        // Parse expression
        const expr = try self.parseExpression();

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
            const sub = try self.parseSuffix() orelse return error.ExpectedExprAfterPredicate;
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .tag = .not_predicate,
                .pred_expr = sub,
            };
            return expr;
        }
        if (self.match('&')) {
            const sub = try self.parseSuffix() orelse return error.ExpectedExprAfterPredicate;
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
// Public API
// ============================================================================

/// Parse a PEG grammar string into a runtime IR.
pub fn parseGrammar(allocator: Allocator, text: []const u8) ParseErr!*Grammar {
    var parser = GrammarParserImpl.init(allocator, text);
    return parser.parse();
}
