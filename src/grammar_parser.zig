//! Grammar string parser: converts PEG-like grammar text into a runtime IR.
//!
//! Grammar syntax:
//!     rule_name = expression                     # or: rule_name "display name" = expression
//!     expression = sequence (('|' | '/') sequence)*   # ordered choice
//!     sequence   = prefix+
//!     prefix     = ('!' | '&')? suffix           # predicates
//!     suffix     = primary ('*' | '+' | '?')?    # repetition
//!     primary    = (label ':')? reference | literal | char_class | '(' expression ')' | '.'
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
    /// Id of the label on this reference (`label:rule`), an index into
    /// Grammar.fields plus one; 0 = unlabelled
    field_id: u8 = 0,

    // sequence / alternative
    children: ?[]const *Expr = null,

    // repetition
    rep_expr: ?*Expr = null,
    rep_kind: u8 = 0, // '*', '+', '?'

    // not_predicate / and_predicate
    pred_expr: ?*Expr = null,
};

/// How a rule's chain `head (group)*` is folded into nested nodes.
pub const Fold = enum(u8) {
    none = 0,
    /// @left: each repetition wraps everything before it: ((a op b) op c)
    left = 1,
    /// @right: each repetition wraps everything after it: (a op (b op c))
    right = 2,
    /// @postfix: each repeated node adopts everything before it as its first child
    postfix = 3,
};

pub const Rule = struct {
    name: []const u8,
    /// What error messages call the rule (`name "display name" = ...`)
    display: ?[]const u8 = null,
    expr: *Expr,
    action: ?[]const u8 = null,
    /// `-> Name()`: call the class with no arguments
    action_no_args: bool = false,
    /// Explicit @silent annotation — forces rule to be silent (no parse tree node).
    silent: bool = false,
    /// @memo annotation — cache the rule's result per input position (packrat).
    memo: bool = false,
    /// @left / @right / @postfix annotation. With no repetition matched the
    /// rule produces no node of its own: its operand stands in for it.
    fold: Fold = .none,
    /// @recover(expr): when error recovery skips a broken occurrence of this
    /// rule in a repetition, it resumes right after the next match of `expr`
    /// (a `;`, say) instead of wherever the rule can start again
    recover: ?*Expr = null,
};

pub const Grammar = struct {
    rules: []const *Rule,
    /// Label names; a node's field id is the index here plus one
    fields: []const []const u8 = &.{},

    pub fn deinit(self: *const Grammar, allocator: Allocator) void {
        for (self.fields) |f| allocator.free(f);
        allocator.free(self.fields);
        for (self.rules) |rule| {
            freeExpr(allocator, rule.expr);
            if (rule.recover) |r| freeExpr(allocator, r);
            if (rule.action) |a| allocator.free(a);
            if (rule.display) |d| allocator.free(d);
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
    UnknownAnnotation,
    ExpectedRuleAfterLabel,
    InvalidFoldRule,
    TooManyFields,
};

const MAX_NESTING_DEPTH = 128;

const GrammarParserImpl = struct {
    text: []const u8,
    pos: usize,
    line: usize,
    col: usize,
    depth: usize,
    allocator: Allocator,
    fields: std.ArrayList([]const u8) = .empty,

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

        // One rule id stays free: the one error nodes get (recovery)
        if (rules_list.items.len >= abi.MAX_RULES) {
            return error.TooManyRules;
        }

        // Validate references
        for (rules_list.items) |rule| {
            try self.validateRefs(rule.expr, rules_list.items);
            if (rule.recover) |r| try self.validateRefs(r, rules_list.items);
        }

        // Detect left recursion (rule can reach itself without consuming input)
        try detectLeftRecursion(self.allocator, rules_list.items);

        const grammar = try self.allocator.create(Grammar);
        grammar.* = .{
            .rules = try self.allocator.dupe(*Rule, rules_list.items),
            .fields = try self.fields.toOwnedSlice(self.allocator),
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

        // Annotations: @silent, @memo, @left / @right / @postfix, @recover(expr) (any order)
        var is_silent = false;
        var is_memo = false;
        var fold: Fold = .none;
        var recover: ?*Expr = null;
        errdefer if (recover) |r| freeExpr(self.allocator, r);
        while (self.peek() == '@') {
            self.advance(); // skip '@'
            const annotation = self.parseIdentifier() orelse return error.UnknownAnnotation;
            if (std.mem.eql(u8, annotation, "silent")) {
                is_silent = true;
            } else if (std.mem.eql(u8, annotation, "memo")) {
                is_memo = true;
            } else if (std.mem.eql(u8, annotation, "recover")) {
                if (recover != null or !self.match('(')) return error.UnknownAnnotation;
                recover = try self.parseExpression();
                self.skipWs();
                if (!self.match(')')) return error.ExpectedCloseParen;
            } else if (std.meta.stringToEnum(Fold, annotation)) |f| {
                if (f == .none or fold != .none) return error.UnknownAnnotation;
                fold = f;
            } else {
                return error.UnknownAnnotation;
            }
            self.skipWs();
        }

        // Parse rule name
        const name = self.parseIdentifier() orelse return error.ExpectedRuleName;
        if (name.len > abi.MAX_RULE_NAME) return error.RuleNameTooLong;
        const owned_name = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned_name);

        // Optional display name for error messages: name "display name" = ...
        self.skipWs();
        var display: ?[]const u8 = null;
        errdefer if (display) |d| self.allocator.free(d);
        if (self.peek() == '"' or self.peek() == '\'') {
            const lit = try self.parseLiteral();
            display = lit.literal_value;
            self.allocator.destroy(lit);
            if (display.?.len > abi.MAX_RULE_NAME) return error.RuleNameTooLong;
        }

        self.skipWs();
        if (!self.match('=')) {
            return error.ExpectedEquals;
        }

        // Parse expression
        const expr = try self.parseExpression();
        errdefer freeExpr(self.allocator, expr);

        // Optional semantic action
        var action: ?[]const u8 = null;
        var no_args = false;
        self.skipWs();
        if (self.matchStr("->")) {
            self.skipWs();
            const action_name = self.parseIdentifier() orelse {
                return error.ExpectedActionName;
            };
            action = try self.allocator.dupe(u8, action_name);
            no_args = self.matchStr("()");
        }

        // A folded rule is `head (group)*`: a sequence ending in a repetition
        if (fold != .none) {
            const seq = if (expr.tag == .sequence) expr.children.? else return error.InvalidFoldRule;
            if (is_silent or seq[seq.len - 1].tag != .repetition) return error.InvalidFoldRule;
        }

        const rule = try self.allocator.create(Rule);
        rule.* = .{
            .name = owned_name,
            .expr = expr,
            .action = action,
            .silent = is_silent,
            .memo = is_memo,
            .fold = fold,
            .display = display,
            .action_no_args = no_args,
            .recover = recover,
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
            if (self.match('|') or self.match('/')) {
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
        if (c == '|' or c == '/' or c == ')') return true;
        if (self.peekStr("->")) return true;

        // Check if we hit a new rule (identifier followed by =)
        if (std.ascii.isAlphabetic(c) or c == '_') {
            const saved_pos = self.pos;
            const saved_line = self.line;
            const saved_col = self.col;

            const ident = self.parseIdentifier();
            self.skipWs();
            // ... or by a display name and =
            const quote = self.peek();
            if (quote == '"' or quote == '\'') {
                self.advance();
                while (self.pos < self.text.len and self.peek() != quote) {
                    if (self.peek() == '\\') self.advance();
                    self.advance();
                }
                self.advance();
                self.skipWs();
            }
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

        // Reference, optionally labelled: label:rule
        if (std.ascii.isAlphabetic(c) or c == '_') {
            var name = self.parseIdentifier() orelse return null;
            var field_id: u8 = 0;
            if (self.peek() == ':') {
                self.advance();
                field_id = try self.fieldId(name);
                name = self.parseIdentifier() orelse return error.ExpectedRuleAfterLabel;
            }
            const owned = try self.allocator.dupe(u8, name);
            const expr = try self.allocator.create(Expr);
            expr.* = .{
                .tag = .reference,
                .ref_name = owned,
                .field_id = field_id,
            };
            return expr;
        }

        return null;
    }

    /// Id of the label `name`, registering it on first use.
    fn fieldId(self: *GrammarParserImpl, name: []const u8) ParseErr!u8 {
        for (self.fields.items, 1..) |f, id| {
            if (std.mem.eql(u8, f, name)) return @intCast(id);
        }
        if (self.fields.items.len >= abi.MAX_FIELDS) return error.TooManyFields;
        const owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned);
        try self.fields.append(self.allocator, owned);
        return @intCast(self.fields.items.len);
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

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var index: std.StringHashMapUnmanaged(usize) = .empty;
    try index.ensureTotalCapacity(alloc, @intCast(n));
    for (rules, 0..) |rule, i| index.putAssumeCapacity(rule.name, i);

    // "Can start with" adjacency: edges[i] holds the rules that rule i can
    // invoke at first position without consuming input.
    const edges = try alloc.alloc(std.ArrayList(usize), n);
    for (rules, edges) |rule, *list| {
        list.* = .empty;
        try collectFirstRefs(alloc, rule.expr, &index, list);
    }

    // DFS cycle detection: can a rule reach itself through those edges?
    const state = try alloc.alloc(Color, n);
    @memset(state, .white);
    for (0..n) |i| {
        if (state[i] == .white and hasCycle(edges, state, i)) return error.LeftRecursion;
    }
}

const Color = enum { white, gray, black };

/// DFS cycle detection. Returns true if a cycle is found from node `u`.
fn hasCycle(edges: []const std.ArrayList(usize), state: []Color, u: usize) bool {
    state[u] = .gray;
    for (edges[u].items) |v| {
        if (state[v] == .gray) return true; // back edge → cycle
        if (state[v] == .white and hasCycle(edges, state, v)) return true;
    }
    state[u] = .black;
    return false;
}

/// Collect the rules that `expr` can invoke at first position (before
/// consuming any input).
fn collectFirstRefs(allocator: Allocator, expr: *const Expr, index: *const std.StringHashMapUnmanaged(usize), out: *std.ArrayList(usize)) ParseErr!void {
    switch (expr.tag) {
        .reference => {
            const i = index.get(expr.ref_name orelse return) orelse return;
            if (std.mem.indexOfScalar(usize, out.items, i) == null) try out.append(allocator, i);
        },
        .sequence => {
            // First position of a sequence is the first child
            if (expr.children) |children| {
                if (children.len > 0) try collectFirstRefs(allocator, children[0], index, out);
            }
        },
        .alternative => {
            // First position of an alternative is ANY of its branches
            if (expr.children) |children| {
                for (children) |child| try collectFirstRefs(allocator, child, index, out);
            }
        },
        .repetition => {
            // For ?, *, + the sub-expression is at first position
            if (expr.rep_expr) |sub| try collectFirstRefs(allocator, sub, index, out);
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
// AST mapping: actions and label multiplicity
// ============================================================================

/// What `-> name` after a rule converts its node to (see parse_ast).
pub const Action = enum(u8) {
    /// No action: the text of a leaf, the value of an only child, else a list
    none,
    str,
    int,
    float,
    list,
    tuple,
    dict,
    true,
    false,
    null,
    /// The text of a quoted string literal: without its first and last
    /// character, and with backslash escapes replaced
    unquote,
    /// The first child's value
    first,
    /// No value: left out of the parent's children
    drop,
    /// Any other name: a class (or callable) supplied by the user
    class,
};

/// What error messages call a rule.
pub fn displayName(rule: *const Rule) []const u8 {
    return rule.display orelse rule.name;
}

pub fn actionOf(rule: *const Rule) Action {
    const name = rule.action orelse return .none;
    const builtins = [_]struct { []const u8, Action }{
        .{ "str", .str },     .{ "int", .int },     .{ "float", .float }, .{ "list", .list },
        .{ "tuple", .tuple }, .{ "dict", .dict },   .{ "True", .true },   .{ "False", .false },
        .{ "None", .null },   .{ "first", .first }, .{ "drop", .drop },   .{ "unquote", .unquote },
    };
    for (builtins) |b| {
        if (std.mem.eql(u8, b[0], name)) return b[1];
    }
    return .class;
}

/// A label used by a rule: can several children carry it (a list), or at
/// most one?
pub const LabelUse = struct {
    field: u8,
    many: bool,
};

/// Occurrence counts, saturating at 2 ("many")
const Counts = [256]u8;
const MAX_SILENT_DEPTH = 16;

fn findRule(grammar: *const Grammar, name: []const u8) ?*const Rule {
    for (grammar.rules) |r| {
        if (std.mem.eql(u8, r.name, name)) return r;
    }
    return null;
}

/// How many top-level nodes matching `expr` can add: 0, 1 or 2 (several).
fn nodeCount(grammar: *const Grammar, expr: *const Expr, depth: u8) u8 {
    switch (expr.tag) {
        .literal, .char_class, .any_char, .not_predicate, .and_predicate => return 0,
        .reference => {
            const rule = findRule(grammar, expr.ref_name orelse return 0) orelse return 0;
            if (!rule.silent) return 1;
            if (depth >= MAX_SILENT_DEPTH) return 2;
            return nodeCount(grammar, rule.expr, depth + 1);
        },
        // (2 is the most: the rest isn't looked at once it's reached, or a
        // silent rule calling itself several times takes exponential time)
        .sequence => {
            var n: u8 = 0;
            for (expr.children orelse return 0) |child| {
                n = @min(2, n + nodeCount(grammar, child, depth));
                if (n == 2) break;
            }
            return n;
        },
        .alternative => {
            var n: u8 = 0;
            for (expr.children orelse return 0) |child| {
                n = @max(n, nodeCount(grammar, child, depth));
                if (n == 2) break;
            }
            return n;
        },
        .repetition => {
            const n = nodeCount(grammar, expr.rep_expr orelse return 0, depth);
            return if (expr.rep_kind == '?' or n == 0) n else 2;
        },
    }
}

/// Count, per label, the children of a node that can carry it. Labels inside
/// @silent rules count for the rule that references them. `fold_rep` is the
/// trailing repetition of a folded rule: each of its iterations is a node of
/// its own, so it counts once.
fn labelCounts(grammar: *const Grammar, expr: *const Expr, depth: u8, fold_rep: ?*const Expr, out: *Counts, memo: *LabelMemo) void {
    switch (expr.tag) {
        .literal, .char_class, .any_char, .not_predicate, .and_predicate => {},
        .reference => {
            const idx = findRuleIndex(grammar, expr.ref_name orelse return) orelse return;
            const rule = grammar.rules[idx];
            if (expr.field_id != 0) {
                const n: u8 = if (rule.silent) @max(1, nodeCount(grammar, rule.expr, depth + 1)) else 1;
                out[expr.field_id] = @min(2, out[expr.field_id] + n);
            } else if (rule.silent and depth < MAX_SILENT_DEPTH) {
                const sub = silentLabels(grammar, idx, depth + 1, memo);
                for (out, sub) |*o, c| o.* = @min(2, o.* + c);
            }
        },
        .sequence => {
            for (expr.children orelse return) |child| labelCounts(grammar, child, depth, fold_rep, out, memo);
        },
        .alternative => {
            var most: Counts = @splat(0);
            for (expr.children orelse return) |child| {
                var branch: Counts = @splat(0);
                labelCounts(grammar, child, depth, null, &branch, memo);
                for (&most, branch) |*m, c| m.* = @max(m.*, c);
            }
            for (out, most) |*o, m| o.* = @min(2, o.* + m);
        },
        .repetition => {
            var inner: Counts = @splat(0);
            labelCounts(grammar, expr.rep_expr orelse return, depth, null, &inner, memo);
            const once = expr.rep_kind == '?' or expr == fold_rep;
            for (out, inner) |*o, c| o.* = @min(2, o.* + if (once or c == 0) c else 2);
        },
    }
}

/// The label counts of a silent rule's body at a depth, each worked out
/// once (by rule and depth: what they are depends on nothing else), or a
/// silent rule calling itself several times takes exponential time.
const LabelMemo = struct {
    known: []bool,
    counts: []Counts,

    fn slot(idx: usize, depth: u8) usize {
        return idx * (MAX_SILENT_DEPTH + 1) + depth;
    }
};

fn silentLabels(grammar: *const Grammar, idx: usize, depth: u8, memo: *LabelMemo) Counts {
    const s = LabelMemo.slot(idx, depth);
    if (memo.known[s]) return memo.counts[s];
    var c: Counts = @splat(0);
    labelCounts(grammar, grammar.rules[idx].expr, depth, null, &c, memo);
    memo.counts[s] = c;
    memo.known[s] = true;
    return c;
}

fn findRuleIndex(grammar: *const Grammar, name: []const u8) ?usize {
    for (grammar.rules, 0..) |r, i| {
        if (std.mem.eql(u8, r.name, name)) return i;
    }
    return null;
}

/// The labels a rule's node can have on its children, in field id order.
pub fn ruleLabels(allocator: Allocator, grammar: *const Grammar, rule: *const Rule) ![]LabelUse {
    var counts: Counts = @splat(0);
    const fold_rep: ?*const Expr = if (rule.fold != .none) blk: {
        const seq = rule.expr.children.?;
        break :blk seq[seq.len - 1];
    } else null;
    const slots = grammar.rules.len * (MAX_SILENT_DEPTH + 1);
    var memo: LabelMemo = .{ .known = try allocator.alloc(bool, slots), .counts = try allocator.alloc(Counts, slots) };
    defer allocator.free(memo.known);
    defer allocator.free(memo.counts);
    @memset(memo.known, false);
    labelCounts(grammar, rule.expr, 0, fold_rep, &counts, &memo);

    var list: std.ArrayList(LabelUse) = .empty;
    errdefer list.deinit(allocator);
    for (counts, 0..) |c, field| {
        if (c != 0) try list.append(allocator, .{ .field = @intCast(field), .many = c > 1 });
    }
    return list.toOwnedSlice(allocator);
}

/// Can a node of this rule have children?
pub fn ruleHasChildren(grammar: *const Grammar, rule: *const Rule) bool {
    return nodeCount(grammar, rule.expr, 0) != 0;
}

// ============================================================================
// Public API
// ============================================================================

/// Parse a PEG grammar string into a runtime IR.
pub fn parseGrammar(allocator: Allocator, text: []const u8) ParseErr!*Grammar {
    var parser = GrammarParserImpl.init(allocator, text);
    return parser.parse();
}
