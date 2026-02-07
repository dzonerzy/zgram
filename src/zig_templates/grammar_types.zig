//! Comptime grammar data structures.
//! These types mirror the Python IR (grammar.py) and are filled in by codegen.

/// A character range for character classes (e.g., 'a'-'z')
pub const CharRange = struct {
    start: u8,
    end: u8,
};

/// Expression types in the grammar
pub const ExprTag = enum {
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

/// A grammar expression (comptime, recursive via indices)
pub const Expr = struct {
    tag: ExprTag,

    // literal
    literal_value: []const u8 = "",

    // char_class
    char_ranges: []const CharRange = &.{},
    char_negated: bool = false,

    // reference: index into Grammar.rules array (resolved by codegen)
    ref_index: usize = 0,

    // sequence / alternative: indices into a shared sub-expression array
    children: []const Expr = &.{},

    // repetition
    rep_expr: ?*const Expr = null,
    rep_kind: u8 = 0, // '*' = 0x2A, '+' = 0x2B, '?' = 0x3F

    // not_predicate / and_predicate
    pred_expr: ?*const Expr = null,
};

/// A named grammar rule
pub const Rule = struct {
    name: []const u8,
    expr: Expr,
};

/// A complete grammar (array of rules, first rule is the start rule)
pub const Grammar = struct {
    rules: []const Rule,
};
