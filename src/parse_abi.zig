//! Shared C ABI contract between zgram and JIT-compiled grammars.
//!
//! The compiled grammar exports:
//!   zgram_parse(input_ptr, input_len, out, start_rule, flags) -> i32
//! It writes a dynamically growing array of FlatNode structs into the output.
//! The zgram module reads these and converts them into Node class instances.
//!
//! ParseOutput is small and holds no per-grammar data, so each parse() call
//! can use its own (on the stack), which makes parsing reentrant.

const std = @import("std");

/// Maximum number of rules in a grammar (rule ids have RULE_MASK's 12 bits)
pub const MAX_RULES = 4096;
/// Maximum rule name length
pub const MAX_RULE_NAME = 64;

/// Maximum number of distinct field names (labels) in a grammar; id 0 = no field
pub const MAX_FIELDS = 255;

/// Layout of FlatNode.meta: child count (12 bits) | rule id (12) | field id (8)
pub const RULE_SHIFT = 12;
pub const RULE_MASK = 0xFFF;
pub const FIELD_SHIFT = 24;
/// Stored child count of a node with this many children or more
pub const CHILD_COUNT_MANY = 0xFFF;

/// A flat node in the parse result (C ABI compatible, 16 bytes)
pub const FlatNode = extern struct {
    /// Byte offset of match start in input
    text_start: u32 = 0,
    /// Byte offset of match end in input
    text_end: u32 = 0,
    /// Total number of descendant nodes in this node's subtree (0 = leaf)
    subtree_size: u32 = 0,
    /// Child count, rule id and field id (see RULE_SHIFT, FIELD_SHIFT).
    /// A count of CHILD_COUNT_MANY means "that many or more": count them by
    /// stepping through the subtree.
    meta: u32 = 0,

    /// The stored count, saturated at CHILD_COUNT_MANY.
    pub inline fn child_count(self: FlatNode) u16 {
        return @truncate(self.meta & CHILD_COUNT_MANY);
    }

    pub inline fn rule_id(self: FlatNode) u16 {
        return @truncate((self.meta >> RULE_SHIFT) & RULE_MASK);
    }

    /// Id of the label this node was matched under in its parent (0 = none)
    pub inline fn field_id(self: FlatNode) u8 {
        return @truncate(self.meta >> FIELD_SHIFT);
    }

    pub inline fn packMeta(child_cnt: u32, rid: u16) u32 {
        return @min(child_cnt, CHILD_COUNT_MANY) | (@as(u32, rid) << RULE_SHIFT);
    }
};

/// Version of the tree interface below (FlatNode's layout and TreeView),
/// bumped on any incompatible change.
pub const TREE_ABI: u32 = 1;

/// A string in a TreeView: not NUL-terminated
pub const Str = extern struct {
    ptr: [*]const u8,
    len: usize,
};

/// What the `zgram.tree.v1` capsule (Tree.capsule) points to: a finished
/// parse tree, for native code in other packages. Everything it points to
/// stays valid while the capsule is referenced. Check `abi` before use.
pub const TreeView = extern struct {
    abi: u32 = TREE_ABI,
    /// Number of nodes; node 0 is the root, the rest follow in pre-order
    node_count: u32 = 0,
    nodes: ?[*]const FlatNode = null,
    /// The parsed text, UTF-8; node offsets index into it
    input: ?[*]const u8 = null,
    input_len: usize = 0,
    rule_count: u32 = 0,
    /// A node's field id is an index into field_names plus one; 0 = no label
    field_count: u32 = 0,
    rule_names: ?[*]const Str = null,
    field_names: ?[*]const Str = null,
};

/// Why a parse failed (ParseOutput.error_kind)
pub const ErrorKind = enum(u8) {
    none = 0,
    /// No match: error_offset is the high-water mark, error_rule_id the rule tried there
    expected_rule = 1,
    /// The start rule matched but didn't consume the whole input
    trailing_input = 2,
    /// Node buffer allocation failed
    out_of_memory = 3,
    /// The input nests deeper than the native stack allows (ParseOutput.stack_limit)
    too_deep = 4,
};

/// Parse flags (zgram_parse `flags` argument)
pub const FLAG_PREFIX: u32 = 1; // succeed without consuming the whole input

/// Parse output (C ABI compatible). The JIT code reads and writes the node
/// fields and the high-water mark directly, at the offsets exported below.
pub const ParseOutput = extern struct {
    /// 1 = success, 0 = error
    status: u8 = 0,
    /// Set by the helpers on failure (ErrorKind)
    error_kind: u8 = 0,

    /// Pointer to dynamically allocated node array (c_allocator)
    nodes_ptr: ?[*]FlatNode = null,
    node_count: u32 = 0,
    node_capacity: u32 = 0,

    /// High-water mark: furthest position reached during parsing
    max_pos: u32 = 0,
    /// Rule ID that was being attempted at the high-water mark position
    max_pos_rule_id: u16 = 0,

    /// On error: location (line/col are 1-based, col counts bytes)
    error_offset: u32 = 0,
    error_line: u32 = 0,
    error_col: u32 = 0,
    error_rule_id: u16 = 0,

    /// On success: end position of the match (== input length unless FLAG_PREFIX)
    end_pos: u64 = 0,

    /// Packrat memo table for @memo rules (jit_helpers.MemoState), created on
    /// first use. The caller frees it with jit_helpers.memoFree() after parsing.
    memo: ?*anyopaque = null,

    /// Set by the caller: the lowest address a rule's frame may be at (0 =
    /// no limit). A recursive rule entered below it fails the whole parse
    /// with ErrorKind.too_deep, and sets this to the highest address so that
    /// every rule entered afterwards fails too.
    stack_limit: usize = 0,

    // Error recovery (the parser compiled in recover mode only)

    /// Positions of the syntax errors known so far, sorted: a repetition
    /// whose element fails after reaching one of them skips the broken text
    /// into an error node and goes on. Set by the caller.
    known_errors: ?[*]const u32 = null,
    known_count: u32 = 0,
    /// The rule id error nodes get: the grammar's rule count. Set by the caller.
    error_rule: u32 = 0,
    /// How freely to skip, raised by the caller each time a round finds
    /// nothing new. 0: skip an element only if its attempt failed furthest
    /// at a known error, and only up to where the element (or an outer
    /// repetition's) goes on. RECOVER_TO_END: skipped text may also run to
    /// the end of the input. RECOVER_LOOSE: also skip an element that
    /// reached a known error but failed further on.
    recover_level: u8 = 0,
    /// Where recovery is looking for a place to resume (zgram_recover_step)
    scan_pos: u64 = 0,
    /// Brackets opened and not closed since the skipped text began
    scan_depth: i64 = 0,
    /// Where the look began, and whether it is the second look, which
    /// ignores the brackets the skipped text opened (the first found no
    /// place outside them: an unclosed `(` in the broken text)
    scan_start: u64 = 0,
    scan_ignoring_opens: bool = false,
    /// Brackets of each kind ( [ { open in the input before bal_pos (a
    /// running count, for telling a stray closing bracket)
    bal_pos: u64 = 0,
    bal: [3]i32 = .{ 0, 0, 0 },
    /// The furthest place a literal that could be inserted was missing: a
    /// more precise position for an error than the furthest failed rule
    /// (where `=` was missing, rather than where the statement began)
    lit_pos: u32 = 0,
    /// The literal missing there (the first one missed at lit_pos), for the
    /// error's message; in the compiled grammar's memory
    lit_len: u32 = 0,
    lit_text: ?[*]const u8 = null,
    /// Each error node made, with the rule the skipped element begins with
    /// (for its error message); a caller-provided buffer, filled while
    /// there's room. Backtracking may drop nodes listed here.
    recovered: ?[*]Recovered = null,
    recovered_count: u32 = 0,
    recovered_capacity: u32 = 0,
    /// Each literal inserted, where: the precise place of the error it was
    /// inserted for (which may be before the whitespace in front of it). A
    /// caller-provided buffer, filled while there's room; backtracking may
    /// drop insertions listed here.
    inserted: ?[*]Inserted = null,
    inserted_count: u32 = 0,
    inserted_capacity: u32 = 0,
};

/// A literal recovery took as present (zero-width) at `pos`. `site`: for
/// a guessed one (the first item of an optional group, see emitSequence),
/// the place in the grammar it was guessed at; 0 for the others
pub const Inserted = extern struct {
    pos: u32,
    len: u32,
    text: [*]const u8,
    site: u32 = 0,
};

/// An error node's start, and the rule the broken element begins with
/// (NO_RULE if it doesn't begin with one)
pub const Recovered = extern struct {
    start: u32,
    rule: u32,
};

pub const NO_RULE: u32 = std.math.maxInt(u32);

/// ParseOutput.recover_level values
pub const RECOVER_TO_END: u8 = 1;
pub const RECOVER_LOOSE: u8 = 2;

/// Field offsets used by the code generator
pub const OFF_NODES_PTR = @offsetOf(ParseOutput, "nodes_ptr");
pub const OFF_NODE_COUNT = @offsetOf(ParseOutput, "node_count");
pub const OFF_NODE_CAPACITY = @offsetOf(ParseOutput, "node_capacity");
pub const OFF_MAX_POS = @offsetOf(ParseOutput, "max_pos");
pub const OFF_MAX_POS_RULE_ID = @offsetOf(ParseOutput, "max_pos_rule_id");
pub const OFF_END_POS = @offsetOf(ParseOutput, "end_pos");
pub const OFF_ERROR_KIND = @offsetOf(ParseOutput, "error_kind");
pub const OFF_LIT_POS = @offsetOf(ParseOutput, "lit_pos");
pub const OFF_LIT_LEN = @offsetOf(ParseOutput, "lit_len");
pub const OFF_LIT_TEXT = @offsetOf(ParseOutput, "lit_text");
pub const OFF_STACK_LIMIT = @offsetOf(ParseOutput, "stack_limit");
pub const OFF_INSERTED_COUNT = @offsetOf(ParseOutput, "inserted_count");

// Compile-time ABI assertions
comptime {
    if (@sizeOf(FlatNode) != 16) @compileError("FlatNode must be exactly 16 bytes");
    if (@alignOf(FlatNode) != 4) @compileError("FlatNode must be 4-byte aligned");
}

/// Function signature exported by compiled grammars
pub const ParseFn = *const fn (
    input_ptr: [*]const u8,
    input_len: usize,
    output: *ParseOutput,
    start_rule: u32,
    flags: u32,
) callconv(.c) i32;

/// Line and column (1-based, column in bytes) of `pos` in `input`.
pub fn lineCol(input: []const u8, pos: usize) struct { line: u32, col: u32 } {
    var line: u32 = 1;
    var col: u32 = 1;
    for (input[0..@min(pos, input.len)]) |ch| {
        if (ch == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    return .{ .line = line, .col = col };
}
