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

/// Maximum number of rules in a grammar
pub const MAX_RULES = 256;
/// Maximum rule name length
pub const MAX_RULE_NAME = 64;

/// A flat node in the parse result (C ABI compatible, 16 bytes)
pub const FlatNode = extern struct {
    /// Byte offset of match start in input
    text_start: u32 = 0,
    /// Byte offset of match end in input
    text_end: u32 = 0,
    /// Total number of descendant nodes in this node's subtree (0 = leaf)
    subtree_size: u32 = 0,
    /// Number of direct children (lower 16 bits) + rule ID (upper 16 bits)
    child_count_and_rule: u32 = 0,

    pub inline fn child_count(self: FlatNode) u16 {
        return @truncate(self.child_count_and_rule & 0xFFFF);
    }

    pub inline fn rule_id(self: FlatNode) u16 {
        return @truncate(self.child_count_and_rule >> 16);
    }

    pub inline fn setChildCountAndRule(child_cnt: u16, rid: u16) u32 {
        return @as(u32, child_cnt) | (@as(u32, rid) << 16);
    }
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
};

/// Field offsets used by the code generator
pub const OFF_NODES_PTR = @offsetOf(ParseOutput, "nodes_ptr");
pub const OFF_NODE_COUNT = @offsetOf(ParseOutput, "node_count");
pub const OFF_NODE_CAPACITY = @offsetOf(ParseOutput, "node_capacity");
pub const OFF_MAX_POS = @offsetOf(ParseOutput, "max_pos");
pub const OFF_MAX_POS_RULE_ID = @offsetOf(ParseOutput, "max_pos_rule_id");
pub const OFF_END_POS = @offsetOf(ParseOutput, "end_pos");

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
