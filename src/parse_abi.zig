//! Shared C ABI contract between zgram and compiled grammar .so files.
//!
//! The grammar .so exports: zgram_parse(input_ptr, input_len, out) -> i32
//! It writes a dynamically growing array of FlatNode structs into the output.
//! The zgram module reads these and converts them into Node class instances.

const std = @import("std");

/// Initial node capacity (grows as needed)
pub const INITIAL_NODE_CAPACITY = 8192;
/// Maximum number of rules in a grammar
pub const MAX_RULES = 256;
/// Maximum rule name length (for the rule name table)
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

/// Rule name entry in the rule name table
pub const RuleNameEntry = extern struct {
    name: [MAX_RULE_NAME]u8 = [_]u8{0} ** MAX_RULE_NAME,
    name_len: u8 = 0,
};

/// Parse output from grammar .so (C ABI compatible)
pub const ParseOutput = extern struct {
    /// 1 = success, 0 = error
    status: u8 = 0,

    /// Pointer to dynamically allocated node array
    nodes_ptr: ?[*]FlatNode = null,
    node_count: u32 = 0,
    node_capacity: u32 = 0,

    /// Rule name table (filled by grammar .so at parse time)
    rule_names: [MAX_RULES]RuleNameEntry = [_]RuleNameEntry{.{}} ** MAX_RULES,
    rule_count: u16 = 0,

    /// On error: error information
    error_offset: u32 = 0,
    error_line: u32 = 0,
    error_col: u32 = 0,
    error_message: [256]u8 = [_]u8{0} ** 256,
    error_message_len: u32 = 0,
};

/// Function signature that grammar .so files export
pub const ParseFn = *const fn (
    input_ptr: [*]const u8,
    input_len: usize,
    output: *ParseOutput,
) callconv(.c) i32;
