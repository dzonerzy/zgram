//! Exported helper functions called by JIT-compiled grammar code.
//!
//! The JIT-generated parser emits calls to these functions instead of
//! reimplementing memory management in LLVM IR. They have C calling
//! convention so LLJIT can resolve them via symbol lookup.
//!
//! The JIT code passes ParseOutput* directly (same pointer from zgram_parse).

const std = @import("std");
const abi = @import("parse_abi.zig");

const allocator = std.heap.c_allocator;

/// Maximum node capacity to prevent unbounded growth (256 MB worth of nodes)
const MAX_NODE_CAPACITY: u32 = 16 * 1024 * 1024;

/// Ensure the node array has capacity for at least `needed` nodes.
/// Returns 1 on success, 0 on failure.
export fn zgram_ensure_capacity(output: *abi.ParseOutput, needed: u32) callconv(.c) i32 {
    if (needed <= output.node_capacity) return 1;
    if (needed > MAX_NODE_CAPACITY) return 0;

    var new_cap: u32 = if (output.node_capacity == 0)
        abi.INITIAL_NODE_CAPACITY
    else
        output.node_capacity;

    while (new_cap < needed) {
        const doubled = @as(u64, new_cap) * 2;
        new_cap = if (doubled > MAX_NODE_CAPACITY)
            MAX_NODE_CAPACITY
        else
            @intCast(doubled);
    }

    if (output.nodes_ptr) |old_ptr| {
        if (output.node_capacity == 0) {
            const new_slice = allocator.alloc(abi.FlatNode, new_cap) catch return 0;
            output.nodes_ptr = new_slice.ptr;
            output.node_capacity = new_cap;
            return 1;
        }
        const old_slice = old_ptr[0..output.node_capacity];
        if (allocator.resize(old_slice, new_cap)) {
            output.node_capacity = new_cap;
            return 1;
        }
        const new_slice = allocator.alloc(abi.FlatNode, new_cap) catch return 0;
        @memcpy(new_slice[0..output.node_count], old_ptr[0..output.node_count]);
        allocator.free(old_slice);
        output.nodes_ptr = new_slice.ptr;
        output.node_capacity = new_cap;
    } else {
        const new_slice = allocator.alloc(abi.FlatNode, new_cap) catch return 0;
        output.nodes_ptr = new_slice.ptr;
        output.node_capacity = new_cap;
    }
    return 1;
}

/// Reserve a node slot. Returns the index, or -1 on failure.
export fn zgram_reserve_node(output: *abi.ParseOutput) callconv(.c) i32 {
    if (output.node_count >= MAX_NODE_CAPACITY) return -1;
    if (zgram_ensure_capacity(output, output.node_count + 1) == 0) return -1;
    const idx = output.node_count;
    output.node_count += 1;
    return @intCast(idx);
}

/// Fill a previously reserved node with its final data.
export fn zgram_fill_node(
    output: *abi.ParseOutput,
    idx: u32,
    rule_id: u16,
    text_start: u32,
    text_end: u32,
    subtree_size: u32,
    child_count: u16,
) callconv(.c) void {
    const nodes = output.nodes_ptr orelse return;
    nodes[idx] = .{
        .text_start = text_start,
        .text_end = text_end,
        .subtree_size = subtree_size,
        .child_count_and_rule = abi.FlatNode.setChildCountAndRule(child_count, rule_id),
    };
}

/// Record a parse error with position and message.
export fn zgram_set_error(
    output: *abi.ParseOutput,
    input_ptr: [*]const u8,
    input_len: usize,
    pos: usize,
    msg_ptr: [*]const u8,
    msg_len: usize,
) callconv(.c) void {
    output.status = 0;
    output.error_offset = @intCast(pos);

    // Compute line/col from input
    var line: u32 = 1;
    var col: u32 = 1;
    const scan_end = @min(pos, input_len);
    for (input_ptr[0..scan_end]) |ch| {
        if (ch == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    output.error_line = line;
    output.error_col = col;

    const mlen: usize = @min(msg_len, 256);
    @memcpy(output.error_message[0..mlen], msg_ptr[0..mlen]);
    output.error_message_len = @intCast(mlen);
}

/// Record a parse error at the high-water mark position.
/// Reads max_pos and max_pos_rule_id from the output struct, looks up the
/// rule name, and formats an "expected <rule_name>" error message.
export fn zgram_set_error_at_hwm(
    output: *abi.ParseOutput,
    input_ptr: [*]const u8,
    input_len: usize,
) callconv(.c) void {
    const pos: usize = output.max_pos;
    const rule_id = output.max_pos_rule_id;

    // Build message: "expected <rule_name>" if we have a rule name, else "unexpected input"
    var msg_buf: [128]u8 = undefined;
    var msg_len: usize = 0;

    if (rule_id < abi.MAX_RULES) {
        const entry = output.rule_names[rule_id];
        if (entry.name_len > 0) {
            const prefix = "expected ";
            @memcpy(msg_buf[0..prefix.len], prefix);
            msg_len = prefix.len;
            const nlen: usize = entry.name_len;
            @memcpy(msg_buf[msg_len .. msg_len + nlen], entry.name[0..nlen]);
            msg_len += nlen;
        }
    }
    if (msg_len == 0) {
        const fallback = "unexpected input";
        @memcpy(msg_buf[0..fallback.len], fallback);
        msg_len = fallback.len;
    }

    output.status = 0;
    output.error_offset = @intCast(pos);

    // Compute line/col from input
    var line: u32 = 1;
    var col: u32 = 1;
    const scan_end = @min(pos, input_len);
    for (input_ptr[0..scan_end]) |ch| {
        if (ch == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    output.error_line = line;
    output.error_col = col;

    const mlen: usize = @min(msg_len, 256);
    @memcpy(output.error_message[0..mlen], msg_buf[0..mlen]);
    output.error_message_len = @intCast(mlen);
}

/// Copy rule names into the ParseOutput rule name table.
/// Called once at the start of parsing to populate the rule name table.
export fn zgram_set_rule_name(
    output: *abi.ParseOutput,
    rule_id: u16,
    name_ptr: [*]const u8,
    name_len: u8,
) callconv(.c) void {
    if (rule_id >= abi.MAX_RULES) return;
    const nlen: usize = @min(name_len, abi.MAX_RULE_NAME);
    @memcpy(output.rule_names[rule_id].name[0..nlen], name_ptr[0..nlen]);
    output.rule_names[rule_id].name_len = @intCast(nlen);
    // Track highest rule_id set
    if (rule_id + 1 > output.rule_count) {
        output.rule_count = rule_id + 1;
    }
}
