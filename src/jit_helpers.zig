//! Exported helper functions called by JIT-compiled grammar code.
//!
//! The JIT-generated parser emits calls to these functions instead of
//! reimplementing memory management in LLVM IR. They have C calling
//! convention so LLJIT can resolve them via symbol lookup.
//!
//! The JIT code passes ParseOutput* directly (same pointer from zgram_parse).
//! None of these touch Python, so parsing can run without the GIL.

const std = @import("std");
const abi = @import("parse_abi.zig");

const allocator = std.heap.c_allocator;

/// Capacity used when the caller didn't preallocate a node buffer
const DEFAULT_NODE_CAPACITY: u32 = 256;
/// Maximum node capacity to prevent unbounded growth (256 MB worth of nodes)
pub const MAX_NODE_CAPACITY: u32 = 16 * 1024 * 1024;

/// Ensure the node array has capacity for at least `needed` nodes.
/// Returns 1 on success, 0 on failure.
pub export fn zgram_ensure_capacity(output: *abi.ParseOutput, needed: u32) callconv(.c) i32 {
    if (needed <= output.node_capacity) return 1;
    if (needed > MAX_NODE_CAPACITY) {
        output.error_kind = @intFromEnum(abi.ErrorKind.out_of_memory);
        return 0;
    }

    var new_cap: u32 = if (output.node_capacity == 0) DEFAULT_NODE_CAPACITY else output.node_capacity;
    while (new_cap < needed) {
        new_cap = @intCast(@min(@as(u64, new_cap) * 2, MAX_NODE_CAPACITY));
    }

    const old: []abi.FlatNode = if (output.nodes_ptr) |p| p[0..output.node_capacity] else &.{};
    const grown = allocator.realloc(old, new_cap) catch {
        output.error_kind = @intFromEnum(abi.ErrorKind.out_of_memory);
        return 0;
    };
    output.nodes_ptr = grown.ptr;
    output.node_capacity = new_cap;
    return 1;
}

/// Reserve a node slot. Returns the index, or -1 on failure.
pub export fn zgram_reserve_node(output: *abi.ParseOutput) callconv(.c) i32 {
    if (output.node_count >= MAX_NODE_CAPACITY) return -1;
    if (zgram_ensure_capacity(output, output.node_count + 1) == 0) return -1;
    const idx = output.node_count;
    output.node_count += 1;
    return @intCast(idx);
}

/// Fill a previously reserved node with its final data.
pub export fn zgram_fill_node(
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

fn setError(output: *abi.ParseOutput, kind: abi.ErrorKind, input_ptr: [*]const u8, input_len: usize, pos: usize, rule_id: u16) void {
    output.status = 0;
    // An allocation failure is the real cause; don't mask it with a syntax error
    if (output.error_kind == @intFromEnum(abi.ErrorKind.out_of_memory)) return;
    output.error_kind = @intFromEnum(kind);
    output.error_offset = @intCast(pos);
    output.error_rule_id = rule_id;
    const lc = abi.lineCol(input_ptr[0..input_len], pos);
    output.error_line = lc.line;
    output.error_col = lc.col;
}

/// The start rule matched but left input unconsumed.
pub export fn zgram_set_error_trailing(
    output: *abi.ParseOutput,
    input_ptr: [*]const u8,
    input_len: usize,
    pos: usize,
) callconv(.c) void {
    setError(output, .trailing_input, input_ptr, input_len, pos, 0);
}

/// The start rule failed: report the high-water mark (furthest position
/// reached) and the rule being attempted there.
pub export fn zgram_set_error_at_hwm(
    output: *abi.ParseOutput,
    input_ptr: [*]const u8,
    input_len: usize,
) callconv(.c) void {
    setError(output, .expected_rule, input_ptr, input_len, output.max_pos, output.max_pos_rule_id);
}

// ── Packrat memoization for @memo rules ──

/// Returned by zgram_memo_lookup when (rule, pos) isn't cached
pub const MEMO_MISS: i64 = -2;

const MemoEntry = struct {
    /// ((rule_id << 32) | pos) + 1; 0 = empty slot
    key: u64 = 0,
    /// Rule result: -1 for failure, else the position (packed with the child
    /// count for silent rules)
    result: i64 = 0,
    /// Nodes the rule produced, stored in the arena
    off: u32 = 0,
    count: u32 = 0,
};

pub const MemoState = struct {
    entries: []MemoEntry,
    used: usize = 0,
    /// Copies of the nodes produced by successful memoized calls: after
    /// backtracking the node buffer gets overwritten, so hits replay from here.
    arena: std.ArrayList(abi.FlatNode) = .empty,

    fn create() ?*MemoState {
        const state = allocator.create(MemoState) catch return null;
        const entries = allocator.alloc(MemoEntry, 256) catch {
            allocator.destroy(state);
            return null;
        };
        @memset(entries, .{});
        state.* = .{ .entries = entries };
        return state;
    }

    fn slot(entries: []MemoEntry, key: u64) *MemoEntry {
        const mask = entries.len - 1;
        var i: usize = @intCast((key *% 0x9E3779B97F4A7C15) >> 32 & mask);
        while (entries[i].key != 0 and entries[i].key != key) i = (i + 1) & mask;
        return &entries[i];
    }

    fn grow(self: *MemoState) bool {
        const bigger = allocator.alloc(MemoEntry, self.entries.len * 2) catch return false;
        @memset(bigger, .{});
        for (self.entries) |e| {
            if (e.key != 0) slot(bigger, e.key).* = e;
        }
        allocator.free(self.entries);
        self.entries = bigger;
        return true;
    }
};

inline fn memoKey(rule_id: u32, pos: u64) u64 {
    return ((@as(u64, rule_id) << 32) | (pos & 0xFFFFFFFF)) + 1;
}

/// Look up a memoized result. On a cached success, the rule's nodes are
/// appended to the node buffer (exactly as if the rule had run).
pub export fn zgram_memo_lookup(output: *abi.ParseOutput, rule_id: u32, pos: u64) callconv(.c) i64 {
    const state: *MemoState = @ptrCast(@alignCast(output.memo orelse return MEMO_MISS));
    const e = MemoState.slot(state.entries, memoKey(rule_id, pos));
    if (e.key == 0) return MEMO_MISS;
    if (e.result < 0 or e.count == 0) return e.result;

    // Replay the nodes; if that can't allocate, let the rule run normally
    if (zgram_ensure_capacity(output, output.node_count + e.count) == 0) {
        output.error_kind = 0;
        return MEMO_MISS;
    }
    const nodes = output.nodes_ptr.?;
    @memcpy(nodes[output.node_count..][0..e.count], state.arena.items[e.off..][0..e.count]);
    output.node_count += e.count;
    return e.result;
}

/// Record the result of a @memo rule called at `pos`; `node_start` is the
/// node count before the call. Failure to allocate just skips caching.
pub export fn zgram_memo_store(output: *abi.ParseOutput, rule_id: u32, pos: u64, result: i64, node_start: u32) callconv(.c) void {
    const state: *MemoState = if (output.memo) |m| @ptrCast(@alignCast(m)) else blk: {
        const created = MemoState.create() orelse return;
        output.memo = created;
        break :blk created;
    };
    if ((state.used + 1) * 10 >= state.entries.len * 7 and !state.grow()) return;

    var entry = MemoEntry{ .key = memoKey(rule_id, pos), .result = result };
    if (result >= 0 and output.node_count > node_start) {
        const produced = output.nodes_ptr.?[node_start..output.node_count];
        entry.off = @intCast(state.arena.items.len);
        state.arena.appendSlice(allocator, produced) catch return;
        entry.count = @intCast(produced.len);
    }
    const s = MemoState.slot(state.entries, entry.key);
    if (s.key == 0) state.used += 1;
    s.* = entry;
}

/// Free the memo table after a parse (no-op if no @memo rule ran).
pub fn memoFree(output: *abi.ParseOutput) void {
    const state: *MemoState = @ptrCast(@alignCast(output.memo orelse return));
    state.arena.deinit(allocator);
    allocator.free(state.entries);
    allocator.destroy(state);
    output.memo = null;
}
