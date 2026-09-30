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
        .meta = abi.FlatNode.packMeta(child_count, rule_id),
    };
}

/// Set the field id of every top-level node from index `from` to the end of
/// the node buffer (the nodes a labelled reference to a @silent rule produced).
pub export fn zgram_tag_field(output: *abi.ParseOutput, from: u32, field: u32) callconv(.c) void {
    const nodes = output.nodes_ptr orelse return;
    var i: u64 = from;
    while (i < output.node_count) : (i += @as(u64, nodes[i].subtree_size) + 1) {
        nodes[i].meta = (nodes[i].meta & ((1 << abi.FIELD_SHIFT) - 1)) | (field << abi.FIELD_SHIFT);
    }
}

// ── Chain folding for @left / @right / @postfix rules ──
//
// A folded rule `head (group)*` reserves no node. While it matches, its nodes
// sit at the top level from index `first`: the head's, then each repetition's.
// The generated code flags the first node of each repetition, and zgram_fold
// rearranges the lot into nested nodes when the rule succeeds.

/// Flag in subtree_size of the first node of a repetition (cleared by zgram_fold)
const FOLD_MARK: u32 = 1 << 31;

inline fn withField(meta: u32, field: u32) u32 {
    return (meta & ((1 << abi.FIELD_SHIFT) - 1)) | (field << abi.FIELD_SHIFT);
}

/// One repetition (or the head): the nodes from `from` up to the next marked
/// top-level node.
const Segment = struct {
    /// Index after the segment's last node
    end: u32,
    /// Number of top-level nodes
    count: u32,
    /// Index of the last top-level node
    last: u32,
};

fn segment(nodes: []const abi.FlatNode, from: u32) Segment {
    var seg = Segment{ .end = from, .count = 0, .last = from };
    while (seg.end < nodes.len) {
        seg.last = seg.end;
        seg.count += 1;
        seg.end += (nodes[seg.end].subtree_size & ~FOLD_MARK) + 1;
        if (seg.end < nodes.len and nodes[seg.end].subtree_size & FOLD_MARK != 0) break;
    }
    return seg;
}

/// Move `len` nodes from `src` down to `dst` (dst <= src; they may overlap).
inline fn moveDown(dst: [*]abi.FlatNode, src: [*]const abi.FlatNode, len: u32) void {
    if (dst != src and len != 0) @memmove(dst[0..len], src[0..len]);
}

/// Fold the nodes a @left/@right/@postfix rule (`kind`: grammar_parser.Fold)
/// produced from index `first` on into a single node. `k` is the number of
/// marked repetitions and `start`/`end` the rule's match. Returns 1, or 0 if
/// out of memory.
///
/// With head h and repetitions r1..rk, in pre-order:
///   left:    [Wk]..[W1] h r1 .. rk          Wi = rule node over (acc, ri)
///   postfix: the same, but a repetition that is one node S becomes Wi itself:
///            its header moves to the front and its children stay in place
///   right:   [W1] h pre1 [W2] last1 pre2 .. [Wk] last(k-1) prek lastk
///            where ri = (prei, lasti) and lasti is ri's last top-level node
/// The inner nodes take the head's label; the outermost has none.
///
/// Done in place: the nodes are first moved up by k slots, then each piece is
/// moved down to its final position, which is never above where it sits.
pub export fn zgram_fold(output: *abi.ParseOutput, first: u32, rule_id: u32, kind: u32, k: u32, start: u32, end: u32) callconv(.c) i32 {
    const count = output.node_count - first;
    const rid: u16 = @intCast(rule_id);
    const postfix = kind == 3;
    const right = kind == 2;

    // By far the most common fold: one repetition (`a + b`). Left and right
    // agree: one header over everything, which moves up one slot.
    if (k == 1 and !postfix) {
        if (output.node_count >= output.node_capacity and zgram_ensure_capacity(output, output.node_count + 1) == 0) return 0;
        const nodes = output.nodes_ptr.? + first;
        var children: u32 = 0;
        var i: u32 = 0;
        while (i < count) : (children += 1) {
            nodes[i].subtree_size &= ~FOLD_MARK;
            i += nodes[i].subtree_size + 1;
        }
        if (count <= 16) {
            i = count;
            while (i > 0) : (i -= 1) nodes[i] = nodes[i - 1];
        } else {
            @memmove(nodes[1 .. count + 1], nodes[0..count]);
        }
        nodes[0] = .{ .text_start = start, .text_end = end, .subtree_size = count, .meta = abi.FlatNode.packMeta(children, rid) };
        output.node_count += 1;
        return 1;
    }

    // The head, and the number of headers to add: one per repetition, except
    // those a @postfix rule turns into the wrapper themselves
    var head = Segment{ .end = 0, .count = 0, .last = 0 };
    var added: u32 = k;
    if (count > 0) {
        const in = output.nodes_ptr.?[first..][0..count];
        if (in[0].subtree_size & FOLD_MARK == 0) head = segment(in, 0);
        var at: u32 = head.end;
        while (postfix and at < count) {
            const seg = segment(in, at);
            if (seg.count == 1) added -= 1;
            at = seg.end;
        }
    }

    if (k == 0) {
        // No repetition: a single operand stands in for the rule
        if (head.count == 1) {
            const node = &output.nodes_ptr.?[first];
            node.meta = withField(node.meta, 0);
            return 1;
        }
        // Otherwise it's an ordinary node
        if (zgram_ensure_capacity(output, output.node_count + 1) == 0) return 0;
        const nodes = output.nodes_ptr.? + first;
        @memmove(nodes[1 .. count + 1], nodes[0..count]);
        nodes[0] = .{ .text_start = start, .text_end = end, .subtree_size = count, .meta = abi.FlatNode.packMeta(head.count, rid) };
        output.node_count += 1;
        return 1;
    }

    if (first + count + k > output.node_capacity and zgram_ensure_capacity(output, first + count + k) == 0) return 0;
    const out: [*]abi.FlatNode = output.nodes_ptr.? + first;
    if (count <= 32) {
        // Short chains (`a + b + c`): cheaper than a call to memmove
        var i: u32 = count;
        while (i > 0) : (i -= 1) out[i - 1 + k] = out[i - 1];
    } else {
        @memmove(out[k .. k + count], out[0..count]);
    }
    const in: []abi.FlatNode = out[k .. k + count];
    const total = count + added;
    const head_field: u32 = if (head.count > 0) in[0].field_id() else 0;

    if (!right) {
        // Headers Wk..W1 first, then the head (already in place), then each
        // repetition's content
        var o: u32 = k + head.end;
        var at: u32 = head.end;
        var i: u32 = 1;
        while (i <= k) : (i += 1) {
            const seg = segment(in, at);
            const left_count: u32 = if (i == 1) head.count else 1;
            var header: abi.FlatNode = undefined;
            if (postfix and seg.count == 1) {
                const s = in[at];
                const kids = if (s.child_count() == abi.CHILD_COUNT_MANY) abi.CHILD_COUNT_MANY else left_count + s.child_count();
                header = .{ .text_start = start, .text_end = s.text_end, .meta = abi.FlatNode.packMeta(kids, s.rule_id()) };
                moveDown(out + o, in.ptr + at + 1, seg.end - at - 1);
                o += seg.end - at - 1;
            } else {
                header = .{ .text_start = start, .text_end = in[seg.last].text_end, .meta = abi.FlatNode.packMeta(left_count + seg.count, rid) };
                moveDown(out + o, in.ptr + at, seg.end - at);
                out[o].subtree_size &= ~FOLD_MARK;
                o += seg.end - at;
            }
            header.subtree_size = o - (k - i) - 1;
            if (i == k) header.text_end = end else header.meta = withField(header.meta, head_field);
            out[k - i] = header;
            at = seg.end;
        }
    } else {
        moveDown(out + 1, in.ptr, head.end);
        var o: u32 = 1 + head.end;
        // The open wrapper Wi: its index in `out` and its header so far
        var w: u32 = 0;
        var header = abi.FlatNode{ .text_start = start, .text_end = end };
        var at: u32 = head.end;
        var i: u32 = 1;
        while (i <= k) : (i += 1) {
            const seg = segment(in, at);
            // Wi = (left, pre_i, last_i or W(i+1))
            header.subtree_size = total - w - 1;
            header.meta = withField(abi.FlatNode.packMeta((if (i == 1) head.count else 1) + seg.count, rid), header.meta >> abi.FIELD_SHIFT);
            const last = in[seg.last];
            out[w] = header;

            moveDown(out + o, in.ptr + at, seg.last - at);
            if (seg.last > at) out[o].subtree_size &= ~FOLD_MARK;
            o += seg.last - at;
            if (i < k) {
                // W(i+1) takes last_i's place and label; last_i becomes its left operand
                w = o;
                header = .{ .text_start = last.text_start, .text_end = end, .meta = withField(0, last.field_id()) };
                o += 1;
            }
            moveDown(out + o, in.ptr + seg.last, seg.end - seg.last);
            out[o].subtree_size &= ~FOLD_MARK;
            if (i < k) out[o].meta = withField(out[o].meta, head_field);
            o += seg.end - seg.last;
            at = seg.end;
        }
    }

    output.node_count = first + total;
    return 1;
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

/// The start rule matched but left input unconsumed. If some rule failed
/// beyond where the match stopped, that failure is why it stopped there
/// (`let x = ;` matching zero statements): report it rather than the end of
/// the match.
pub export fn zgram_set_error_trailing(
    output: *abi.ParseOutput,
    input_ptr: [*]const u8,
    input_len: usize,
    pos: usize,
) callconv(.c) void {
    if (output.max_pos > pos) return zgram_set_error_at_hwm(output, input_ptr, input_len);
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
