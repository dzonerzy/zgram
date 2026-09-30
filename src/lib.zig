const std = @import("std");
const builtin = @import("builtin");
const pyoz = @import("PyOZ");
const py = pyoz.py;
const abi = @import("parse_abi.zig");
const grammar_parser = @import("grammar_parser.zig");
const jit_codegen = @import("jit_codegen.zig");
const jit_compiler = @import("jit_compiler.zig");
const jit_helpers = @import("jit_helpers.zig");

const allocator = std.heap.c_allocator;

// Force-export JIT helpers so LLJIT can resolve them
comptime {
    _ = @import("jit_helpers.zig");
}

// ============================================================================
// ParseError class — error info when parsing fails
// ============================================================================

const ParseError = struct {
    _message: [256]u8 = [_]u8{0} ** 256,
    _message_len: usize = 0,
    _offset: i64 = 0,
    _line: i64 = 0,
    _column: i64 = 0,

    pub fn message(self: *const ParseError) []const u8 {
        return self._message[0..self._message_len];
    }

    pub fn offset(self: *const ParseError) i64 {
        return self._offset;
    }

    pub fn line(self: *const ParseError) i64 {
        return self._line;
    }

    pub fn column(self: *const ParseError) i64 {
        return self._column;
    }

    pub fn __str__(self: *const ParseError, buf: []u8) []const u8 {
        // "line 3, col 5: unexpected input after match"
        var pos: usize = 0;
        pos += copySlice(buf, pos, "line ");
        pos += fmtInt(buf, pos, self._line);
        pos += copySlice(buf, pos, ", col ");
        pos += fmtInt(buf, pos, self._column);
        pos += copySlice(buf, pos, ": ");
        pos += copySlice(buf, pos, self._message[0..self._message_len]);
        return buf[0..pos];
    }

    pub fn __repr__(self: *const ParseError, buf: []u8) []const u8 {
        // "ParseError('message', line=3, col=5)"
        var pos: usize = 0;
        pos += copySlice(buf, pos, "ParseErrorInfo('");
        pos += copySlice(buf, pos, self._message[0..self._message_len]);
        pos += copySlice(buf, pos, "', line=");
        pos += fmtInt(buf, pos, self._line);
        pos += copySlice(buf, pos, ", col=");
        pos += fmtInt(buf, pos, self._column);
        pos += copySlice(buf, pos, ")");
        return buf[0..pos];
    }

    pub const __doc__: [*:0]const u8 = "Details of a failed parse (parser.error): message, line, column, offset.";
};

// ============================================================================
// Compiled grammars — JIT code shared by every parser compiled from the same text
// ============================================================================

/// A JIT-compiled grammar. Reference-counted: each GrammarParser holds one
/// reference and the recent-compiles cache holds one. Contains no Python
/// objects, so it can be built and freed without the GIL.
const Compiled = struct {
    parse_fn: abi.ParseFn,
    resource: jit_compiler.ResourceHandle,
    /// Grammar text (cache key)
    text: []u8,
    hash: u64,
    /// Rule names by rule id, slices into name_buf
    rule_names: [][]const u8,
    name_buf: []u8,
    refs: std.atomic.Value(u32) = .init(1),

    /// Accept/reject-only parser, compiled on first use (see validator())
    validate_fn: std.atomic.Value(?abi.ParseFn) = .init(null),
    validate_resource: jit_compiler.ResourceHandle = null,
    validate_lock: std.atomic.Value(bool) = .init(false),

    fn build(grammar_text: []const u8) !*Compiled {
        // Grammar IR and codegen scratch live only for this call
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const alloc = arena.allocator();

        const grammar = try grammar_parser.parseGrammar(alloc, grammar_text);

        const self = try allocator.create(Compiled);
        errdefer allocator.destroy(self);
        const text = try allocator.dupe(u8, grammar_text);
        errdefer allocator.free(text);

        var total: usize = 0;
        for (grammar.rules) |r| total += r.name.len;
        const name_buf = try allocator.alloc(u8, total);
        errdefer allocator.free(name_buf);
        const rule_names = try allocator.alloc([]const u8, grammar.rules.len);
        errdefer allocator.free(rule_names);
        var off: usize = 0;
        for (grammar.rules, 0..) |r, i| {
            @memcpy(name_buf[off..][0..r.name.len], r.name);
            rule_names[i] = name_buf[off..][0..r.name.len];
            off += r.name.len;
        }

        const module = jit_codegen.generateModule(alloc, grammar, .tree) catch return error.CompilationFailed;
        const jit = jit_compiler.jitCompile(module.module, module.context) catch return error.CompilationFailed;

        self.* = .{
            .parse_fn = jit.parse_fn,
            .resource = jit.resource,
            .text = text,
            .hash = std.hash.Wyhash.hash(0, grammar_text),
            .rule_names = rule_names,
            .name_buf = name_buf,
        };
        return self;
    }

    fn retain(self: *Compiled) void {
        _ = self.refs.fetchAdd(1, .monotonic);
    }

    /// The validator for this grammar, compiled on first call. Doesn't touch
    /// Python, so it runs with the GIL released.
    fn validator(self: *Compiled) !abi.ParseFn {
        if (self.validate_fn.load(.acquire)) |f| return f;
        while (self.validate_lock.cmpxchgWeak(false, true, .acquire, .monotonic) != null) {
            std.Thread.yield() catch {};
        }
        defer self.validate_lock.store(false, .release);
        if (self.validate_fn.load(.acquire)) |f| return f;

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const grammar = try grammar_parser.parseGrammar(arena.allocator(), self.text);
        const module = jit_codegen.generateModule(arena.allocator(), grammar, .validate) catch return error.CompilationFailed;
        const jit = jit_compiler.jitCompile(module.module, module.context) catch return error.CompilationFailed;
        self.validate_resource = jit.resource;
        self.validate_fn.store(jit.parse_fn, .release);
        return jit.parse_fn;
    }

    fn release(self: *Compiled) void {
        if (self.refs.fetchSub(1, .acq_rel) != 1) return;
        if (self.validate_resource != null) jit_compiler.releaseGrammar(self.validate_resource);
        jit_compiler.releaseGrammar(self.resource);
        allocator.free(self.rule_names);
        allocator.free(self.name_buf);
        allocator.free(self.text);
        allocator.destroy(self);
    }

    fn ruleId(self: *const Compiled, name: []const u8) ?u32 {
        for (self.rule_names, 0..) |n, i| {
            if (std.mem.eql(u8, n, name)) return @intCast(i);
        }
        return null;
    }
};

/// Recently compiled grammars, so compiling the same grammar again (in a
/// loop, per request, ...) returns immediately and doesn't grow LLVM's JIT.
const cache = struct {
    const capacity = 16;
    /// Most recent first; each entry holds a reference
    var entries: [capacity]?*Compiled = @splat(null);
    var lock: std.atomic.Value(bool) = .init(false);

    fn acquire() void {
        while (lock.cmpxchgWeak(false, true, .acquire, .monotonic) != null) {
            std.Thread.yield() catch {};
        }
    }

    fn releaseLock() void {
        lock.store(false, .release);
    }

    /// Return a new reference to the cached grammar for `text`, if any.
    fn get(text: []const u8) ?*Compiled {
        const h = std.hash.Wyhash.hash(0, text);
        acquire();
        defer releaseLock();
        for (&entries, 0..) |*slot, i| {
            const c = slot.* orelse continue;
            if (c.hash == h and std.mem.eql(u8, c.text, text)) {
                // Move to front
                std.mem.copyBackwards(?*Compiled, entries[1 .. i + 1], entries[0..i]);
                entries[0] = c;
                c.retain();
                return c;
            }
        }
        return null;
    }

    /// Insert `c` (the caller keeps its own reference). If another thread
    /// cached the same grammar meanwhile, returns that one instead and
    /// releases `c`.
    fn put(c: *Compiled) *Compiled {
        var evicted: ?*Compiled = null;
        const result = blk: {
            acquire();
            defer releaseLock();
            for (entries) |slot| {
                const e = slot orelse continue;
                if (e.hash == c.hash and std.mem.eql(u8, e.text, c.text)) {
                    e.retain();
                    break :blk e;
                }
            }
            evicted = entries[capacity - 1];
            std.mem.copyBackwards(?*Compiled, entries[1..], entries[0 .. capacity - 1]);
            c.retain();
            entries[0] = c;
            break :blk c;
        };
        if (evicted) |e| e.release();
        if (result != c) c.release();
        return result;
    }

    fn clear() void {
        var old: [capacity]?*Compiled = undefined;
        {
            acquire();
            defer releaseLock();
            old = entries;
            entries = @splat(null);
        }
        for (old) |slot| if (slot) |c| c.release();
    }
};

/// Compile (or fetch from the cache) without touching Python: safe with the
/// GIL released and on async worker threads.
fn compileCompiled(grammar_text: []const u8) !*Compiled {
    if (cache.get(grammar_text)) |c| return c;
    const c = try Compiled.build(grammar_text);
    return cache.put(c);
}

// ============================================================================
// Rule table — interned rule-name strings for one GrammarParser
// ============================================================================

/// Rule names indexed by rule id, as interned Python strings (returned by
/// Node.rule() without allocating) plus their UTF-8 bytes. Created under the
/// GIL on a parser's first parse.
const RuleTable = struct {
    names: []*pyoz.PyObject,
    bytes: []const []const u8,

    fn create(rule_names: []const []const u8) !*RuleTable {
        const table = try allocator.create(RuleTable);
        errdefer allocator.destroy(table);
        const names = try allocator.alloc(*pyoz.PyObject, rule_names.len);
        errdefer allocator.free(names);
        for (rule_names, 0..) |name, i| {
            var s: ?*pyoz.PyObject = py.PyUnicode_FromStringAndSize(name.ptr, @intCast(name.len));
            if (s == null) {
                for (names[0..i]) |n| py.Py_DecRef(n);
                return error.AllocationFailed;
            }
            py.c.PyUnicode_InternInPlace(@ptrCast(&s));
            names[i] = s.?;
        }
        table.* = .{ .names = names, .bytes = rule_names };
        return table;
    }

    fn destroy(self: *RuleTable) void {
        for (self.names) |n| py.Py_DecRef(n);
        allocator.free(self.names);
        allocator.destroy(self);
    }

    fn idOf(self: *const RuleTable, name: []const u8) ?u16 {
        for (self.bytes, 0..) |b, i| {
            if (std.mem.eql(u8, b, name)) return @intCast(i);
        }
        return null;
    }
};

// ============================================================================
// _Tree class — owns the result of one parse
// ============================================================================

/// Storage shared by all Nodes of one parse: a private copy of the flat node
/// array and a reference to the input object. Nodes hold a Ref to it, so they
/// stay valid across later parse() calls and after the caller drops the input.
const Tree = struct {
    /// Keeps the parser (and its rule table) alive while any Node exists
    _parser: pyoz.Ref(GrammarParser) = .{},
    _nodes: ?[*]abi.FlatNode = null,
    _count: u32 = 0,
    /// Allocated length of _nodes (>= _count)
    _alloc_len: u32 = 0,
    /// Strong reference to the input str/bytes; _input_ptr points into it
    _input_obj: ?*pyoz.PyObject = null,
    _input_ptr: ?[*]const u8 = null,
    _input_len: usize = 0,
    _rules: ?*const RuleTable = null,
    /// Last child lookup, so indexing children in order (n[0], n[1], ...)
    /// resumes from the previous sibling instead of the first one
    _cache_parent: u32 = std.math.maxInt(u32),
    _cache_pos: u32 = 0,
    _cache_idx: u32 = 0,

    pub fn __del__(self: *Tree) void {
        if (self._nodes) |nodes| allocator.free(nodes[0..self._alloc_len]);
        self._nodes = null;
        if (self._input_obj) |obj| py.Py_DecRef(obj);
        self._input_obj = null;
    }

    fn flat(self: *const Tree, idx: u32) ?abi.FlatNode {
        const nodes = self._nodes orelse return null;
        if (idx >= self._count) return null;
        return nodes[idx];
    }

    /// Flat index of the node after `idx`'s subtree (its next sibling).
    fn skip(self: *const Tree, idx: u32) u32 {
        const next = @as(u64, idx) + @as(u64, self._nodes.?[idx].subtree_size) + 1;
        return @intCast(@min(next, self._count));
    }

    fn ruleName(self: *const Tree, f: abi.FlatNode) []const u8 {
        const rules = self._rules orelse return "";
        const rid = f.rule_id();
        return if (rid < rules.bytes.len) rules.bytes[rid] else "";
    }

    pub const __doc__: [*:0]const u8 = "Internal storage shared by the Nodes of one parse.";
};

fn makeNode(tree: *Tree, idx: u32) Node {
    var node = Node{ ._t = tree, ._idx = idx };
    node._tree.set(Module.selfObject(Tree, tree));
    return node;
}

// ============================================================================
// Node class — a node in the parse tree
// ============================================================================

const Node = struct {
    /// Strong reference to the tree this node belongs to
    _tree: pyoz.Ref(Tree) = .{},
    /// Data pointer of _tree (kept alive by the Ref)
    _t: ?*Tree = null,
    /// Index into the tree's flat node array
    _idx: u32 = 0,

    fn flat(self: *const Node) ?abi.FlatNode {
        const t = self._t orelse return null;
        return t.flat(self._idx);
    }

    pub fn rule(self: *const Node) pyoz.Signature(?*pyoz.PyObject, "str") {
        if (self._t) |t| {
            if (t._rules) |rules| {
                if (self.flat()) |f| {
                    const rid = f.rule_id();
                    if (rid < rules.names.len) {
                        py.Py_IncRef(rules.names[rid]);
                        return .{ .value = rules.names[rid] };
                    }
                }
            }
        }
        return .{ .value = py.PyUnicode_FromStringAndSize("", 0) };
    }

    pub fn text(self: *const Node) []const u8 {
        // Zero-copy: slice directly from the input object's UTF-8 data
        const t = self._t orelse return "";
        const inp = t._input_ptr orelse return "";
        const f = self.flat() orelse return "";
        if (f.text_start <= f.text_end and f.text_end <= t._input_len) {
            return inp[f.text_start..f.text_end];
        }
        return "";
    }

    pub fn start(self: *const Node) i64 {
        const f = self.flat() orelse return 0;
        return f.text_start;
    }

    pub fn end(self: *const Node) i64 {
        const f = self.flat() orelse return 0;
        return f.text_end;
    }

    pub fn span(self: *const Node) struct { i64, i64 } {
        return .{ self.start(), self.end() };
    }

    pub fn child_count(self: *const Node) i64 {
        const f = self.flat() orelse return 0;
        return f.child_count();
    }

    /// Flat index of the child at `index`. Children are found by skipping
    /// sibling subtrees; sequential lookups resume from the previous one.
    fn childIndex(self: *const Node, index: u32) ?u32 {
        const t = self._t orelse return null;
        const f = self.flat() orelse return null;
        if (index >= f.child_count()) return null;

        var pos: u32 = 0;
        var ci: u32 = self._idx + 1;
        if (t._cache_parent == self._idx and t._cache_pos <= index) {
            pos = t._cache_pos;
            ci = t._cache_idx;
        }
        while (pos < index) : (pos += 1) {
            if (ci >= t._count) return null;
            ci = t.skip(ci);
        }
        if (ci >= t._count) return null;

        t._cache_parent = self._idx;
        t._cache_pos = index;
        t._cache_idx = ci;
        return ci;
    }

    /// Get child node at index, or None if out of bounds.
    pub fn child(self: *const Node, index: i64) ?Node {
        if (index < 0 or index > std.math.maxInt(u32)) return null;
        const ci = self.childIndex(@intCast(index)) orelse return null;
        return makeNode(self._t.?, ci);
    }

    // ── Sequence protocol ──

    pub fn __len__(self: *const Node) i64 {
        return self.child_count();
    }

    pub fn __getitem__(self: *const Node, index: i64) !Node {
        const cc = self.child_count();
        var idx = index;
        if (idx < 0) idx += cc;
        if (idx < 0 or idx >= cc) return error.IndexOutOfBounds;
        return self.child(idx) orelse return error.IndexOutOfBounds;
    }

    // ── Iterator protocol ──

    pub fn __iter__(self: *const Node) NodeIter {
        var it = NodeIter{};
        const t = self._t orelse return it;
        const f = self.flat() orelse return it;
        it._t = t;
        it._tree.set(Module.selfObject(Tree, t));
        it._next = self._idx + 1;
        it._remaining = f.child_count();
        return it;
    }

    // ── String representations ──

    pub fn __str__(self: *const Node) []const u8 {
        return self.text();
    }

    pub fn __repr__(self: *const Node, buf: []u8) []const u8 {
        // Format: Node('rule', start..end, N children)
        const r = if (self._t) |t| (if (self.flat()) |f| t.ruleName(f) else "") else "";

        var pos: usize = 0;
        pos += copySlice(buf, pos, "Node('");
        pos += copySlice(buf, pos, r);
        pos += copySlice(buf, pos, "', ");
        pos += fmtInt(buf, pos, self.start());
        pos += copySlice(buf, pos, "..");
        pos += fmtInt(buf, pos, self.end());
        pos += copySlice(buf, pos, ", ");
        pos += fmtInt(buf, pos, self.child_count());
        pos += copySlice(buf, pos, " children)");

        return buf[0..pos];
    }

    // ── Boolean / equality ──

    pub fn __bool__(self: *const Node) bool {
        _ = self;
        return true;
    }

    pub fn __eq__(self: *const Node, other: *const Node) bool {
        return self._t == other._t and self._idx == other._idx;
    }

    // ── Tree navigation ──

    /// Return all children as a list.
    pub fn children(self: *const Node) pyoz.Signature(?*pyoz.PyObject, "list[Node]") {
        const f = self.flat() orelse return .{ .value = py.c.PyList_New(0) };
        const t = self._t.?;
        const list = py.c.PyList_New(f.child_count()) orelse return .{ .value = null };
        var ci: u32 = self._idx + 1;
        for (0..f.child_count()) |i| {
            if (ci >= t._count) {
                py.Py_DecRef(list);
                py.PyErr_SetString(py.PyExc_RuntimeError(), "corrupt parse tree");
                return .{ .value = null };
            }
            const obj = Module.toPy(Node, makeNode(t, ci)) orelse {
                py.Py_DecRef(list);
                return .{ .value = null };
            };
            _ = py.c.PyList_SetItem(list, @intCast(i), obj);
            ci = t.skip(ci);
        }
        return .{ .value = list };
    }

    /// Search this node and its descendants for nodes matching a rule name.
    pub fn find(self: *const Node, rule_name: []const u8) pyoz.Signature(?*pyoz.PyObject, "list[Node]") {
        const t = self._t orelse return .{ .value = py.c.PyList_New(0) };
        const table = t._rules orelse return .{ .value = py.c.PyList_New(0) };
        const f = self.flat() orelse return .{ .value = py.c.PyList_New(0) };
        const rid = table.idOf(rule_name) orelse return .{ .value = py.c.PyList_New(0) };

        // Descendants are contiguous in the pre-order array: [idx, idx + subtree_size]
        const nodes = t._nodes.?;
        const last: u32 = @intCast(@min(@as(u64, self._idx) + f.subtree_size, t._count - 1));
        var matches: usize = 0;
        for (nodes[self._idx .. last + 1]) |n| {
            if (n.rule_id() == rid) matches += 1;
        }
        const list = py.c.PyList_New(@intCast(matches)) orelse return .{ .value = null };
        var k: usize = 0;
        var i: u32 = self._idx;
        while (i <= last) : (i += 1) {
            if (nodes[i].rule_id() != rid) continue;
            const obj = Module.toPy(Node, makeNode(t, i)) orelse {
                py.Py_DecRef(list);
                return .{ .value = null };
            };
            _ = py.c.PyList_SetItem(list, @intCast(k), obj);
            k += 1;
        }
        return .{ .value = list };
    }

    /// Convert this subtree to nested tuples in one native pass:
    /// (rule, text, children) or, with spans=True, (rule, start, end, text, children).
    pub fn to_tuple(self: *const Node, args: pyoz.Args(struct { spans: bool = false })) pyoz.Signature(?*pyoz.PyObject, "tuple") {
        const t = self._t orelse return .{ .value = py.c.PyTuple_New(0) };
        const f = self.flat() orelse return .{ .value = py.c.PyTuple_New(0) };
        const table = t._rules orelse return .{ .value = py.c.PyTuple_New(0) };
        const nodes = t._nodes.?;
        const inp = t._input_ptr orelse return .{ .value = py.c.PyTuple_New(0) };
        const spans = args.value.spans;

        // Visit the subtree in reverse pre-order: when a node is reached, its
        // children's tuples are the top entries of the stack, first child on top.
        var stack: std.ArrayList(*pyoz.PyObject) = .empty;
        defer {
            for (stack.items) |o| py.Py_DecRef(o);
            stack.deinit(allocator);
        }
        stack.ensureTotalCapacity(allocator, 64) catch return .{ .value = null };

        const first = self._idx;
        var i: u32 = @intCast(@min(@as(u64, first) + f.subtree_size, t._count - 1));
        while (true) : (i -= 1) {
            const n = nodes[i];
            const cc = n.child_count();
            if (stack.items.len < cc) return .{ .value = null };

            const kids = py.c.PyTuple_New(cc) orelse return .{ .value = null };
            for (0..cc) |k| {
                // PyTuple_SetItem steals the reference
                _ = py.c.PyTuple_SetItem(kids, @intCast(k), stack.pop().?);
            }

            const rid = n.rule_id();
            const rule_obj: *pyoz.PyObject = if (rid < table.names.len) table.names[rid] else {
                py.Py_DecRef(kids);
                return .{ .value = null };
            };
            const s = @min(n.text_start, t._input_len);
            const e = @min(@max(n.text_end, s), t._input_len);
            const text_obj = py.PyUnicode_FromStringAndSize(inp + s, @intCast(e - s)) orelse {
                py.Py_DecRef(kids);
                return .{ .value = null };
            };

            const item = py.c.PyTuple_New(if (spans) 5 else 3) orelse {
                py.Py_DecRef(kids);
                py.Py_DecRef(text_obj);
                return .{ .value = null };
            };
            py.Py_IncRef(rule_obj);
            _ = py.c.PyTuple_SetItem(item, 0, rule_obj);
            if (spans) {
                _ = py.c.PyTuple_SetItem(item, 1, py.c.PyLong_FromUnsignedLong(n.text_start));
                _ = py.c.PyTuple_SetItem(item, 2, py.c.PyLong_FromUnsignedLong(n.text_end));
                _ = py.c.PyTuple_SetItem(item, 3, text_obj);
                _ = py.c.PyTuple_SetItem(item, 4, kids);
            } else {
                _ = py.c.PyTuple_SetItem(item, 1, text_obj);
                _ = py.c.PyTuple_SetItem(item, 2, kids);
            }
            stack.append(allocator, item) catch {
                py.Py_DecRef(item);
                return .{ .value = null };
            };
            if (i == first) break;
        }
        if (stack.items.len != 1) return .{ .value = null };
        return .{ .value = stack.pop().? };
    }

    // ── Docstrings ──

    pub const __doc__: [*:0]const u8 = "A node in the parse tree. Supports iteration, indexing, and tree navigation.";
    pub const rule__doc__: [*:0]const u8 = "Return the grammar rule name that matched this node.";
    pub const text__doc__: [*:0]const u8 = "Return the matched text (zero-copy slice from input).";
    pub const child__doc__: [*:0]const u8 = "Get child node at index, or None if out of bounds.";
    pub const child__params__ = "index";
    pub const children__doc__: [*:0]const u8 = "Return all children as a list of Node.";
    pub const find__doc__: [*:0]const u8 = "Search this node and its descendants for nodes matching a rule name. Returns a list.";
    pub const find__params__ = "rule_name";
    pub const child_count__doc__: [*:0]const u8 = "Return the number of child nodes.";
    pub const to_tuple__doc__: [*:0]const u8 = "Convert this subtree to nested tuples in one native pass: (rule, text, children), or (rule, start, end, text, children) with spans=True.";

    // ── Freelist for fast allocation ──

    pub const __freelist__: usize = 64;
};

/// Iterator over a node's direct children (each `iter(node)` gets its own)
const NodeIter = struct {
    _tree: pyoz.Ref(Tree) = .{},
    _t: ?*Tree = null,
    /// Flat index of the next child
    _next: u32 = 0,
    _remaining: u32 = 0,

    pub fn __iter__(self: *NodeIter) *NodeIter {
        return self;
    }

    pub fn __next__(self: *NodeIter) ?Node {
        if (self._remaining == 0) return null;
        const t = self._t orelse return null;
        if (self._next >= t._count) return null;
        const idx = self._next;
        self._remaining -= 1;
        self._next = t.skip(idx);
        return makeNode(t, idx);
    }

    pub const __doc__: [*:0]const u8 = "Iterator over the direct children of a Node.";
    pub const __freelist__: usize = 16;
};

// ── Formatting helpers (no allocator needed) ──

fn copySlice(buf: []u8, pos: usize, src: []const u8) usize {
    const avail = buf.len - pos;
    const n = @min(src.len, avail);
    @memcpy(buf[pos..][0..n], src[0..n]);
    return n;
}

fn fmtInt(buf: []u8, pos: usize, val: i64) usize {
    var tmp: [20]u8 = undefined;
    var v: u64 = if (val < 0) @intCast(-val) else @intCast(val);
    var len: usize = 0;

    if (v == 0) {
        tmp[0] = '0';
        len = 1;
    } else {
        while (v > 0) : (len += 1) {
            tmp[len] = @intCast('0' + (v % 10));
            v /= 10;
        }
        // Reverse
        var i: usize = 0;
        var j: usize = len - 1;
        while (i < j) {
            const t = tmp[i];
            tmp[i] = tmp[j];
            tmp[j] = t;
            i += 1;
            j -= 1;
        }
    }

    const start: usize = if (val < 0) blk: {
        if (pos < buf.len) buf[pos] = '-';
        break :blk 1;
    } else 0;

    return start + copySlice(buf, pos + start, tmp[0..len]);
}

// ============================================================================
// GrammarParser class — wraps a JIT-compiled grammar
// ============================================================================

/// Inputs at least this large are parsed with the GIL released, so other
/// threads can run (or parse) meanwhile. Smaller parses finish faster than a
/// GIL round trip.
const GIL_RELEASE_BYTES = 16 * 1024;

/// Details of the last failed parse
const LastError = struct {
    kind: abi.ErrorKind = .none,
    offset: u32 = 0,
    line: u32 = 0,
    col: u32 = 0,
    rule_id: u16 = 0,
};

const GrammarParser = struct {
    /// Shared JIT-compiled grammar (one reference)
    _compiled: ?*Compiled = null,
    /// Interned rule names, created on first parse (needs the GIL)
    _rules: ?*RuleTable = null,
    _last_error: LastError = .{},

    pub fn __del__(self: *GrammarParser) void {
        if (self._rules) |table| table.destroy();
        self._rules = null;
        if (self._compiled) |c| c.release();
        self._compiled = null;
    }

    fn ruleTable(self: *GrammarParser) !*RuleTable {
        if (self._rules) |r| return r;
        const compiled = self._compiled orelse return error.ParserNotLoaded;
        self._rules = try RuleTable.create(compiled.rule_names);
        return self._rules.?;
    }

    /// Build the "line L, col C: ..." message for an error.
    fn formatError(self: *const GrammarParser, err: LastError, buf: []u8) []const u8 {
        var pos: usize = 0;
        pos += copySlice(buf, pos, "line ");
        pos += fmtInt(buf, pos, err.line);
        pos += copySlice(buf, pos, ", col ");
        pos += fmtInt(buf, pos, err.col);
        pos += copySlice(buf, pos, ": ");
        pos += copySlice(buf, pos, errorMessage(self, err, buf[pos..]));
        return buf[0..pos];
    }

    fn errorMessage(self: *const GrammarParser, err: LastError, scratch: []u8) []const u8 {
        return switch (err.kind) {
            .none => "",
            .trailing_input => "unexpected input after match",
            .out_of_memory => "out of memory (parse tree too large)",
            .expected_rule => blk: {
                const compiled = self._compiled orelse break :blk "unexpected input";
                if (err.rule_id >= compiled.rule_names.len) break :blk "unexpected input";
                // Built in place at the start of scratch, then copied by the caller
                var tmp: [abi.MAX_RULE_NAME + 16]u8 = undefined;
                const name = compiled.rule_names[err.rule_id];
                const msg = std.fmt.bufPrint(&tmp, "expected {s}", .{name}) catch break :blk "unexpected input";
                const n = @min(msg.len, scratch.len);
                @memcpy(scratch[0..n], msg[0..n]);
                break :blk scratch[0..n];
            },
        };
    }

    /// Run the compiled parser. On success returns the root Node; on failure
    /// records the error and returns null (raising only if `raise_on_fail`).
    fn run(self: *GrammarParser, input: *pyoz.PyObject, start: ?*pyoz.PyObject, flags: u32, raise_on_fail: bool) !?Node {
        const compiled = self._compiled orelse return error.ParserNotLoaded;
        const table = try self.ruleTable();

        // Parse the str/bytes object's own UTF-8 buffer in place: the Tree
        // keeps a reference to the object, so the buffer outlives the Nodes.
        var len: py.Py_ssize_t = 0;
        const ptr: [*]const u8 = blk: {
            if (py.PyUnicode_Check(input)) {
                break :blk py.c.PyUnicode_AsUTF8AndSize(input, &len) orelse return null;
            }
            if (py.PyBytes_Check(input)) {
                var p: [*]u8 = undefined;
                if (py.PyBytes_AsStringAndSize(input, &p, &len) < 0) return null;
                break :blk p;
            }
            return raise(py.PyExc_TypeError(), "parse input must be str or bytes");
        };
        const input_len: usize = @intCast(len);
        // FlatNode stores positions as u32 — reject inputs that would overflow
        if (input_len > std.math.maxInt(u32)) return error.InputTooLarge;

        var start_rule: u32 = 0;
        if (start) |s| {
            if (s != py.Py_None()) {
                if (!py.PyUnicode_Check(s)) return raise(py.PyExc_TypeError(), "start must be a rule name (str)");
                var slen: py.Py_ssize_t = 0;
                const sptr = py.c.PyUnicode_AsUTF8AndSize(s, &slen) orelse return null;
                start_rule = compiled.ruleId(sptr[0..@intCast(slen)]) orelse
                    return raise(py.PyExc_ValueError(), "unknown start rule");
            }
        }

        // Size the node buffer from the input (JSON averages ~1 node per 4 bytes);
        // the JIT grows it if needed.
        var output = abi.ParseOutput{};
        defer jit_helpers.memoFree(&output);
        const initial_cap: u32 = @intCast(std.math.clamp(input_len / 4 + 16, 16, 64 * 1024));
        const initial = try allocator.alloc(abi.FlatNode, initial_cap);
        output.nodes_ptr = initial.ptr;
        output.node_capacity = initial_cap;
        errdefer if (output.nodes_ptr) |n| allocator.free(n[0..output.node_capacity]);

        const rc = if (input_len >= GIL_RELEASE_BYTES)
            pyoz.allowThreads(callParse, .{ compiled.parse_fn, ptr, input_len, &output, start_rule, flags })
        else
            compiled.parse_fn(ptr, input_len, &output, start_rule, flags);
        if (rc < 0) {
            allocator.free(output.nodes_ptr.?[0..output.node_capacity]);
            output.nodes_ptr = null;
            return raise(py.PyExc_ValueError(), "unknown start rule");
        }

        if (output.status == 1) {
            self._last_error = .{};
            if (output.node_count == 0) {
                // A @silent start rule that matched without producing nodes
                allocator.free(output.nodes_ptr.?[0..output.node_capacity]);
                output.nodes_ptr = null;
                return raise(py.PyExc_ValueError(), "the start rule matched but produced no nodes (is it @silent?)");
            }
            // Hand the node buffer to the tree, trimmed if it's mostly unused
            var nodes = output.nodes_ptr.?[0..output.node_capacity];
            if (output.node_count < output.node_capacity / 2) {
                nodes = allocator.realloc(nodes, output.node_count) catch nodes;
            }
            output.nodes_ptr = null;

            var tree = Tree{
                ._nodes = nodes.ptr,
                ._count = output.node_count,
                ._alloc_len = @intCast(nodes.len),
                ._input_obj = input,
                ._input_ptr = ptr,
                ._input_len = input_len,
                ._rules = table,
            };
            py.Py_IncRef(input);
            tree._parser.set(Module.selfObject(GrammarParser, self));

            const tree_obj = Module.toPy(Tree, tree) orelse {
                tree._parser.clear();
                tree.__del__();
                return error.AllocationFailed;
            };
            // The root Node takes its own reference to the tree
            defer py.Py_DecRef(tree_obj);
            const t = Module.fromPy(*Tree, tree_obj) catch return error.AllocationFailed;
            return makeNode(t, 0);
        }

        allocator.free(output.nodes_ptr.?[0..output.node_capacity]);
        output.nodes_ptr = null;
        self._last_error = .{
            .kind = @enumFromInt(output.error_kind),
            .offset = output.error_offset,
            .line = output.error_line,
            .col = output.error_col,
            .rule_id = output.error_rule_id,
        };
        if (self._last_error.kind == .out_of_memory) return error.OutOfMemory;
        if (!raise_on_fail) return null;

        var msg_buf: [320]u8 = undefined;
        const msg = self.formatError(self._last_error, msg_buf[0 .. msg_buf.len - 1]);
        msg_buf[msg.len] = 0;
        Module.getException(0).raise(msg_buf[0..msg.len :0]);
        self.annotateException();
        return null;
    }

    /// Attach line/column/offset/message attributes to the ParseError being raised.
    fn annotateException(self: *const GrammarParser) void {
        var t: ?*pyoz.PyObject = null;
        var v: ?*pyoz.PyObject = null;
        var tb: ?*pyoz.PyObject = null;
        py.c.PyErr_Fetch(@ptrCast(&t), @ptrCast(&v), @ptrCast(&tb));
        py.c.PyErr_NormalizeException(@ptrCast(&t), @ptrCast(&v), @ptrCast(&tb));
        if (v) |exc| {
            const err = self._last_error;
            var scratch: [abi.MAX_RULE_NAME + 32]u8 = undefined;
            const message = self.errorMessage(err, &scratch);
            const attrs = [_]struct { [*:0]const u8, ?*pyoz.PyObject }{
                .{ "line", py.c.PyLong_FromUnsignedLong(err.line) },
                .{ "column", py.c.PyLong_FromUnsignedLong(err.col) },
                .{ "offset", py.c.PyLong_FromUnsignedLong(err.offset) },
                .{ "message", py.PyUnicode_FromStringAndSize(message.ptr, @intCast(message.len)) },
            };
            for (attrs) |a| {
                if (a[1]) |val| {
                    _ = py.c.PyObject_SetAttrString(exc, a[0], val);
                    py.Py_DecRef(val);
                }
            }
        }
        py.c.PyErr_Restore(t, v, tb);
    }

    fn raise(exc: *pyoz.PyObject, msg: [*:0]const u8) ?Node {
        py.PyErr_SetString(exc, msg);
        return null;
    }

    fn callParse(f: abi.ParseFn, ptr: [*]const u8, len: usize, out: *abi.ParseOutput, start_rule: u32, flags: u32) i32 {
        return f(ptr, len, out, start_rule, flags);
    }

    /// Parse the whole input. Returns the root Node; raises ParseError on failure.
    pub fn parse(self: *GrammarParser, args: pyoz.Args(struct { input: *pyoz.PyObject, start: ?*pyoz.PyObject = null })) pyoz.Signature(anyerror!?Node, "Node") {
        return .{ .value = self.run(args.value.input, args.value.start, 0, true) };
    }

    /// Match a prefix of the input. Returns the root Node (its end() is where
    /// the match stopped), or None if the start rule doesn't match.
    pub fn match(self: *GrammarParser, args: pyoz.Args(struct { input: *pyoz.PyObject, start: ?*pyoz.PyObject = null })) !?Node {
        return self.run(args.value.input, args.value.start, abi.FLAG_PREFIX, false);
    }

    /// Does the whole input match? Runs a separate accept/reject-only parser
    /// (compiled on first use) that builds no tree and tracks no errors; on a
    /// rejection, the tree parser runs once more to fill in `error`.
    pub fn matches(self: *GrammarParser, args: pyoz.Args(struct { input: *pyoz.PyObject, start: ?*pyoz.PyObject = null })) pyoz.Signature(anyerror!?bool, "bool") {
        return .{ .value = self.runValidate(args.value.input, args.value.start) };
    }

    fn runValidate(self: *GrammarParser, input: *pyoz.PyObject, start: ?*pyoz.PyObject) !?bool {
        const compiled = self._compiled orelse return error.ParserNotLoaded;

        var len: py.Py_ssize_t = 0;
        const ptr: [*]const u8 = blk: {
            if (py.PyUnicode_Check(input)) {
                break :blk py.c.PyUnicode_AsUTF8AndSize(input, &len) orelse return null;
            }
            if (py.PyBytes_Check(input)) {
                var p: [*]u8 = undefined;
                if (py.PyBytes_AsStringAndSize(input, &p, &len) < 0) return null;
                break :blk p;
            }
            py.PyErr_SetString(py.PyExc_TypeError(), "input must be str or bytes");
            return null;
        };
        const input_len: usize = @intCast(len);
        if (input_len > std.math.maxInt(u32)) return error.InputTooLarge;

        var start_rule: u32 = 0;
        if (start) |s| {
            if (s != py.Py_None()) {
                if (!py.PyUnicode_Check(s)) {
                    py.PyErr_SetString(py.PyExc_TypeError(), "start must be a rule name (str)");
                    return null;
                }
                var slen: py.Py_ssize_t = 0;
                const sptr = py.c.PyUnicode_AsUTF8AndSize(s, &slen) orelse return null;
                start_rule = compiled.ruleId(sptr[0..@intCast(slen)]) orelse {
                    py.PyErr_SetString(py.PyExc_ValueError(), "unknown start rule");
                    return null;
                };
            }
        }

        const validate = if (compiled.validate_fn.load(.acquire)) |f| f else try pyoz.allowThreadsTry(Compiled.validator, .{compiled});
        var output = abi.ParseOutput{};
        defer jit_helpers.memoFree(&output);
        const rc = if (input_len >= GIL_RELEASE_BYTES)
            pyoz.allowThreads(callParse, .{ validate, ptr, input_len, &output, start_rule, 0 })
        else
            validate(ptr, input_len, &output, start_rule, 0);
        if (rc < 0) return error.ParseFailed;
        if (output.status == 1) {
            self._last_error = .{};
            return true;
        }

        // Rejected: let the tree parser explain why (sets `error`). It
        // agrees with the validator, so it returns no node; if it ever did,
        // release the node's reference to its tree.
        const node = self.run(input, start, 0, false) catch {
            py.c.PyErr_Clear();
            return false;
        };
        if (node) |n| {
            var owned = n;
            owned._tree.clear();
        }
        return false;
    }

    /// Exposed to Python as the `error` property (PyOZ maps get_X to property X).
    /// parse() raises automatically; this keeps the structured details around.
    pub fn get_error(self: *const GrammarParser) ?ParseError {
        const err = self._last_error;
        if (err.kind == .none) return null;

        var e = ParseError{};
        const msg = self.errorMessage(err, &e._message);
        e._message_len = msg.len;
        e._offset = err.offset;
        e._line = err.line;
        e._column = err.col;
        return e;
    }

    /// Names of the grammar's rules, in definition order (the first is the default start rule).
    pub fn rules(self: *GrammarParser) pyoz.Signature(?*pyoz.PyObject, "list[str]") {
        const tbl = self.ruleTable() catch {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        const list = py.c.PyList_New(@intCast(tbl.names.len)) orelse return .{ .value = null };
        for (tbl.names, 0..) |n, i| {
            py.Py_IncRef(n);
            _ = py.c.PyList_SetItem(list, @intCast(i), n);
        }
        return .{ .value = list };
    }

    pub fn __repr__(self: *const GrammarParser, buf: []u8) []const u8 {
        const compiled = self._compiled orelse return "GrammarParser(not loaded)";
        var pos: usize = 0;
        pos += copySlice(buf, pos, "GrammarParser(");
        pos += fmtInt(buf, pos, @intCast(compiled.rule_names.len));
        pos += copySlice(buf, pos, " rules)");
        return buf[0..pos];
    }

    pub const __doc__: [*:0]const u8 = "A compiled grammar parser. Call .parse(input) to parse a string.";
    pub const parse__doc__: [*:0]const u8 = "Parse a str (or UTF-8 bytes), which must match completely. start= names the start rule (default: the first). Returns the root Node, raises ParseError on failure.";
    pub const match__doc__: [*:0]const u8 = "Match the start rule at the beginning of the input without requiring it to consume everything. Returns the root Node (see end()), or None if it doesn't match.";
    pub const rules__doc__: [*:0]const u8 = "Names of the grammar's rules, in definition order.";
    pub const matches__doc__: [*:0]const u8 = "Does the whole input match the grammar? Several times faster than parse(): builds no tree. On False, `error` explains the rejection. start= names the start rule.";
    pub const error__doc__: [*:0]const u8 = "ParseError from the last failed parse (message, line, column, offset), or None.";
};

// ============================================================================
// Module-level functions
// ============================================================================

/// Return zgram version
fn version() []const u8 {
    return @import("build_options").version;
}

fn compileNative(grammar_text: []const u8) !GrammarParser {
    return .{ ._compiled = try compileCompiled(grammar_text) };
}

/// Compile a grammar string into a native parser via LLVM JIT.
/// Runs with the GIL released; repeated grammars come from a cache.
fn compile(grammar_text: []const u8) !GrammarParser {
    return pyoz.allowThreadsTry(compileNative, .{grammar_text});
}

/// Drop cached compiled grammars (parsers already created keep working).
fn clear_cache() void {
    cache.clear();
}

/// Dump the LLVM IR text for a grammar (useful for debugging/optimization).
fn dump_ir(grammar_text: []const u8) pyoz.Signature(anyerror!pyoz.Owned([]const u8), "str") {
    return .{ .value = dumpIr(grammar_text) };
}

fn dumpIr(grammar_text: []const u8) !pyoz.Owned([]const u8) {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const grammar = try grammar_parser.parseGrammar(alloc, grammar_text);
    const result = jit_codegen.generateModule(alloc, grammar, .tree) catch return error.CompilationFailed;

    // Print module to string, then dispose the module/context
    const LB = @import("llvm_builder.zig");
    defer LB.llvm.LLVMContextDispose(result.context);
    defer LB.llvm.LLVMDisposeModule(result.module);
    const ir = LB.llvm.LLVMPrintModuleToString(result.module) orelse return error.CompilationFailed;
    defer LB.llvm.LLVMDisposeMessage(ir);
    return pyoz.owned(allocator, try allocator.dupe(u8, std.mem.span(ir)));
}

// ============================================================================
// Module definition
// ============================================================================

pub const Module = pyoz.module(.{
    .name = "zgram",
    .doc = "zgram - PEG parser generator. Compiles grammars to native code via LLVM JIT.",
    .funcs = &.{
        pyoz.func("compile", compile, "Compile a grammar string into a native parser").withParams("grammar"),
        pyoz.func("compile_async", pyoz.asyncFn(compileNative), "Compile a grammar on a worker thread; returns an awaitable GrammarParser").withParams("grammar"),
        pyoz.func("clear_cache", clear_cache, "Drop cached compiled grammars"),
        pyoz.func("dump_ir", dump_ir, "Dump LLVM IR text for a grammar").withParams("grammar"),
        pyoz.func("version", version, "Return zgram version string"),
    },
    .classes = &.{
        pyoz.class("Node", Node),
        pyoz.class("NodeIter", NodeIter),
        pyoz.class("_Tree", Tree),
        pyoz.class("GrammarParser", GrammarParser),
        pyoz.class("ParseErrorInfo", ParseError),
    },
    .exceptions = &.{
        pyoz.exception("ParseError", .{ .doc = "Raised when parsing fails", .base = .ValueError }),
    },
    .error_mappings = &.{
        pyoz.mapError("ParserNotLoaded", .RuntimeError),
        pyoz.mapError("AllocationFailed", .RuntimeError),
        pyoz.mapError("ParseFailed", .RuntimeError),
        pyoz.mapError("IndexOutOfBounds", .IndexError),
        pyoz.mapError("InputTooLarge", .ValueError),
        pyoz.mapError("DuplicateRule", .ValueError),
        pyoz.mapError("InvalidCharRange", .ValueError),
        pyoz.mapError("NestingTooDeep", .ValueError),
        pyoz.mapError("RuleNameTooLong", .ValueError),
        pyoz.mapError("EmptyLiteral", .ValueError),
        pyoz.mapError("TooManyRules", .ValueError),
        pyoz.mapError("EmptyGrammar", .ValueError),
        pyoz.mapError("UndefinedRule", .ValueError),
        pyoz.mapError("LeftRecursion", .ValueError),
        pyoz.mapErrorMsg("UnknownAnnotation", .ValueError, "unknown annotation (expected @silent or @memo)"),
        pyoz.mapError("ExpectedRuleName", .ValueError),
        pyoz.mapError("ExpectedEquals", .ValueError),
        pyoz.mapError("ExpectedExpression", .ValueError),
        pyoz.mapError("ExpectedCloseParen", .ValueError),
        pyoz.mapError("UnterminatedString", .ValueError),
        pyoz.mapError("UnterminatedCharClass", .ValueError),
        pyoz.mapError("CompilationFailed", .RuntimeError),
    },
});

// ============================================================================
// Windows: run C++ static constructors
// ============================================================================

/// On Windows the .pyd's entry point is Zig's _DllMainCRTStartup, which
/// doesn't run the C++ static constructors of the bundled LLVM libraries
/// (the MinGW CRT entry point normally would). Without them every LLVM
/// command-line option keeps a zero value instead of its default, which
/// among other things makes LLVM loop forever uniquing value names. Zig's
/// entry point calls root.DllMain, so run the constructors from there.
pub const DllMain = if (builtin.os.tag == .windows) windows_init.DllMain else {};

const windows_init = struct {
    const win = std.os.windows;
    /// MinGW CRT (crt/gccmain.c): runs the global constructor list once
    extern fn __main() callconv(.c) void;

    fn DllMain(hinst: win.HINSTANCE, reason: win.DWORD, reserved: win.LPVOID) callconv(.winapi) win.BOOL {
        _ = hinst;
        _ = reserved;
        const DLL_PROCESS_ATTACH = 1;
        if (reason == DLL_PROCESS_ATTACH) __main();
        return .TRUE;
    }
};

// Required: forces analysis of all pub decls so PyInit_ is exported.
comptime {
    for (@typeInfo(@This()).@"struct".decls) |decl| {
        _ = @field(@This(), decl.name);
    }
}
