const std = @import("std");
const pyoz = @import("PyOZ");
const abi = @import("parse_abi.zig");
const cache = @import("cache.zig");
const grammar_parser = @import("grammar_parser.zig");
const codegen = @import("codegen.zig");
const compiler = @import("compiler.zig");

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
        pos += copySlice(buf, pos, "ParseError('");
        pos += copySlice(buf, pos, self._message[0..self._message_len]);
        pos += copySlice(buf, pos, "', line=");
        pos += fmtInt(buf, pos, self._line);
        pos += copySlice(buf, pos, ", col=");
        pos += fmtInt(buf, pos, self._column);
        pos += copySlice(buf, pos, ")");
        return buf[0..pos];
    }

    pub const __doc__: [*:0]const u8 = "Parse error with location information (line, column, offset, message).";
};

// ============================================================================
// Node class — a node in the parse tree
// ============================================================================

const Node = struct {
    _rule: [abi.MAX_RULE_NAME]u8 = [_]u8{0} ** abi.MAX_RULE_NAME,
    _rule_len: usize = 0,
    _start: i64 = 0,
    _end: i64 = 0,
    /// Index into shared flat node array (stored on GrammarParser)
    _node_index: i64 = 0,
    /// Pointer back to the parser's node/input storage
    _nodes_ptr: ?[*]const abi.FlatNode = null,
    _nodes_count: usize = 0,
    _input_ptr: ?[*]const u8 = null,
    _input_len: usize = 0,
    /// Pointer to rule name table (lives on ParseOutput, persists across calls)
    _rule_names_ptr: ?*const [abi.MAX_RULES]abi.RuleNameEntry = null,
    /// Iterator state for __iter__/__next__
    _iter_index: i64 = 0,

    pub fn rule(self: *const Node) []const u8 {
        return self._rule[0..self._rule_len];
    }

    pub fn text(self: *const Node) []const u8 {
        // Zero-copy: slice directly from input buffer
        const inp = self._input_ptr orelse return "";
        const s: usize = @intCast(self._start);
        const e: usize = @intCast(self._end);
        if (s <= e and e <= self._input_len) {
            return inp[s..e];
        }
        return "";
    }

    pub fn start(self: *const Node) i64 {
        return self._start;
    }

    pub fn end(self: *const Node) i64 {
        return self._end;
    }

    pub fn span(self: *const Node) struct { i64, i64 } {
        return .{ self._start, self._end };
    }

    pub fn child_count(self: *const Node) i64 {
        const nodes = self._nodes_ptr orelse return 0;
        const idx: usize = @intCast(self._node_index);
        if (idx >= self._nodes_count) return 0;
        return @intCast(nodes[idx].child_count());
    }

    /// Get child node at index. Returns a new Node struct (PyOZ wraps it).
    /// Children are found by skipping subtrees from parent_idx + 1.
    pub fn child(self: *const Node, index: i64) ?Node {
        const nodes = self._nodes_ptr orelse return null;
        const idx: usize = @intCast(self._node_index);
        if (idx >= self._nodes_count) return null;

        const flat = nodes[idx];
        const cc = flat.child_count();
        if (cc == 0) return null;
        if (index < 0 or index >= @as(i64, @intCast(cc))) return null;

        // Walk past `index` subtrees starting from idx+1
        var child_idx: usize = idx + 1;
        var i: usize = 0;
        while (i < @as(usize, @intCast(index))) : (i += 1) {
            if (child_idx >= self._nodes_count) return null;
            const skip = @as(usize, nodes[child_idx].subtree_size) + 1;
            child_idx, const overflow = @addWithOverflow(child_idx, skip);
            if (overflow != 0) return null;
        }
        if (child_idx >= self._nodes_count) return null;

        return nodeFromFlat(nodes, child_idx, self._nodes_count, self._input_ptr, self._input_len, self._rule_names_ptr);
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

    pub fn __iter__(self: *Node) *Node {
        self._iter_index = 0;
        return self;
    }

    pub fn __next__(self: *Node) ?Node {
        const cc = self.child_count();
        if (self._iter_index >= cc) return null;
        const c = self.child(self._iter_index);
        self._iter_index += 1;
        return c;
    }

    // ── String representations ──

    pub fn __str__(self: *const Node) []const u8 {
        return self.text();
    }

    pub fn __repr__(self: *const Node, buf: []u8) []const u8 {
        // Format: Node('rule', start..end, N children)
        const r = self.rule();
        const cc = self.child_count();

        var pos: usize = 0;
        pos += copySlice(buf, pos, "Node('");
        pos += copySlice(buf, pos, r);
        pos += copySlice(buf, pos, "', ");
        pos += fmtInt(buf, pos, self._start);
        pos += copySlice(buf, pos, "..");
        pos += fmtInt(buf, pos, self._end);
        pos += copySlice(buf, pos, ", ");
        pos += fmtInt(buf, pos, cc);
        pos += copySlice(buf, pos, " children)");

        return buf[0..pos];
    }

    // ── Boolean / equality ──

    pub fn __bool__(self: *const Node) bool {
        _ = self;
        return true;
    }

    pub fn __eq__(self: *const Node, other: *const Node) bool {
        return self._start == other._start and
            self._end == other._end and
            self._node_index == other._node_index and
            self._nodes_ptr == other._nodes_ptr;
    }

    // ── Tree navigation ──

    /// Return all children as a list.
    pub fn children(self: *const Node) []Node {
        const cc = self.child_count();
        if (cc == 0) return &.{};
        const buf = std.heap.c_allocator.alloc(Node, @intCast(cc)) catch return &.{};
        var i: i64 = 0;
        var count: usize = 0;
        while (i < cc) : (i += 1) {
            if (self.child(i)) |c| {
                buf[count] = c;
                count += 1;
            }
        }
        return buf[0..count];
    }

    /// Search descendants depth-first for nodes matching a rule name.
    pub fn find(self: *const Node, rule_name: []const u8) []Node {
        const nodes = self._nodes_ptr orelse return &.{};
        var result: std.ArrayList(Node) = .empty;
        findRecursive(nodes, self._nodes_count, self._input_ptr, self._input_len, self._rule_names_ptr, @intCast(self._node_index), rule_name, &result);
        return result.items;
    }

    // ── Docstrings ──

    pub const __doc__: [*:0]const u8 = "A node in the parse tree. Supports iteration, indexing, and tree navigation.";
    pub const rule__doc__: [*:0]const u8 = "Return the grammar rule name that matched this node.";
    pub const text__doc__: [*:0]const u8 = "Return the matched text (zero-copy slice from input).";
    pub const child__doc__: [*:0]const u8 = "Get child node at index, or None if out of bounds.";
    pub const child__params__ = "index";
    pub const children__doc__: [*:0]const u8 = "Return all children as a list of Node.";
    pub const find__doc__: [*:0]const u8 = "Search descendants for nodes matching a rule name. Returns a list.";
    pub const find__params__ = "rule_name";
    pub const child_count__doc__: [*:0]const u8 = "Return the number of child nodes.";

    // ── Freelist for fast allocation ──

    pub const __freelist__: usize = 64;
};

/// Construct a Node from a flat node at the given index
fn nodeFromFlat(
    nodes: [*]const abi.FlatNode,
    idx: usize,
    count: usize,
    input_ptr: ?[*]const u8,
    input_len: usize,
    rule_names: ?*const [abi.MAX_RULES]abi.RuleNameEntry,
) Node {
    const flat = nodes[idx];
    var node = Node{
        ._node_index = @intCast(idx),
        ._nodes_ptr = nodes,
        ._nodes_count = count,
        ._input_ptr = input_ptr,
        ._input_len = input_len,
        ._rule_names_ptr = rule_names,
    };

    // Look up rule name from rule_id via the rule name table
    if (rule_names) |rn| {
        const rid = flat.rule_id();
        if (rid < abi.MAX_RULES) {
            const entry = rn[rid];
            const rlen: usize = entry.name_len;
            @memcpy(node._rule[0..rlen], entry.name[0..rlen]);
            node._rule_len = rlen;
        }
    }

    // Copy text span info (zero-copy text — text() slices from input directly)
    node._start = @intCast(flat.text_start);
    node._end = @intCast(flat.text_end);

    return node;
}

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

// ── Tree search helper ──

fn findRecursive(
    nodes: [*]const abi.FlatNode,
    count: usize,
    input_ptr: ?[*]const u8,
    input_len: usize,
    rule_names: ?*const [abi.MAX_RULES]abi.RuleNameEntry,
    node_idx: usize,
    target_rule: []const u8,
    result: *std.ArrayList(Node),
) void {
    if (node_idx >= count) return;
    const flat = nodes[node_idx];

    // Check if this node's rule matches
    if (rule_names) |rn| {
        const rid = flat.rule_id();
        if (rid < abi.MAX_RULES) {
            const entry = rn[rid];
            const name = entry.name[0..entry.name_len];
            if (std.mem.eql(u8, name, target_rule)) {
                const node = nodeFromFlat(nodes, node_idx, count, input_ptr, input_len, rule_names);
                result.append(std.heap.c_allocator, node) catch return;
            }
        }
    }

    // Recurse into children (skip subtrees to find each direct child)
    const cc = flat.child_count();
    if (cc > 0) {
        var child_idx: usize = node_idx + 1;
        var i: usize = 0;
        while (i < cc) : (i += 1) {
            if (child_idx >= count) break;
            findRecursive(nodes, count, input_ptr, input_len, rule_names, child_idx, target_rule, result);
            if (child_idx >= count) break;
            const skip = @as(usize, nodes[child_idx].subtree_size) + 1;
            child_idx, const overflow = @addWithOverflow(child_idx, skip);
            if (overflow != 0) break;
        }
    }
}

// ============================================================================
// GrammarParser class — wraps a dlopen'd grammar .so
// ============================================================================

const GrammarParser = struct {
    _path: [512]u8 = [_]u8{0} ** 512,
    _path_len: usize = 0,
    _handle: ?*anyopaque = null,
    _parse_fn: ?abi.ParseFn = null,
    /// Heap-allocated parse output (persists so Node back-references remain valid)
    _output: ?*abi.ParseOutput = null,
    /// Stores a copy of the input so Node text slicing works after parse returns
    _input_buf: ?[*]u8 = null,
    _input_len: usize = 0,
    _input_cap: usize = 0,

    const allocator = std.heap.c_allocator;

    fn ensureAllocated(self: *GrammarParser) !void {
        if (self._output == null) {
            self._output = try allocator.create(abi.ParseOutput);
            self._output.?.* = .{};
        }
    }

    fn ensureInputBuf(self: *GrammarParser, needed: usize) !void {
        if (self._input_cap >= needed) return;
        // Grow to next power of 2, using u64 to prevent overflow
        var cap: u64 = if (self._input_cap == 0) 4096 else self._input_cap;
        while (cap < needed) {
            cap *|= 2;
            if (cap >= std.math.maxInt(usize) / 2) break;
        }
        const final_cap: usize = @intCast(@min(cap, std.math.maxInt(usize)));

        if (self._input_buf) |old| {
            const old_slice = old[0..self._input_cap];
            if (allocator.resize(old_slice, final_cap)) {
                self._input_cap = final_cap;
                return;
            }
            // Allocate new BEFORE freeing old to prevent use-after-free on failure
            const new_slice = try allocator.alloc(u8, final_cap);
            allocator.free(old_slice);
            self._input_buf = new_slice.ptr;
            self._input_cap = final_cap;
        } else {
            const new_slice = try allocator.alloc(u8, final_cap);
            self._input_buf = new_slice.ptr;
            self._input_cap = final_cap;
        }
    }

    pub fn __del__(self: *GrammarParser) void {
        if (self._handle) |handle| {
            _ = std.c.dlclose(handle);
            self._handle = null;
            self._parse_fn = null;
        }
        if (self._output) |output| {
            if (output.nodes_ptr) |nodes| {
                allocator.free(nodes[0..output.node_capacity]);
            }
            allocator.destroy(output);
            self._output = null;
        }
        if (self._input_buf) |buf| {
            allocator.free(buf[0..self._input_cap]);
            self._input_buf = null;
        }
    }

    /// Parse an input string. Returns the root Node on success.
    /// Raises ParseError (via get_error()) on failure.
    pub fn parse(self: *GrammarParser, input: []const u8) !?Node {
        const parse_fn = self._parse_fn orelse return error.ParserNotLoaded;

        // FlatNode stores positions as u32 — reject inputs that would overflow
        if (input.len > std.math.maxInt(u32)) return error.InputTooLarge;

        try self.ensureAllocated();
        const output = self._output orelse return error.AllocationFailed;

        // We must copy input because Python may free/move it after parse returns,
        // but Nodes hold pointers into it for zero-copy text().
        try self.ensureInputBuf(input.len);
        const input_buf = self._input_buf orelse return error.AllocationFailed;
        @memcpy(input_buf[0..input.len], input);
        self._input_len = input.len;

        // Call the grammar's parse function
        const rc = parse_fn(input.ptr, input.len, output);
        if (rc < 0) return error.ParseFailed;

        if (output.status == 1 and output.node_count > 0) {
            const nodes = output.nodes_ptr orelse return error.ParseFailed;
            return nodeFromFlat(
                nodes,
                0,
                output.node_count,
                input_buf,
                self._input_len,
                &output.rule_names,
            );
        }

        // Parse failed — raise ParseError exception with details
        const mlen = @min(output.error_message_len, 256);
        var msg_buf: [320]u8 = undefined;
        var pos: usize = 0;
        pos += copySlice(&msg_buf, pos, "line ");
        pos += fmtInt(&msg_buf, pos, @intCast(output.error_line));
        pos += copySlice(&msg_buf, pos, ", col ");
        pos += fmtInt(&msg_buf, pos, @intCast(output.error_col));
        pos += copySlice(&msg_buf, pos, ": ");
        pos += copySlice(&msg_buf, pos, output.error_message[0..mlen]);
        msg_buf[pos] = 0;

        Module.getException(0).raise(msg_buf[0..pos :0]);
        return null;
    }

    /// Get the parse error (call after parse() returns None).
    /// Kept for backward compatibility — parse() now raises automatically.
    pub fn get_error(self: *const GrammarParser) ?ParseError {
        const output = self._output orelse return null;
        if (output.status != 0) return null;

        var err = ParseError{};
        const mlen = @min(output.error_message_len, 256);
        @memcpy(err._message[0..mlen], output.error_message[0..mlen]);
        err._message_len = mlen;
        err._offset = @intCast(output.error_offset);
        err._line = @intCast(output.error_line);
        err._column = @intCast(output.error_col);
        return err;
    }

    pub fn __repr__(self: *const GrammarParser, buf: []u8) []const u8 {
        const output = self._output orelse return "GrammarParser(not loaded)";
        if (output.rule_count == 0) return "GrammarParser(not parsed yet)";
        var pos: usize = 0;
        pos += copySlice(buf, pos, "GrammarParser(");
        pos += fmtInt(buf, pos, @intCast(output.rule_count));
        pos += copySlice(buf, pos, " rules)");
        return buf[0..pos];
    }

    pub const __doc__: [*:0]const u8 = "A compiled grammar parser. Call .parse(input) to parse a string.";
    pub const parse__doc__: [*:0]const u8 = "Parse an input string. Returns the root Node on success, raises ParseError on failure.";
    pub const parse__params__ = "input";
    pub const get_error__doc__: [*:0]const u8 = "Get the parse error from the last failed parse, or None.";
};

// ============================================================================
// Module-level functions
// ============================================================================

/// Load a compiled grammar .so and return a GrammarParser
fn load_parser(path: []const u8) !GrammarParser {
    var path_buf: [512:0]u8 = [_:0]u8{0} ** 512;
    if (path.len >= path_buf.len) return error.PathTooLong;
    @memcpy(path_buf[0..path.len], path);
    path_buf[path.len] = 0;

    const handle = std.c.dlopen(&path_buf, .{ .LAZY = true }) orelse {
        // Log dlerror for diagnostics
        if (std.c.dlerror()) |err| {
            std.log.err("dlopen failed: {s}", .{err});
        }
        return error.LoadFailed;
    };

    const sym = std.c.dlsym(handle, "zgram_parse") orelse {
        if (std.c.dlerror()) |err| {
            std.log.err("dlsym failed: {s}", .{err});
        }
        _ = std.c.dlclose(handle);
        return error.MissingSymbol;
    };

    const parse_fn: abi.ParseFn = @ptrCast(sym);

    var parser = GrammarParser{};
    @memcpy(parser._path[0..path.len], path);
    parser._path_len = path.len;
    parser._handle = handle;
    parser._parse_fn = parse_fn;

    return parser;
}

/// Return zgram version
fn version() []const u8 {
    return "0.1.0";
}

/// Compile a grammar string into a native parser.
/// This is the main entry point exposed to Python.
fn compile(grammar_text: []const u8) !GrammarParser {
    const alloc = std.heap.page_allocator;

    // 1. Hash the grammar
    var hash_buf: [64]u8 = undefined;
    cache.grammarHash(grammar_text, &hash_buf);

    // 2. Check cache
    var cached_path_buf: [512]u8 = undefined;
    if (cache.getCached(&hash_buf, &cached_path_buf)) |cached_path| {
        return load_parser(cached_path);
    }

    // 3. Parse grammar -> IR
    const grammar = try grammar_parser.parseGrammar(alloc, grammar_text);
    defer grammar.deinit(alloc);

    // 4. Generate Zig source
    const zig_source = try codegen.generateZigSource(alloc, grammar);
    defer alloc.free(zig_source);

    // 5. Compile
    const so_path = try compiler.compileGrammar(alloc, zig_source, &hash_buf);
    defer alloc.free(so_path);

    // 6. Cache, then clean up temp build directory
    var store_path_buf: [512]u8 = undefined;
    const final_path = try cache.storeCached(&hash_buf, so_path, &store_path_buf);
    compiler.cleanupBuildDir(&hash_buf);

    // 7. Load and return
    return load_parser(final_path);
}

/// Clear all cached grammar .so files
fn clear_cache() !i64 {
    return cache.clearCache();
}

// ============================================================================
// Module definition
// ============================================================================

pub const Module = pyoz.module(.{
    .name = "zgram",
    .doc = "zgram - Comptime-optimized PEG parser generator. Compiles grammars to native code via Zig.",
    .funcs = &.{
        pyoz.func("compile", compile, "Compile a grammar string into a native parser"),
        pyoz.func("load_parser", load_parser, "Load a compiled grammar .so and return a GrammarParser"),
        pyoz.func("version", version, "Return zgram version string"),
        pyoz.func("clear_cache", clear_cache, "Clear all cached grammar .so files"),
    },
    .classes = &.{
        pyoz.class("Node", Node),
        pyoz.class("GrammarParser", GrammarParser),
        pyoz.class("ParseError", ParseError),
    },
    .exceptions = &.{
        pyoz.exception("ParseError", .{ .doc = "Raised when parsing fails", .base = .ValueError }),
    },
    .error_mappings = &.{
        pyoz.mapError("ParserNotLoaded", .RuntimeError),
        pyoz.mapError("AllocationFailed", .RuntimeError),
        pyoz.mapError("ParseFailed", .RuntimeError),
        pyoz.mapError("PathTooLong", .ValueError),
        pyoz.mapError("LoadFailed", .RuntimeError),
        pyoz.mapError("MissingSymbol", .RuntimeError),
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
        pyoz.mapError("ExpectedRuleName", .ValueError),
        pyoz.mapError("ExpectedEquals", .ValueError),
        pyoz.mapError("ExpectedExpression", .ValueError),
        pyoz.mapError("ExpectedCloseParen", .ValueError),
        pyoz.mapError("UnterminatedString", .ValueError),
        pyoz.mapError("UnterminatedCharClass", .ValueError),
        pyoz.mapError("CompilationFailed", .RuntimeError),
        pyoz.mapError("OutputNotFound", .RuntimeError),
        pyoz.mapError("HomeNotSet", .RuntimeError),
    },
});

pub export fn PyInit_zgram() ?*pyoz.PyObject {
    return Module.init();
}
