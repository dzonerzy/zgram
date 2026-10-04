const std = @import("std");
const builtin = @import("builtin");
const pyoz = @import("PyOZ");
const py = pyoz.py;
const abi = @import("parse_abi.zig");
const grammar_parser = @import("grammar_parser.zig");
const jit_codegen = @import("jit_codegen.zig");
const jit_compiler = @import("jit_compiler.zig");
const llvm_capsule_mod = @import("llvm_capsule.zig");
const jit_helpers = @import("jit_helpers.zig");
const diagnose = @import("diagnose.zig");
const native_stack = @import("stack.zig");

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
// Diagnostic class — one error, warning or note about a source text
// ============================================================================

/// The type every stage of a language built on zgram reports problems with:
/// zgram's own syntax errors (ParseError.diagnostic), and the checks and
/// runtime errors of the packages built on it.
const Diagnostic = struct {
    _severity: ?*pyoz.PyObject = null,
    _code: ?*pyoz.PyObject = null,
    _message: ?*pyoz.PyObject = null,
    /// Byte offsets into the UTF-8 source
    _start: i64 = 0,
    _end: i64 = 0,
    /// 1-based, the column in bytes; 0 = unknown (render() works them out)
    _line: i64 = 0,
    _column: i64 = 0,
    /// list[Diagnostic]
    _notes: ?*pyoz.PyObject = null,

    fn utf8(obj: ?*pyoz.PyObject) []const u8 {
        var len: py.Py_ssize_t = 0;
        const ptr = py.c.PyUnicode_AsUTF8AndSize(obj orelse return "", &len) orelse {
            py.c.PyErr_Clear();
            return "";
        };
        return ptr[0..@intCast(len)];
    }

    /// Takes new references to the objects. Raises (and returns null) on
    /// arguments of the wrong type.
    fn create(severity: *pyoz.PyObject, code: *pyoz.PyObject, message: *pyoz.PyObject, start: i64, end: i64, line: i64, column: i64, notes: ?*pyoz.PyObject) ?Diagnostic {
        if (!py.PyUnicode_Check(severity) or !py.PyUnicode_Check(code) or !py.PyUnicode_Check(message)) {
            py.PyErr_SetString(py.PyExc_TypeError(), "severity, code and message must be str");
            return null;
        }
        const sev = utf8(severity);
        if (!std.mem.eql(u8, sev, "error") and !std.mem.eql(u8, sev, "warning") and !std.mem.eql(u8, sev, "note")) {
            py.PyErr_SetString(py.PyExc_ValueError(), "severity must be 'error', 'warning' or 'note'");
            return null;
        }
        if (start < 0 or end < start or line < 0 or column < 0) {
            py.PyErr_SetString(py.PyExc_ValueError(), "span must be 0 <= start <= end, and line and column >= 0");
            return null;
        }
        const note_list = (if (notes) |n| py.c.PySequence_List(n) else py.c.PyList_New(0)) orelse return null;
        for (0..@intCast(py.c.PyList_Size(note_list))) |i| {
            const item = py.c.PyList_GetItem(note_list, @intCast(i)).?;
            _ = Module.fromPy(*const Diagnostic, item) catch {
                py.Py_DecRef(note_list);
                py.c.PyErr_Clear();
                py.PyErr_SetString(py.PyExc_TypeError(), "notes must be Diagnostic objects");
                return null;
            };
        }
        py.Py_IncRef(severity);
        py.Py_IncRef(code);
        py.Py_IncRef(message);
        return .{ ._severity = severity, ._code = code, ._message = message, ._start = start, ._end = end, ._line = line, ._column = column, ._notes = note_list };
    }

    /// Diagnostic(severity, code, message, span, line=0, column=0, notes=())
    pub fn __new__(args: pyoz.Args(struct {
        severity: *pyoz.PyObject,
        code: *pyoz.PyObject,
        message: *pyoz.PyObject,
        span: struct { i64, i64 },
        line: i64 = 0,
        column: i64 = 0,
        notes: ?*pyoz.PyObject = null,
    })) ?Diagnostic {
        const a = args.value;
        return create(a.severity, a.code, a.message, a.span[0], a.span[1], a.line, a.column, a.notes);
    }

    pub fn __del__(self: *Diagnostic) void {
        inline for (.{ "_severity", "_code", "_message", "_notes" }) |name| {
            if (@field(self, name)) |obj| py.Py_DecRef(obj);
            @field(self, name) = null;
        }
    }

    fn owned(obj: ?*pyoz.PyObject) ?*pyoz.PyObject {
        const o = obj orelse py.Py_None();
        py.Py_IncRef(o);
        return o;
    }

    pub fn get_severity(self: *const Diagnostic) pyoz.Signature(?*pyoz.PyObject, "str") {
        return .{ .value = owned(self._severity) };
    }

    pub fn get_code(self: *const Diagnostic) pyoz.Signature(?*pyoz.PyObject, "str") {
        return .{ .value = owned(self._code) };
    }

    pub fn get_message(self: *const Diagnostic) pyoz.Signature(?*pyoz.PyObject, "str") {
        return .{ .value = owned(self._message) };
    }

    pub fn get_span(self: *const Diagnostic) struct { i64, i64 } {
        return .{ self._start, self._end };
    }

    pub fn get_line(self: *const Diagnostic) i64 {
        return self._line;
    }

    pub fn get_column(self: *const Diagnostic) i64 {
        return self._column;
    }

    pub fn get_notes(self: *const Diagnostic) pyoz.Signature(?*pyoz.PyObject, "list[Diagnostic]") {
        return .{ .value = owned(self._notes) };
    }

    pub fn __eq__(self: *const Diagnostic, other: *const Diagnostic) bool {
        if (self._start != other._start or self._end != other._end or self._line != other._line or self._column != other._column) return false;
        inline for (.{ "_severity", "_code", "_message", "_notes" }) |name| {
            const a = @field(self, name) orelse py.Py_None();
            const b = @field(other, name) orelse py.Py_None();
            if (py.c.PyObject_RichCompareBool(a, b, py.c.Py_EQ) != 1) {
                py.c.PyErr_Clear();
                return false;
            }
        }
        return true;
    }

    pub fn __repr__(self: *const Diagnostic, buf: []u8) []const u8 {
        // Diagnostic('error', 'syntax', 'expected num', span=(19, 19), line=2, column=9)
        var pos: usize = 0;
        pos += copySlice(buf, pos, "Diagnostic(");
        pos += copyRepr(buf, pos, self._severity);
        pos += copySlice(buf, pos, ", ");
        pos += copyRepr(buf, pos, self._code);
        pos += copySlice(buf, pos, ", ");
        pos += copyRepr(buf, pos, self._message);
        pos += copySlice(buf, pos, ", span=(");
        pos += fmtInt(buf, pos, self._start);
        pos += copySlice(buf, pos, ", ");
        pos += fmtInt(buf, pos, self._end);
        pos += copySlice(buf, pos, "), line=");
        pos += fmtInt(buf, pos, self._line);
        pos += copySlice(buf, pos, ", column=");
        pos += fmtInt(buf, pos, self._column);
        pos += copySlice(buf, pos, ")");
        return buf[0..pos];
    }

    /// Append "<file>:<line>:<col>: <severity>: <message> [<code>]", the
    /// source line and a line of carets under the span, then the notes.
    fn renderInto(self: *const Diagnostic, out: *std.ArrayList(u8), source: []const u8, filename: []const u8) !void {
        const start: usize = @min(@as(usize, @intCast(self._start)), source.len);
        const end: usize = @min(@as(usize, @intCast(self._end)), source.len);
        var line: u64 = @intCast(self._line);
        var column: u64 = @intCast(self._column);
        if (line == 0) {
            const lc = abi.lineCol(source, start);
            line = lc.line;
            column = lc.col;
        }
        const line_start = if (std.mem.lastIndexOfScalar(u8, source[0..start], '\n')) |i| i + 1 else 0;
        const line_end = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse source.len;
        const text = std.mem.trimEnd(u8, source[line_start..line_end], "\r");

        var num_buf: [24]u8 = undefined;
        const line_str = std.fmt.bufPrint(&num_buf, "{d}", .{line}) catch unreachable;
        var col_buf: [24]u8 = undefined;
        const col_str = std.fmt.bufPrint(&col_buf, "{d}", .{column}) catch unreachable;

        if (filename.len != 0) {
            try out.appendSlice(allocator, filename);
            try out.append(allocator, ':');
        }
        try out.appendSlice(allocator, line_str);
        try out.append(allocator, ':');
        try out.appendSlice(allocator, col_str);
        try out.appendSlice(allocator, ": ");
        try out.appendSlice(allocator, utf8(self._severity));
        try out.appendSlice(allocator, ": ");
        try out.appendSlice(allocator, utf8(self._message));
        const code = utf8(self._code);
        if (code.len != 0) {
            try out.appendSlice(allocator, " [");
            try out.appendSlice(allocator, code);
            try out.append(allocator, ']');
        }

        // "    3 | source line" / "      | ^^^^"
        const gutter = @max(line_str.len, 5);
        try out.append(allocator, '\n');
        try out.appendNTimes(allocator, ' ', gutter - line_str.len);
        try out.appendSlice(allocator, line_str);
        try out.appendSlice(allocator, " | ");
        try out.appendSlice(allocator, text);
        try out.append(allocator, '\n');
        try out.appendNTimes(allocator, ' ', gutter);
        try out.appendSlice(allocator, " | ");
        // One column per character: tabs stay tabs, UTF-8 continuation bytes don't count
        for (source[line_start..start]) |ch| {
            if (ch & 0xC0 == 0x80) continue;
            try out.append(allocator, if (ch == '\t') '\t' else ' ');
        }
        var carets: usize = 0;
        for (source[start..@max(start, @min(end, line_end))]) |ch| {
            if (ch & 0xC0 != 0x80) carets += 1;
        }
        try out.appendNTimes(allocator, '^', @max(carets, 1));

        if (self._notes) |notes| {
            for (0..@intCast(py.c.PyList_Size(notes))) |i| {
                const note = Module.fromPy(*const Diagnostic, py.c.PyList_GetItem(notes, @intCast(i)).?) catch continue;
                try out.append(allocator, '\n');
                try note.renderInto(out, source, filename);
            }
        }
    }

    /// Format the diagnostic with its source line and a caret under the span.
    pub fn render(self: *const Diagnostic, args: pyoz.Args(struct { source: *pyoz.PyObject, filename: ?[]const u8 = null })) pyoz.Signature(?*pyoz.PyObject, "str") {
        const src_obj = args.value.source;
        var len: py.Py_ssize_t = 0;
        const ptr: [*]const u8 = blk: {
            if (py.PyUnicode_Check(src_obj)) break :blk py.c.PyUnicode_AsUTF8AndSize(src_obj, &len) orelse return .{ .value = null };
            var p: [*]u8 = undefined;
            if (py.PyBytes_Check(src_obj) and py.PyBytes_AsStringAndSize(src_obj, &p, &len) == 0) break :blk p;
            py.PyErr_SetString(py.PyExc_TypeError(), "source must be str or bytes");
            return .{ .value = null };
        };
        var out: std.ArrayList(u8) = .empty;
        defer out.deinit(allocator);
        self.renderInto(&out, ptr[0..@intCast(len)], args.value.filename orelse "") catch {
            _ = py.c.PyErr_NoMemory();
            return .{ .value = null };
        };
        return .{ .value = py.c.PyUnicode_DecodeUTF8(out.items.ptr, @intCast(out.items.len), "replace") };
    }

    pub const __doc__: [*:0]const u8 = "Diagnostic(severity, code, message, span, line=0, column=0, notes=()): an error, warning or note about a source text. span is (start, end) in bytes of the UTF-8 source; line and column are 1-based.";
    pub const render__doc__: [*:0]const u8 = "Format as 'file:line:col: severity: message [code]' followed by the source line, a caret line under the span, and the notes.";
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
    /// Label names by field id - 1, slices into name_buf
    field_names: [][]const u8,
    /// What error messages call each rule: its display name, else its name
    display_names: [][]const u8,
    /// Per rule: `-> Name()`, a class called with no arguments
    no_args: []bool,
    /// rule_names then field_names as C strings, for TreeView
    name_strs: []abi.Str,
    /// AST mapping per rule: what `-> name` means, and the name of a class action
    actions: []grammar_parser.Action,
    action_names: [][]const u8,
    /// Labels per rule: rule r's are labels[label_start[r]..label_start[r + 1]]
    labels: []grammar_parser.LabelUse,
    label_start: []u32,
    /// Per rule: can its node have children?
    has_children: []bool,
    name_buf: []u8,
    refs: std.atomic.Value(u32) = .init(1),

    /// Accept/reject-only parser, compiled on first use (see validator())
    validate_fn: std.atomic.Value(?abi.ParseFn) = .init(null),
    validate_resource: jit_compiler.ResourceHandle = null,
    validate_lock: std.atomic.Value(bool) = .init(false),
    /// Error-recovering parser, compiled the first time a parse with
    /// recover=True fails (see recoverer())
    recover_fn: std.atomic.Value(?abi.ParseFn) = .init(null),
    recover_resource: jit_compiler.ResourceHandle = null,
    recover_lock: std.atomic.Value(bool) = .init(false),

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
        for (grammar.fields) |f| total += f.len;
        for (grammar.rules) |r| total += if (r.action) |a| a.len else 0;
        for (grammar.rules) |r| total += if (r.display) |d| d.len else 0;
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
        const field_names = try allocator.alloc([]const u8, grammar.fields.len);
        errdefer allocator.free(field_names);
        for (grammar.fields, 0..) |f, i| {
            @memcpy(name_buf[off..][0..f.len], f);
            field_names[i] = name_buf[off..][0..f.len];
            off += f.len;
        }

        const name_strs = try allocator.alloc(abi.Str, rule_names.len + field_names.len);
        errdefer allocator.free(name_strs);
        for (rule_names, 0..) |n, i| name_strs[i] = .{ .ptr = n.ptr, .len = n.len };
        for (field_names, rule_names.len..) |n, i| name_strs[i] = .{ .ptr = n.ptr, .len = n.len };

        const n_rules = grammar.rules.len;
        const actions = try allocator.alloc(grammar_parser.Action, n_rules);
        errdefer allocator.free(actions);
        const action_names = try allocator.alloc([]const u8, n_rules);
        errdefer allocator.free(action_names);
        const has_children = try allocator.alloc(bool, n_rules);
        errdefer allocator.free(has_children);
        const label_start = try allocator.alloc(u32, n_rules + 1);
        errdefer allocator.free(label_start);
        const display_names = try allocator.alloc([]const u8, n_rules);
        errdefer allocator.free(display_names);
        const no_args = try allocator.alloc(bool, n_rules);
        errdefer allocator.free(no_args);
        var all_labels: std.ArrayList(grammar_parser.LabelUse) = .empty;
        for (grammar.rules, 0..) |r, i| {
            no_args[i] = r.action_no_args;
            display_names[i] = rule_names[i];
            if (r.display) |d| {
                @memcpy(name_buf[off..][0..d.len], d);
                display_names[i] = name_buf[off..][0..d.len];
                off += d.len;
            }
            actions[i] = grammar_parser.actionOf(r);
            const a = r.action orelse "";
            @memcpy(name_buf[off..][0..a.len], a);
            action_names[i] = name_buf[off..][0..a.len];
            off += a.len;
            has_children[i] = grammar_parser.ruleHasChildren(grammar, r);
            label_start[i] = @intCast(all_labels.items.len);
            try all_labels.appendSlice(alloc, try grammar_parser.ruleLabels(alloc, grammar, r));
        }
        label_start[n_rules] = @intCast(all_labels.items.len);
        const labels = try allocator.dupe(grammar_parser.LabelUse, all_labels.items);
        errdefer allocator.free(labels);

        const module = jit_codegen.generateModule(alloc, grammar, .tree) catch return error.CompilationFailed;
        const jit = jit_compiler.jitCompile(module.module, module.context) catch return error.CompilationFailed;

        self.* = .{
            .parse_fn = jit.parse_fn,
            .resource = jit.resource,
            .text = text,
            .hash = std.hash.Wyhash.hash(0, grammar_text),
            .rule_names = rule_names,
            .field_names = field_names,
            .display_names = display_names,
            .no_args = no_args,
            .name_strs = name_strs,
            .actions = actions,
            .action_names = action_names,
            .labels = labels,
            .label_start = label_start,
            .has_children = has_children,
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

    /// The error-recovering parser for this grammar, compiled on first call:
    /// a grammar whose input always parses never pays for it.
    fn recoverer(self: *Compiled) !abi.ParseFn {
        if (self.recover_fn.load(.acquire)) |f| return f;
        while (self.recover_lock.cmpxchgWeak(false, true, .acquire, .monotonic) != null) {
            std.Thread.yield() catch {};
        }
        defer self.recover_lock.store(false, .release);
        if (self.recover_fn.load(.acquire)) |f| return f;

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const grammar = try grammar_parser.parseGrammar(arena.allocator(), self.text);
        const module = jit_codegen.generateModule(arena.allocator(), grammar, .recover) catch return error.CompilationFailed;
        const jit = jit_compiler.jitCompile(module.module, module.context) catch return error.CompilationFailed;
        self.recover_resource = jit.resource;
        self.recover_fn.store(jit.parse_fn, .release);
        return jit.parse_fn;
    }

    fn release(self: *Compiled) void {
        if (self.refs.fetchSub(1, .acq_rel) != 1) return;
        if (self.validate_resource != null) jit_compiler.releaseGrammar(self.validate_resource);
        if (self.recover_resource != null) jit_compiler.releaseGrammar(self.recover_resource);
        jit_compiler.releaseGrammar(self.resource);
        allocator.free(self.rule_names);
        allocator.free(self.field_names);
        allocator.free(self.display_names);
        allocator.free(self.no_args);
        allocator.free(self.name_strs);
        allocator.free(self.actions);
        allocator.free(self.action_names);
        allocator.free(self.labels);
        allocator.free(self.label_start);
        allocator.free(self.has_children);
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
/// The rule name of error nodes: not an identifier, so no grammar rule has it
const ERROR_RULE_NAME = "<error>";

const RuleTable = struct {
    /// Interned rule names by rule id, and ERROR_RULE_NAME after them
    names: []*pyoz.PyObject,
    bytes: []const []const u8,
    /// Label names indexed by field id - 1, interned likewise
    fields: []*pyoz.PyObject,
    field_bytes: []const []const u8,
    /// The interned strings "__zspan__" and "__znode__"
    span_attr: *pyoz.PyObject,
    node_attr: *pyoz.PyObject,

    fn create(rule_names: []const []const u8, field_names: []const []const u8) !*RuleTable {
        const table = try allocator.create(RuleTable);
        errdefer allocator.destroy(table);
        // One name more than the grammar's rules: the rule id after the last
        // one is that of error nodes (parse_tree(recover=True))
        const with_error = try allocator.alloc([]const u8, rule_names.len + 1);
        defer allocator.free(with_error);
        @memcpy(with_error[0..rule_names.len], rule_names);
        with_error[rule_names.len] = ERROR_RULE_NAME;
        const names = try intern(with_error);
        errdefer release(names);
        const fields = try intern(field_names);
        errdefer release(fields);
        var span_attr: ?*pyoz.PyObject = py.PyUnicode_FromStringAndSize("__zspan__", 9) orelse return error.AllocationFailed;
        py.c.PyUnicode_InternInPlace(@ptrCast(&span_attr));
        errdefer py.Py_DecRef(span_attr.?);
        var node_attr: ?*pyoz.PyObject = py.PyUnicode_FromStringAndSize("__znode__", 9) orelse return error.AllocationFailed;
        py.c.PyUnicode_InternInPlace(@ptrCast(&node_attr));
        table.* = .{ .names = names, .bytes = rule_names, .fields = fields, .field_bytes = field_names, .span_attr = span_attr.?, .node_attr = node_attr.? };
        return table;
    }

    fn intern(strings: []const []const u8) ![]*pyoz.PyObject {
        const objs = try allocator.alloc(*pyoz.PyObject, strings.len);
        errdefer allocator.free(objs);
        for (strings, 0..) |name, i| {
            var s: ?*pyoz.PyObject = py.PyUnicode_FromStringAndSize(name.ptr, @intCast(name.len));
            if (s == null) {
                for (objs[0..i]) |n| py.Py_DecRef(n);
                return error.AllocationFailed;
            }
            py.c.PyUnicode_InternInPlace(@ptrCast(&s));
            objs[i] = s.?;
        }
        return objs;
    }

    fn release(objs: []*pyoz.PyObject) void {
        for (objs) |n| py.Py_DecRef(n);
        allocator.free(objs);
    }

    fn destroy(self: *RuleTable) void {
        release(self.names);
        release(self.fields);
        py.Py_DecRef(self.span_attr);
        py.Py_DecRef(self.node_attr);
        allocator.destroy(self);
    }

    /// Field id of the label `name` (0 if the grammar has no such label).
    fn fieldIdOf(self: *const RuleTable, name: []const u8) u8 {
        for (self.field_bytes, 1..) |b, id| {
            if (std.mem.eql(u8, b, name)) return @intCast(id);
        }
        return 0;
    }

    fn idOf(self: *const RuleTable, name: []const u8) ?u16 {
        for (self.bytes, 0..) |b, i| {
            if (std.mem.eql(u8, b, name)) return @intCast(i);
        }
        // Error nodes' rule: the id after the grammar's rules
        if (std.mem.eql(u8, name, ERROR_RULE_NAME)) return @intCast(self.bytes.len);
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
    /// Last counted node whose stored child count was saturated
    _many_parent: u32 = std.math.maxInt(u32),
    _many_count: u32 = 0,
    /// The compiled grammar, kept alive by _parser
    _compiled: ?*const Compiled = null,
    /// The parser itself (the object _parser references)
    _parser_ptr: ?*GrammarParser = null,
    /// What `capsule` points to, filled in on first use
    _view: abi.TreeView = .{},
    /// With recover=True: the syntax errors recovered from (a list of
    /// Diagnostic); null when the input had none
    _errors: ?*pyoz.PyObject = null,

    /// The syntax errors this tree was recovered from, in source order: empty
    /// unless the input had errors and was parsed with recover=True. Their
    /// text is in the tree as error nodes (rule "<error>").
    pub fn get_errors(self: *const Tree) pyoz.Signature(?*pyoz.PyObject, "list[Diagnostic]") {
        if (self._errors) |e| {
            py.Py_IncRef(e);
            return .{ .value = e };
        }
        return .{ .value = py.c.PyList_New(0) };
    }

    /// The root node.
    pub fn get_root(self: *const Tree) Node {
        return makeNode(@constCast(self), 0);
    }

    /// Number of nodes.
    pub fn __len__(self: *const Tree) i64 {
        return self._count;
    }

    /// The node at `index` in the node array (Node.index is the inverse).
    pub fn node(self: *Tree, index: i64) !Node {
        if (index < 0 or index >= self._count) return error.IndexOutOfBounds;
        return makeNode(self, @intCast(index));
    }

    /// A copy of the node array: 16 bytes per node, in pre-order.
    pub fn get_nodes(self: *const Tree) pyoz.Signature(?*pyoz.PyObject, "bytes") {
        const ptr: [*]const u8 = if (self._nodes) |n| @ptrCast(n) else "";
        return .{ .value = py.c.PyBytes_FromStringAndSize(ptr, @as(py.Py_ssize_t, self._count) * @sizeOf(abi.FlatNode)) };
    }

    /// The parsed text as UTF-8 bytes; node offsets index into it.
    pub fn get_input(self: *const Tree) pyoz.Signature(?*pyoz.PyObject, "bytes") {
        if (self._input_obj) |obj| {
            if (py.PyBytes_Check(obj)) {
                py.Py_IncRef(obj);
                return .{ .value = obj };
            }
        }
        const ptr: [*]const u8 = self._input_ptr orelse "";
        return .{ .value = py.c.PyBytes_FromStringAndSize(ptr, @intCast(self._input_len)) };
    }

    fn nameList(names: []const *pyoz.PyObject) ?*pyoz.PyObject {
        const list = py.c.PyList_New(@intCast(names.len)) orelse return null;
        for (names, 0..) |n, i| {
            py.Py_IncRef(n);
            _ = py.c.PyList_SetItem(list, @intCast(i), n);
        }
        return list;
    }

    /// Rule names by rule id.
    pub fn get_rules(self: *const Tree) pyoz.Signature(?*pyoz.PyObject, "list[str]") {
        // The grammar's rules by id (error nodes' "<error>" is not one of them)
        return .{ .value = nameList(if (self._rules) |r| r.names[0..r.bytes.len] else &.{}) };
    }

    /// Label names by field id - 1.
    pub fn get_fields(self: *const Tree) pyoz.Signature(?*pyoz.PyObject, "list[str]") {
        return .{ .value = nameList(if (self._rules) |r| r.fields else &.{}) };
    }

    const CAPSULE_NAME = "zgram.tree.v1";

    fn capsuleFree(capsule: ?*pyoz.PyObject) callconv(.c) void {
        // The context is the Tree object the capsule kept alive
        const tree: ?*pyoz.PyObject = @ptrCast(@alignCast(py.c.PyCapsule_GetContext(capsule)));
        if (tree) |obj| py.Py_DecRef(obj);
    }

    /// A PyCapsule named "zgram.tree.v1" pointing to a parse_abi.TreeView of
    /// this tree, for native code. The capsule keeps the tree alive.
    pub fn get_capsule(const_self: *const Tree) pyoz.Signature(?*pyoz.PyObject, "object") {
        // (property getters receive a const pointer)
        const self: *Tree = @constCast(const_self);
        const compiled = self._compiled orelse {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        self._view = .{
            .node_count = self._count,
            .nodes = self._nodes,
            .input = self._input_ptr,
            .input_len = self._input_len,
            .rule_count = @intCast(compiled.rule_names.len),
            .field_count = @intCast(compiled.field_names.len),
            .rule_names = compiled.name_strs.ptr,
            .field_names = compiled.name_strs.ptr + compiled.rule_names.len,
        };
        const capsule = py.c.PyCapsule_New(&self._view, CAPSULE_NAME, &capsuleFree) orelse return .{ .value = null };
        const self_obj = Module.selfObject(Tree, self);
        py.Py_IncRef(self_obj);
        _ = py.c.PyCapsule_SetContext(capsule, self_obj);
        return .{ .value = capsule };
    }

    pub fn __del__(self: *Tree) void {
        if (self._nodes) |nodes| allocator.free(nodes[0..self._alloc_len]);
        self._nodes = null;
        if (self._input_obj) |obj| py.Py_DecRef(obj);
        self._input_obj = null;
        if (self._errors) |e| py.Py_DecRef(e);
        self._errors = null;
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

    /// Number of direct children of the node at `idx`. The stored count
    /// saturates, so nodes with more children are counted sibling by sibling.
    fn childCount(self: *Tree, idx: u32) u32 {
        const f = self.flat(idx) orelse return 0;
        if (f.child_count() < abi.CHILD_COUNT_MANY) return f.child_count();
        if (self._many_parent == idx) return self._many_count;
        const end = self.skip(idx);
        var n: u32 = 0;
        var ci: u32 = idx + 1;
        while (ci < end) : (n += 1) ci = self.skip(ci);
        self._many_parent = idx;
        self._many_count = n;
        return n;
    }

    fn ruleName(self: *const Tree, f: abi.FlatNode) []const u8 {
        const rules = self._rules orelse return "";
        const rid = f.rule_id();
        if (rid == rules.bytes.len) return ERROR_RULE_NAME;
        return if (rid < rules.bytes.len) rules.bytes[rid] else "";
    }

    pub const node__doc__: [*:0]const u8 = "Return the Node at an index of the node array (the inverse of Node.index). Raises IndexError when out of range.";
    pub const node__params__ = "index";
    pub const __doc__: [*:0]const u8 = "The result of one parse: the flat node array, the input and the rule and label names. Shared by all its Nodes.";
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
        const t = self._t orelse return 0;
        return t.childCount(self._idx);
    }

    /// Flat index of the child at `index`. Children are found by skipping
    /// sibling subtrees; sequential lookups resume from the previous one.
    fn childIndex(self: *const Node, index: u32) ?u32 {
        const t = self._t orelse return null;
        if (index >= t.childCount(self._idx)) return null;

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
        it._t = t;
        it._tree.set(Module.selfObject(Tree, t));
        it._next = self._idx + 1;
        it._remaining = t.childCount(self._idx);
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
        const t = self._t orelse return .{ .value = py.c.PyList_New(0) };
        const cc = t.childCount(self._idx);
        const list = py.c.PyList_New(cc) orelse return .{ .value = null };
        var ci: u32 = self._idx + 1;
        for (0..cc) |i| {
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

    /// The Tree this node belongs to.
    pub fn get_tree(self: *const Node) pyoz.Signature(?*pyoz.PyObject, "Tree") {
        const t = self._t orelse {
            py.Py_IncRef(py.Py_None());
            return .{ .value = py.Py_None() };
        };
        const obj = Module.selfObject(Tree, t);
        py.Py_IncRef(obj);
        return .{ .value = obj };
    }

    /// Index of this node in its tree's node array.
    pub fn get_index(self: *const Node) i64 {
        return self._idx;
    }

    /// The node this one is a child of, or None for the root. Nodes don't
    /// store their parent: it is found by descending from the root through
    /// the subtrees that contain this node.
    pub fn parent(self: *const Node) ?Node {
        const t = self._t orelse return null;
        if (self._idx == 0 or self._idx >= t._count) return null;
        var current: u32 = 0;
        while (true) {
            var kid = current + 1;
            const stop = t.skip(current);
            while (kid < stop) : (kid = t.skip(kid)) {
                if (kid == self._idx) return makeNode(t, current);
                if (self._idx < t.skip(kid)) break;
            }
            if (kid >= stop) return null;
            current = kid;
        }
    }

    /// The label this node was matched under in its parent rule, or None.
    pub fn field(self: *const Node) pyoz.Signature(?*pyoz.PyObject, "str | None") {
        const none = py.Py_None();
        if (self._t) |t| {
            if (t._rules) |table| {
                const id = (self.flat() orelse return .{ .value = null }).field_id();
                if (id != 0 and id <= table.fields.len) {
                    py.Py_IncRef(table.fields[id - 1]);
                    return .{ .value = table.fields[id - 1] };
                }
            }
        }
        py.Py_IncRef(none);
        return .{ .value = none };
    }

    /// Flat index of the first child labelled `id` at or after flat index
    /// `from` (a child of this node), or null.
    fn nextLabelled(self: *const Node, from: u32, id: u8) ?u32 {
        const t = self._t orelse return null;
        const stop = t.skip(self._idx);
        var ci = from;
        while (ci < stop) : (ci = t.skip(ci)) {
            if (t._nodes.?[ci].field_id() == id) return ci;
        }
        return null;
    }

    /// The first child matched under the label `name`, or None.
    pub fn get(self: *const Node, name: []const u8) ?Node {
        const t = self._t orelse return null;
        const id = (t._rules orelse return null).fieldIdOf(name);
        if (id == 0 or self._idx >= t._count) return null;
        return makeNode(t, self.nextLabelled(self._idx + 1, id) orelse return null);
    }

    /// All children matched under the label `name`, in order.
    pub fn get_all(self: *const Node, name: []const u8) pyoz.Signature(?*pyoz.PyObject, "list[Node]") {
        const list = py.c.PyList_New(0) orelse return .{ .value = null };
        const t = self._t orelse return .{ .value = list };
        const id = (t._rules orelse return .{ .value = list }).fieldIdOf(name);
        if (id == 0 or self._idx >= t._count) return .{ .value = list };
        var from = self._idx + 1;
        while (self.nextLabelled(from, id)) |ci| : (from = t.skip(ci)) {
            const obj = Module.toPy(Node, makeNode(t, ci)) orelse {
                py.Py_DecRef(list);
                return .{ .value = null };
            };
            defer py.Py_DecRef(obj);
            if (py.c.PyList_Append(list, obj) != 0) {
                py.Py_DecRef(list);
                return .{ .value = null };
            }
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

    /// Convert this subtree to values as the rules' `-> name` actions say
    /// (what parse_ast() does for the whole input).
    pub fn to_ast(self: *const Node, args: pyoz.Args(struct { spans: bool = true })) pyoz.Signature(?*pyoz.PyObject, "object") {
        const t = self._t orelse return .{ .value = null };
        const parser = t._parser_ptr orelse {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        return .{ .value = parser.buildAst(t, self._idx, args.value.spans) };
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
            const cc = t.childCount(i);
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
    pub const parent__doc__: [*:0]const u8 = "Return the node this one is a child of, or None for the root.";
    pub const field__doc__: [*:0]const u8 = "Return the label this node was matched under in its parent rule (label:rule in the grammar), or None.";
    pub const get__doc__: [*:0]const u8 = "Return the first child matched under a label, or None.";
    pub const get__params__ = "name";
    pub const get_all__doc__: [*:0]const u8 = "Return all children matched under a label, in order.";
    pub const get_all__params__ = "name";
    pub const child_count__doc__: [*:0]const u8 = "Return the number of child nodes.";
    pub const to_ast__doc__: [*:0]const u8 = "Convert this subtree to values as the rules' `-> name` actions say. Objects built by `-> Class` get __zspan__ = (start, end) and __znode__ = the node's index, unless spans=False.";
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

/// Python's repr() of `obj` (quotes escaped as Python does)
fn copyRepr(buf: []u8, pos: usize, obj: ?*pyoz.PyObject) usize {
    const r = py.c.PyObject_Repr(obj orelse py.Py_None()) orelse {
        py.c.PyErr_Clear();
        return copySlice(buf, pos, "?");
    };
    defer py.Py_DecRef(r);
    return copySlice(buf, pos, Diagnostic.utf8(r));
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
    /// The message when the error is a terminal failure found by
    /// diagnose.zig ("expected ';'"); empty for the kinds above
    text: [200]u8 = undefined,
    text_len: u8 = 0,
};

const GrammarParser = struct {
    /// Shared JIT-compiled grammar (one reference)
    _compiled: ?*Compiled = null,
    /// Interned rule names, created on first parse (needs the GIL)
    _rules: ?*RuleTable = null,
    _last_error: LastError = .{},
    /// The class (or callable) of each rule with a class action, once bound
    /// with compile(ast=...) or bind(); one entry per rule
    _classes: ?[*]?*pyoz.PyObject = null,

    fn dropClasses(self: *GrammarParser) void {
        const classes = self._classes orelse return;
        const n = if (self._compiled) |c| c.rule_names.len else 0;
        for (classes[0..n]) |cls| {
            if (cls) |obj| py.Py_DecRef(obj);
        }
        allocator.free(classes[0..n]);
        self._classes = null;
    }

    pub fn __del__(self: *GrammarParser) void {
        self.dropClasses();
        if (self._rules) |table| table.destroy();
        self._rules = null;
        if (self._compiled) |c| c.release();
        self._compiled = null;
    }

    fn ruleTable(self: *GrammarParser) !*RuleTable {
        if (self._rules) |r| return r;
        const compiled = self._compiled orelse return error.ParserNotLoaded;
        self._rules = try RuleTable.create(compiled.rule_names, compiled.field_names);
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
        // errorMessage may build its text in the scratch space, which is
        // already where the message goes
        const msg = errorMessage(self, err, buf[pos..]);
        pos += if (msg.ptr == buf[pos..].ptr) msg.len else copySlice(buf, pos, msg);
        return buf[0..pos];
    }

    fn errorMessage(self: *const GrammarParser, err: LastError, scratch: []u8) []const u8 {
        if (err.text_len != 0) {
            const n = @min(err.text_len, scratch.len);
            @memcpy(scratch[0..n], err.text[0..n]);
            return scratch[0..n];
        }
        return switch (err.kind) {
            .none => "",
            .trailing_input => "unexpected input after match",
            .out_of_memory => "out of memory (parse tree too large)",
            .too_deep => "nested too deeply to parse (the native stack is not deep enough)",
            .expected_rule => blk: {
                const compiled = self._compiled orelse break :blk "unexpected input";
                if (err.rule_id >= compiled.rule_names.len) break :blk "unexpected input";
                // Built in place at the start of scratch, then copied by the caller
                var tmp: [abi.MAX_RULE_NAME + 16]u8 = undefined;
                const name = compiled.display_names[err.rule_id];
                const msg = std.fmt.bufPrint(&tmp, "expected {s}", .{name}) catch break :blk "unexpected input";
                const n = @min(msg.len, scratch.len);
                @memcpy(scratch[0..n], msg[0..n]);
                break :blk scratch[0..n];
            },
        };
    }

    /// The generated parser reports the first rule that failed at the
    /// furthest position a rule started from. diagnose.zig finds the furthest
    /// failure of any kind and everything expected there (a missing ';' at
    /// the end of a statement: "expected ';'", where it was expected).
    /// Prefer that, except for input left over after a complete match with
    /// nothing failing beyond it.
    fn refineError(self: *GrammarParser, compiled: *const Compiled, input: []const u8, start_rule: u32) void {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const grammar = grammar_parser.parseGrammar(arena.allocator(), compiled.text) catch return;
        const found = diagnose.diagnose(allocator, grammar, input, start_rule) orelse return;
        if (found.pos < self._last_error.offset) return;
        if (found.pos == self._last_error.offset and self._last_error.kind != .expected_rule) return;

        const err = &self._last_error;
        const lc = abi.lineCol(input, found.pos);
        err.offset = @intCast(found.pos);
        err.line = lc.line;
        err.col = lc.col;
        err.text_len = @intCast(found.message(&err.text).len);
    }

    /// A syntax error as a Diagnostic object (null, with the Python error
    /// cleared, if it can't be created).
    fn syntaxDiagnostic(message: []const u8, err: LastError) ?*pyoz.PyObject {
        const severity = py.PyUnicode_FromStringAndSize("error", 5);
        const code = py.PyUnicode_FromStringAndSize("syntax", 6);
        const msg = py.PyUnicode_FromStringAndSize(message.ptr, @intCast(message.len));
        defer inline for (.{ severity, code, msg }) |o| {
            if (o) |obj| py.Py_DecRef(obj);
        };
        if (severity != null and code != null and msg != null) {
            if (Diagnostic.create(severity.?, code.?, msg.?, err.offset, err.offset, err.line, err.col, null)) |d| {
                if (Module.toPy(Diagnostic, d)) |obj| return obj;
                var dropped = d;
                dropped.__del__();
            }
        }
        py.c.PyErr_Clear();
        return null;
    }

    /// A Tree object that takes over `output`'s node buffer (trimmed if it's
    /// mostly unused), and its root Node. `errors`, a list of Diagnostic, is
    /// the tree's to keep.
    fn adoptTree(self: *GrammarParser, output: *abi.ParseOutput, input: *pyoz.PyObject, ptr: [*]const u8, input_len: usize, errors: ?*pyoz.PyObject) !?Node {
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
            ._rules = try self.ruleTable(),
            ._compiled = self._compiled,
            ._parser_ptr = self,
            ._errors = errors,
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

    /// Run the compiled parser. On success returns the root Node; on failure
    /// records the error and returns null (raising only if `raise_on_fail`).
    /// With `recover`, a syntax error doesn't fail the parse: see recoverParse().
    fn run(self: *GrammarParser, input: *pyoz.PyObject, start: ?*pyoz.PyObject, flags: u32, raise_on_fail: bool, recover: bool) !?Node {
        const compiled = self._compiled orelse return error.ParserNotLoaded;
        // (made here, with the GIL: the tree needs it)
        _ = try self.ruleTable();

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
        // Input nested deeper than this thread's stack allows is an error, not a crash
        output.stack_limit = native_stack.limit(@frameAddress());

        const rc = if (input_len >= GIL_RELEASE_BYTES)
            pyoz.allowThreads(callParse, .{ compiled.parse_fn, ptr, input_len, &output, start_rule, flags })
        else
            compiled.parse_fn(ptr, input_len, &output, start_rule, flags);
        if (rc < 0) {
            allocator.free(output.nodes_ptr.?[0..output.node_capacity]);
            output.nodes_ptr = null;
            return raise(py.PyExc_ValueError(), "unknown start rule");
        }

        // Out of stack: whatever the rules above the deepest one made of it
        // (an optional part may have "matched" nothing), the parse failed
        const too_deep = output.error_kind == @intFromEnum(abi.ErrorKind.too_deep);
        if (too_deep and output.status == 1) {
            output.status = 0;
            const at = @min(@as(usize, output.max_pos), input_len);
            const where = abi.lineCol(ptr[0..input_len], at);
            output.error_offset = @intCast(at);
            output.error_line = where.line;
            output.error_col = where.col;
        }

        if (output.status == 1) {
            self._last_error = .{};
            if (output.node_count == 0) {
                // A @silent start rule that matched without producing nodes
                allocator.free(output.nodes_ptr.?[0..output.node_capacity]);
                output.nodes_ptr = null;
                return raise(py.PyExc_ValueError(), "the start rule matched but produced no nodes (is it @silent?)");
            }
            return self.adoptTree(&output, input, ptr, input_len, null);
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

        // The error, as precisely as the whole input tells (refineError)
        if (!too_deep) self.refineError(compiled, ptr[0..input_len], start_rule);
        // A syntax error: with recover=True, parse again and skip the broken
        // text. The first error is this one; the others are diagnosed
        // around each one, in the recovered tree.
        if (recover and !too_deep) {
            return self.recoverParse(compiled, input, ptr, input_len, start_rule, output.error_offset, output.error_rule_id, self._last_error);
        }
        if (!raise_on_fail) return null;
        return self.raiseLastError();
    }

    /// Raise ParseError for the last failed parse. Always null.
    fn raiseLastError(self: *GrammarParser) ?Node {
        var msg_buf: [320]u8 = undefined;
        const msg = self.formatError(self._last_error, msg_buf[0 .. msg_buf.len - 1]);
        msg_buf[msg.len] = 0;
        Module.getException(0).raise(msg_buf[0..msg.len :0]);
        self.annotateException();
        return null;
    }

    /// Most syntax errors recovered from in one parse; past them, the rest
    /// of the input is one error node
    const MAX_RECOVERED = 100;

    /// The parse failed with a syntax error at `first` (the generated
    /// parser's furthest failure, where it expected `first_rule`). Parse
    /// again with the recovering parser: a repetition whose element runs into
    /// a known error skips the broken text into an error node and goes on.
    /// Each round knows the errors found so far; a round that stops at a new
    /// one adds it and runs again, until the input parses to its end, up to
    /// MAX_RECOVERED errors. A round that finds nothing new raises the
    /// recover level (ParseOutput.recover_level) for the rounds after it.
    /// What can't be recovered from (an error no repetition reaches) ends
    /// the tree with an error node for the rest of the input. Returns the
    /// root of a tree whose `errors` lists them all.
    fn recoverParse(self: *GrammarParser, compiled: *Compiled, input: *pyoz.PyObject, ptr: [*]const u8, input_len: usize, start_rule: u32, first: u32, first_rule: u16, plain: LastError) !?Node {
        const parse_fn = try pyoz.allowThreadsTry(Compiled.recoverer, .{compiled});
        // The errors known so far: their positions (sorted, for the parser),
        // and per position what was expected there, and whether it is only a
        // more precise position of an error already listed (where a literal
        // was missing), which recovery uses but which isn't reported again
        var known: std.ArrayList(u32) = .empty;
        defer known.deinit(allocator);
        var info: std.ArrayList(KnownError) = .empty;
        defer info.deinit(allocator);
        _ = try addKnown(&known, &info, first, .{ .rule = first_rule });
        var reported: usize = 1;
        var recovered: [MAX_RECOVERED * 4]abi.Recovered = undefined;
        var inserted: [MAX_RECOVERED * 4]abi.Inserted = undefined;
        var level: u8 = 0;

        while (true) {
            var output = abi.ParseOutput{};
            defer jit_helpers.memoFree(&output);
            defer if (output.nodes_ptr) |n| allocator.free(n[0..output.node_capacity]);
            const initial_cap: u32 = @intCast(std.math.clamp(input_len / 4 + 16, 16, 64 * 1024));
            output.nodes_ptr = (try allocator.alloc(abi.FlatNode, initial_cap)).ptr;
            output.node_capacity = initial_cap;
            output.stack_limit = native_stack.limit(@frameAddress());
            output.known_errors = known.items.ptr;
            output.known_count = @intCast(known.items.len);
            output.error_rule = @intCast(compiled.rule_names.len);
            output.recover_level = level;
            output.recovered = &recovered;
            output.recovered_capacity = recovered.len;
            output.inserted = &inserted;
            output.inserted_capacity = inserted.len;
            _ = if (input_len >= GIL_RELEASE_BYTES)
                pyoz.allowThreads(callParse, .{ parse_fn, ptr, input_len, &output, start_rule, 0 })
            else
                parse_fn(ptr, input_len, &output, start_rule, 0);

            const kind: abi.ErrorKind = @enumFromInt(output.error_kind);
            if (kind == .out_of_memory) return error.OutOfMemory;
            if (kind == .too_deep) {
                const at = @min(@as(usize, output.max_pos), input_len);
                const where = abi.lineCol(ptr[0..input_len], at);
                self._last_error = .{ .kind = .too_deep, .offset = @intCast(at), .line = where.line, .col = where.col };
                return self.raiseLastError();
            }
            const recovered_list = recovered[0..output.recovered_count];
            if (output.status == 1 and output.node_count > 0) {
                return self.finishRecovery(&output, input, ptr, input_len, known.items, info.items, recovered_list, first, plain);
            }

            // Stopped at an error: a new one is added and the parse runs
            // again; so is the precise place a literal was missing there, if
            // further on (where a `=` or `}` can be inserted)
            const at = output.error_offset;
            const literal: ?[]const u8 = if (output.lit_text) |t| t[0..output.lit_len] else null;
            const new_error = try addKnown(&known, &info, at, .{ .rule = output.error_rule_id, .literal = if (output.lit_pos == at) literal else null });
            if (new_error) reported += 1;
            const new_position = output.lit_pos > at and try addKnown(&known, &info, output.lit_pos, .{ .rule = output.error_rule_id, .alias = true, .literal = literal });
            if ((new_error or new_position) and reported <= MAX_RECOVERED) continue;
            // Nothing new: from now on skip more freely (skipped text may run
            // to the end of the input; then elements that ran into a known
            // error may be skipped even if they failed further on)
            if (level < abi.RECOVER_LOOSE and reported <= MAX_RECOVERED) {
                level += 1;
                continue;
            }

            // No way on: the rest of the input becomes one error node
            try endWithError(&output, input_len);
            return self.finishRecovery(&output, input, ptr, input_len, known.items, info.items, recovered_list, first, plain);
        }
    }

    /// The nodes of the tree in `output` whose text contains `at`, innermost
    /// first, as (start, rule) to diagnose from; and the start of the error
    /// node `at` is in, if it is in one, with the node before it (its
    /// previous sibling). Found by descending from the root, so it costs the
    /// depth, not the tree.
    fn nodesAround(output: *const abi.ParseOutput, at: u32, out: []abi.Recovered) Around {
        const nodes = (output.nodes_ptr orelse return .{})[0..output.node_count];
        if (nodes.len == 0) return .{};
        var result = Around{};
        var chain: [64]u32 = undefined;
        var depth: usize = 0;
        var idx: u32 = 0;
        var prev: ?abi.FlatNode = null;
        descend: while (depth < chain.len) {
            const node = nodes[idx];
            if (node.text_start > at or node.text_end < at) break;
            if (node.rule_id() == output.error_rule) {
                result.error_start = node.text_start;
                result.before_error = prev;
            } else {
                chain[depth] = idx;
                depth += 1;
            }
            // Into the child that contains it
            const end = idx + 1 + node.subtree_size;
            var child = idx + 1;
            prev = null;
            while (child < end) : (child += nodes[child].subtree_size + 1) {
                if (nodes[child].text_start <= at and at <= nodes[child].text_end) {
                    idx = child;
                    continue :descend;
                }
                prev = nodes[child];
            }
            break;
        }
        while (depth > 0 and result.count < out.len) : (result.count += 1) {
            depth -= 1;
            const node = nodes[chain[depth]];
            out[result.count] = .{ .start = node.text_start, .rule = node.rule_id() };
        }
        return result;
    }

    const Around = struct {
        count: usize = 0,
        error_start: ?u32 = null,
        before_error: ?abi.FlatNode = null,
    };

    fn allSpace(text: []const u8) bool {
        for (text) |ch| {
            if (ch != ' ' and ch != '\t' and ch != '\r' and ch != '\n') return false;
        }
        return true;
    }

    /// A known syntax error (see recoverParse)
    const KnownError = struct {
        /// What the generated parser expected there
        rule: u16,
        alias: bool = false,
        /// The literal found missing there, if one was (in the compiled
        /// grammar's memory)
        literal: ?[]const u8 = null,
    };

    /// Add a known error position in order; false if it was known already.
    fn addKnown(known: *std.ArrayList(u32), info: *std.ArrayList(KnownError), at: u32, what: KnownError) !bool {
        const index = std.sort.lowerBound(u32, known.items, at, orderU32);
        if (index < known.items.len and known.items[index] == at) return false;
        try known.insert(allocator, index, at);
        try info.insert(allocator, index, what);
        return true;
    }

    fn orderU32(a: u32, b: u32) std.math.Order {
        return std.math.order(a, b);
    }

    /// A tree for a parse that stopped at an error it couldn't recover
    /// from: the start rule matched part of the input (its node gets an error
    /// node for the rest as its last child), or nothing (the whole input is
    /// one error node).
    fn endWithError(output: *abi.ParseOutput, input_len: usize) !void {
        const error_rule: u16 = @intCast(output.error_rule);
        const end: u32 = @intCast(input_len);
        // The start rule matched a prefix exactly when it left its node (a
        // failed rule takes its nodes back); the error kind can't tell,
        // since input left over is reported where the furthest failure was
        if (output.node_count > 0) {
            if (jit_helpers.zgram_ensure_capacity(output, output.node_count + 1) == 0) return error.OutOfMemory;
            const nodes = output.nodes_ptr.?;
            const root = &nodes[0];
            nodes[output.node_count] = .{
                .text_start = root.text_end,
                .text_end = end,
                .subtree_size = 0,
                .meta = abi.FlatNode.packMeta(0, error_rule),
            };
            output.node_count += 1;
            root.subtree_size += 1;
            root.text_end = end;
            // One more child (the stored count saturates)
            if (root.child_count() < abi.CHILD_COUNT_MANY) root.meta += 1;
            return;
        }
        output.node_count = 1;
        output.nodes_ptr.?[0] = .{ .text_start = 0, .text_end = end, .subtree_size = 0, .meta = abi.FlatNode.packMeta(0, error_rule) };
    }

    /// The tree of a recovered parse, with a Diagnostic per error: the first
    /// (`plain`, found at `first`) as a plain parse reports it; each later
    /// one with what the nodes around it expected (diagnose.zig, run from
    /// their start), or else the literal inserted or the rule expected there.
    fn finishRecovery(self: *GrammarParser, output: *abi.ParseOutput, input: *pyoz.PyObject, ptr: [*]const u8, input_len: usize, known: []const u32, info: []const KnownError, recovered: []const abi.Recovered, first: u32, plain: LastError) !?Node {
        const compiled = self._compiled.?;
        const text = ptr[0..input_len];
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const grammar = grammar_parser.parseGrammar(arena.allocator(), compiled.text) catch null;

        // (`plain` wins over another error at its position)
        const Entry = struct { offset: u32, message: []const u8, plain: bool = false };
        var entries: std.ArrayList(Entry) = .empty;
        const insertions: []const abi.Inserted = if (output.inserted) |list| list[0..output.inserted_count] else &.{};
        for (known, info) |known_at, what| {
            // (only a more precise position of an error listed on its own)
            if (what.alias) continue;
            if (known_at == first) {
                var buf: [200]u8 = undefined;
                const message = self.errorMessage(plain, &buf);
                try entries.append(arena.allocator(), .{ .offset = plain.offset, .message = try arena.allocator().dupe(u8, message), .plain = true });
                continue;
            }
            const rule = what.rule;
            // A literal inserted for this error, across whitespace from it:
            // where it was missing is the precise place (`)` missing after
            // the space the furthest failure was at)
            var at = known_at;
            var inserted_literal: ?[]const u8 = null;
            for (insertions) |ins| {
                const lo = @min(ins.pos, known_at);
                const hi = @max(ins.pos, known_at);
                if (hi > text.len or !allSpace(text[lo..hi])) continue;
                at = ins.pos;
                inserted_literal = ins.text[0..ins.len];
                break;
            }
            // What to diagnose from, most specific first: the skipped element
            // that broke here, then the nodes
            // of the tree around the error, innermost first (an inserted `;`
            // leaves a statement node rather than a skipped element)
            var candidates: [11]abi.Recovered = undefined;
            var n: usize = 0;
            var around: [9]abi.Recovered = undefined;
            const found_around = nodesAround(output, at, &around);
            // The skipped element: the one recorded at the start of the error
            // node the error is in (records of nodes that backtracking
            // dropped don't count)
            if (found_around.error_start) |error_start| {
                var i = recovered.len;
                while (i > 0) {
                    i -= 1;
                    if (recovered[i].start != error_start) continue;
                    if (recovered[i].rule == abi.NO_RULE) break;
                    // With a literal inserted right there, the error is of
                    // the element before, which got it (`x = 1;` read as
                    // `x;` and `= 1;`): what it expected comes first
                    if (inserted_literal != null) {
                        if (found_around.before_error) |before| {
                            if (before.text_end <= at and allSpace(text[before.text_end..at])) {
                                candidates[n] = .{ .start = before.text_start, .rule = recovered[i].rule };
                                n += 1;
                            }
                        }
                    }
                    candidates[n] = recovered[i];
                    n += 1;
                    break;
                }
            }
            @memcpy(candidates[n..][0..found_around.count], around[0..found_around.count]);
            n += found_around.count;
            const found: ?diagnose.Result = if (grammar) |g| blk: {
                // With a literal inserted there, the first node whose rule
                // expected it (a missing `end` belongs to the statement it
                // ends, not to the block inside); else the first that gets
                // as far as the error
                var reached: ?diagnose.Result = null;
                for (candidates[0..n]) |c| {
                    // (a diagnosis that stops short of the error is of another one)
                    const r = diagnose.diagnoseAt(allocator, g, text, c.rule, c.start) orelse continue;
                    if (r.pos < at) continue;
                    const lit = inserted_literal orelse break :blk r;
                    if (r.pos == at and r.expects(lit)) break :blk r;
                    if (reached == null) reached = r;
                }
                break :blk reached;
            } else null;
            if (found) |r| {
                var buf: [200]u8 = undefined;
                try entries.append(arena.allocator(), .{ .offset = @intCast(r.pos), .message = try arena.allocator().dupe(u8, r.message(&buf)) });
                continue;
            }
            // Else the literal the parser would have inserted there, or the
            // rule it expected
            var buf: [200]u8 = undefined;
            const message = if (what.literal orelse inserted_literal) |lit|
                diagnose.expectedLiteral(lit, &buf)
            else
                try std.fmt.bufPrint(&buf, "expected {s}", .{if (rule < compiled.display_names.len) compiled.display_names[rule] else "input"});
            try entries.append(arena.allocator(), .{ .offset = at, .message = try arena.allocator().dupe(u8, message) });
        }

        // In source order, one per position
        std.sort.pdq(Entry, entries.items, {}, struct {
            fn lt(_: void, a: Entry, b: Entry) bool {
                return if (a.offset != b.offset) a.offset < b.offset else a.plain and !b.plain;
            }
        }.lt);
        const list = py.c.PyList_New(0) orelse return null;
        errdefer py.Py_DecRef(list);
        var last: ?u32 = null;
        for (entries.items) |e| {
            if (last != null and last.? == e.offset) continue;
            last = e.offset;
            const where = abi.lineCol(text, e.offset);
            var err = LastError{ .kind = .expected_rule, .offset = e.offset, .line = where.line, .col = where.col };
            const n = @min(e.message.len, err.text.len);
            @memcpy(err.text[0..n], e.message[0..n]);
            err.text_len = @intCast(n);
            // parser.error: the error a plain parse raises
            if (e.plain) self._last_error = err;
            const d = syntaxDiagnostic(e.message, err) orelse return null;
            defer py.Py_DecRef(d);
            if (py.c.PyList_Append(list, d) != 0) return null;
        }
        return self.adoptTree(output, input, ptr, input_len, list);
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
            var scratch: [256]u8 = undefined;
            const message = self.errorMessage(err, &scratch);
            const attrs = [_]struct { [*:0]const u8, ?*pyoz.PyObject }{
                .{ "line", py.c.PyLong_FromUnsignedLong(err.line) },
                .{ "column", py.c.PyLong_FromUnsignedLong(err.col) },
                .{ "offset", py.c.PyLong_FromUnsignedLong(err.offset) },
                .{ "message", py.PyUnicode_FromStringAndSize(message.ptr, @intCast(message.len)) },
                .{ "diagnostic", syntaxDiagnostic(message, err) },
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

    // ── AST building ──

    /// Raise `exc` with "<before><name><after>".
    fn raiseNamed(exc: *pyoz.PyObject, before: []const u8, name: []const u8, after: []const u8) void {
        var buf: [512]u8 = undefined;
        var pos: usize = 0;
        pos += copySlice(buf[0 .. buf.len - 1], pos, before);
        pos += copySlice(buf[0 .. buf.len - 1], pos, name);
        pos += copySlice(buf[0 .. buf.len - 1], pos, after);
        buf[pos] = 0;
        py.PyErr_SetString(exc, @ptrCast(&buf));
    }

    /// Look up the class of every `-> Class` rule in `ast` (a mapping or any
    /// object with attributes, such as a module). Sets a Python error and
    /// returns false if one is missing.
    fn bindClasses(self: *GrammarParser, ast: *pyoz.PyObject) bool {
        const compiled = self._compiled orelse {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return false;
        };
        const n = compiled.rule_names.len;
        const classes = allocator.alloc(?*pyoz.PyObject, n) catch {
            _ = py.c.PyErr_NoMemory();
            return false;
        };
        @memset(classes, null);
        var ok = true;
        for (compiled.actions, 0..) |action, i| {
            if (action != .class) continue;
            const name = compiled.action_names[i];
            const key = py.PyUnicode_FromStringAndSize(name.ptr, @intCast(name.len)) orelse {
                ok = false;
                break;
            };
            defer py.Py_DecRef(key);
            classes[i] = if (py.PyDict_Check(ast)) py.c.PyObject_GetItem(ast, key) else py.PyObject_GetAttr(ast, key);
            if (classes[i] == null) {
                py.c.PyErr_Clear();
                raiseNamed(py.PyExc_ValueError(), "ast has no '", name, "' (used as `-> name` in the grammar)");
                ok = false;
                break;
            }
        }
        if (!ok) {
            for (classes) |cls| {
                if (cls) |obj| py.Py_DecRef(obj);
            }
            allocator.free(classes);
            return false;
        }
        self.dropClasses();
        self._classes = classes.ptr;
        return true;
    }

    /// Bind the classes named by `-> Class` actions: `ast` is a dict or an
    /// object with them as attributes (a module, a namespace).
    pub fn bind(self: *GrammarParser, ast: *pyoz.PyObject) pyoz.Signature(?*pyoz.PyObject, "None") {
        if (!self.bindClasses(ast)) return .{ .value = null };
        py.Py_IncRef(py.Py_None());
        return .{ .value = py.Py_None() };
    }

    /// Parse the whole input and return the Tree (see Node.tree). With
    /// recover=True a syntax error doesn't raise: the broken text becomes
    /// error nodes and tree.errors lists the errors.
    pub fn parse_tree(self: *GrammarParser, args: pyoz.Args(struct { input: *pyoz.PyObject, start: ?*pyoz.PyObject = null, recover: bool = false })) pyoz.Signature(anyerror!?*pyoz.PyObject, "Tree") {
        const root = (self.run(args.value.input, args.value.start, 0, true, args.value.recover) catch |e| return .{ .value = e }) orelse return .{ .value = null };
        var node = root;
        defer node._tree.clear();
        const obj = Module.selfObject(Tree, root._t.?);
        py.Py_IncRef(obj);
        return .{ .value = obj };
    }

    /// Parse the whole input and convert the tree to values, bottom-up, as
    /// the rules' `-> name` actions say. With recover=True a syntax error
    /// doesn't raise, and the broken text is None in the result.
    pub fn parse_ast(self: *GrammarParser, args: pyoz.Args(struct { input: *pyoz.PyObject, start: ?*pyoz.PyObject = null, spans: bool = true, recover: bool = false })) pyoz.Signature(anyerror!?*pyoz.PyObject, "object") {
        const root = (self.run(args.value.input, args.value.start, 0, true, args.value.recover) catch |e| return .{ .value = e }) orelse return .{ .value = null };
        var node = root;
        defer node._tree.clear();
        return .{ .value = self.buildAst(root._t.?, root._idx, args.value.spans) };
    }

    /// Convert the subtree at `root` to a value (a new reference; None if the
    /// root is dropped). On failure sets a Python error and returns null.
    fn buildAst(self: *GrammarParser, t: *Tree, root: u32, spans: bool) ?*pyoz.PyObject {
        const compiled = self._compiled.?;
        const nodes = t._nodes.?;

        // Reverse pre-order: when a node is reached its children's values are
        // the top entries of the stack, first child on top. null = dropped.
        var stack: std.ArrayList(?*pyoz.PyObject) = .empty;
        defer {
            for (stack.items) |v| {
                if (v) |o| py.Py_DecRef(o);
            }
            stack.deinit(allocator);
        }

        var i: u32 = @intCast(@min(@as(u64, root) + nodes[root].subtree_size, t._count - 1));
        while (true) : (i -= 1) {
            const cc = t.childCount(i);
            if (stack.items.len < cc) {
                py.PyErr_SetString(py.PyExc_RuntimeError(), "corrupt parse tree");
                return null;
            }
            const kids = stack.items[stack.items.len - cc ..];
            const value = self.convert(compiled, t, i, kids, spans) catch return null;
            for (kids) |v| {
                if (v) |o| py.Py_DecRef(o);
            }
            stack.items.len -= cc;
            stack.append(allocator, value) catch {
                if (value) |o| py.Py_DecRef(o);
                _ = py.c.PyErr_NoMemory();
                return null;
            };
            if (i == root) break;
        }
        const result = stack.pop().? orelse py.Py_None();
        if (result == py.Py_None()) py.Py_IncRef(result);
        return result;
    }

    const BuildError = error{PythonError};

    /// `-> unquote`: the string a quoted literal stands for. Drops the first
    /// and last byte (the quotes) and replaces backslash escapes: \n \t \r
    /// \b \f \0, \xHH, \uHHHH (UTF-16 surrogate pairs combine), and any
    /// other escaped character stands for itself (\" \' \\ \/).
    fn unquote(text: []const u8) ?*pyoz.PyObject {
        const inner = if (text.len >= 2) text[1 .. text.len - 1] else "";
        if (std.mem.indexOfScalar(u8, inner, '\\') == null) return py.PyUnicode_FromStringAndSize(inner.ptr, @intCast(inner.len));

        // Every escape is at least as long as the UTF-8 it stands for
        var stack_buf: [512]u8 = undefined;
        const out = if (inner.len <= stack_buf.len) stack_buf[0..inner.len] else allocator.alloc(u8, inner.len) catch {
            _ = py.c.PyErr_NoMemory();
            return null;
        };
        defer if (out.ptr != &stack_buf) allocator.free(out);

        var n: usize = 0;
        var i: usize = 0;
        while (i < inner.len) {
            if (inner[i] != '\\' or i + 1 == inner.len) {
                out[n] = inner[i];
                n += 1;
                i += 1;
                continue;
            }
            const esc = inner[i + 1];
            i += 2;
            var cp: u21 = switch (esc) {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                'b' => 0x08,
                'f' => 0x0C,
                '0' => 0,
                'x', 'u' => blk: {
                    const digits: usize = if (esc == 'x') 2 else 4;
                    if (i + digits > inner.len) break :blk esc;
                    const v = std.fmt.parseInt(u16, inner[i..][0..digits], 16) catch break :blk esc;
                    i += digits;
                    break :blk v;
                },
                else => esc,
            };
            // A high surrogate followed by an escaped low one is one character
            if (cp >= 0xD800 and cp < 0xDC00 and i + 6 <= inner.len and inner[i] == '\\' and inner[i + 1] == 'u') {
                if (std.fmt.parseInt(u16, inner[i + 2 ..][0..4], 16)) |low| {
                    if (low >= 0xDC00 and low < 0xE000) {
                        cp = 0x10000 + ((cp - 0xD800) << 10) + (low - 0xDC00);
                        i += 6;
                    }
                } else |_| {}
            }
            // Encode by hand: a lone surrogate must get through (surrogatepass below)
            if (cp < 0x80) {
                out[n] = @intCast(cp);
                n += 1;
            } else if (cp < 0x800) {
                out[n] = @intCast(0xC0 | (cp >> 6));
                out[n + 1] = @intCast(0x80 | (cp & 0x3F));
                n += 2;
            } else if (cp < 0x10000) {
                out[n] = @intCast(0xE0 | (cp >> 12));
                out[n + 1] = @intCast(0x80 | ((cp >> 6) & 0x3F));
                out[n + 2] = @intCast(0x80 | (cp & 0x3F));
                n += 3;
            } else {
                out[n] = @intCast(0xF0 | (cp >> 18));
                out[n + 1] = @intCast(0x80 | ((cp >> 12) & 0x3F));
                out[n + 2] = @intCast(0x80 | ((cp >> 6) & 0x3F));
                out[n + 3] = @intCast(0x80 | (cp & 0x3F));
                n += 4;
            }
        }
        return py.c.PyUnicode_DecodeUTF8(out.ptr, @intCast(n), "surrogatepass");
    }

    /// The kept (non-dropped) values of `kids` in child order, as a list or tuple.
    fn collect(kids: []const ?*pyoz.PyObject, as_tuple: bool) BuildError!*pyoz.PyObject {
        var n: usize = 0;
        for (kids) |v| n += @intFromBool(v != null);
        const seq = (if (as_tuple) py.c.PyTuple_New(@intCast(n)) else py.c.PyList_New(@intCast(n))) orelse return error.PythonError;
        var k: py.Py_ssize_t = 0;
        var j = kids.len;
        while (j > 0) {
            j -= 1;
            const v = kids[j] orelse continue;
            py.Py_IncRef(v);
            // Both steal the reference
            _ = if (as_tuple) py.c.PyTuple_SetItem(seq, k, v) else py.c.PyList_SetItem(seq, k, v);
            k += 1;
        }
        return seq;
    }

    /// The value of node `i` given its children's values (`kids`, first child
    /// last). Returns a new reference, or null for a dropped node.
    fn convert(self: *GrammarParser, compiled: *const Compiled, t: *Tree, i: u32, kids: []const ?*pyoz.PyObject, spans: bool) BuildError!?*pyoz.PyObject {
        const n = t._nodes.?[i];
        const rid = n.rule_id();
        // An error node (recover=True): None, where the broken text was
        if (rid == compiled.actions.len) {
            py.Py_IncRef(py.Py_None());
            return py.Py_None();
        }
        if (rid > compiled.actions.len) {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "corrupt parse tree");
            return error.PythonError;
        }
        const s = @min(n.text_start, t._input_len);
        const e = @min(@max(n.text_end, s), t._input_len);
        const text_ptr = t._input_ptr.? + s;
        const text_len: py.Py_ssize_t = @intCast(e - s);

        switch (compiled.actions[rid]) {
            .none => {
                if (kids.len == 0) return py.PyUnicode_FromStringAndSize(text_ptr, text_len) orelse error.PythonError;
                if (kids.len == 1) {
                    if (kids[0]) |v| py.Py_IncRef(v);
                    return kids[0];
                }
                return try collect(kids, false);
            },
            .str => return py.PyUnicode_FromStringAndSize(text_ptr, text_len) orelse error.PythonError,
            .int, .float => |action| {
                // Plain decimal numbers convert natively; anything else goes
                // through Python's int()/float() for its exact rules and errors
                const bytes = text_ptr[0..@intCast(text_len)];
                if (action == .int and bytes.len <= 18) {
                    if (std.fmt.parseInt(i64, bytes, 10)) |v| {
                        if (std.mem.indexOfScalar(u8, bytes, '_') == null and bytes[0] != '+')
                            return py.c.PyLong_FromLongLong(v) orelse error.PythonError;
                    } else |_| {}
                } else if (action == .float and bytes.len <= 32 and std.mem.indexOfNone(u8, bytes, "0123456789.-eE+") == null) {
                    if (std.fmt.parseFloat(f64, bytes)) |v| {
                        return py.c.PyFloat_FromDouble(v) orelse error.PythonError;
                    } else |_| {}
                }
                const text = py.PyUnicode_FromStringAndSize(text_ptr, text_len) orelse return error.PythonError;
                defer py.Py_DecRef(text);
                return (if (action == .int) py.c.PyNumber_Long(text) else py.c.PyFloat_FromString(text)) orelse error.PythonError;
            },
            .true, .false, .null => |action| {
                const obj = switch (action) {
                    .true => py.Py_True(),
                    .false => py.Py_False(),
                    else => py.Py_None(),
                };
                py.Py_IncRef(obj);
                return obj;
            },
            .unquote => return unquote(text_ptr[0..@intCast(text_len)]) orelse error.PythonError,
            .list => return try collect(kids, false),
            .tuple => return try collect(kids, true),
            .dict => {
                const dict = py.c.PyDict_New() orelse return error.PythonError;
                errdefer py.Py_DecRef(dict);
                var j = kids.len;
                while (j > 0) {
                    j -= 1;
                    const pair = kids[j] orelse continue;
                    if (!py.PyTuple_Check(pair) or py.PyTuple_Size(pair) != 2) {
                        raiseNamed(py.PyExc_TypeError(), "rule '", compiled.rule_names[rid], "' -> dict: every child must be a (key, value) tuple (e.g. a rule with `-> tuple`)");
                        return error.PythonError;
                    }
                    if (py.PyDict_SetItem(dict, py.PyTuple_GetItem(pair, 0).?, py.PyTuple_GetItem(pair, 1).?) != 0) return error.PythonError;
                }
                return dict;
            },
            .first => {
                var j = kids.len;
                while (j > 0) {
                    j -= 1;
                    if (kids[j]) |v| {
                        py.Py_IncRef(v);
                        return v;
                    }
                }
                py.Py_IncRef(py.Py_None());
                return py.Py_None();
            },
            .drop => return null,
            .class => {},
        }

        // -> Class: labelled children become keyword arguments. A rule with
        // no labels passes its children's values positionally, or its text
        // if it can't have children.
        const cls = (if (self._classes) |c| c[rid] else null) orelse {
            raiseNamed(py.PyExc_ValueError(), "the grammar uses `-> ", compiled.action_names[rid], "`: compile with ast=... (or call bind()) before parse_ast()");
            return error.PythonError;
        };
        const rule_labels = compiled.labels[compiled.label_start[rid]..compiled.label_start[rid + 1]];
        const table = t._rules.?;

        var any_labelled = rule_labels.len != 0;
        var ci: u32 = i + 1;
        for (0..kids.len) |_| {
            if (t._nodes.?[ci].field_id() != 0) any_labelled = true;
            ci = t.skip(ci);
        }

        var obj: *pyoz.PyObject = undefined;
        if (compiled.no_args[rid]) {
            const no_args = py.c.PyTuple_New(0) orelse return error.PythonError;
            defer py.Py_DecRef(no_args);
            obj = py.c.PyObject_Call(cls, no_args, null) orelse return error.PythonError;
        } else if (any_labelled) {
            const kwargs = py.c.PyDict_New() orelse return error.PythonError;
            defer py.Py_DecRef(kwargs);
            for (rule_labels) |l| {
                const initial = if (l.many) py.c.PyList_New(0) orelse return error.PythonError else py.Py_None();
                defer if (l.many) py.Py_DecRef(initial);
                if (py.PyDict_SetItem(kwargs, table.fields[l.field - 1], initial) != 0) return error.PythonError;
            }
            ci = i + 1;
            var j = kids.len;
            while (j > 0) : (ci = t.skip(ci)) {
                j -= 1;
                const field = t._nodes.?[ci].field_id();
                const v = kids[j] orelse continue;
                if (field == 0 or field > table.fields.len) continue;
                const key = table.fields[field - 1];
                var many = false;
                for (rule_labels) |l| {
                    if (l.field == field) many = l.many;
                }
                if (many) {
                    // Borrowed reference to the list created above
                    const list = py.c.PyDict_GetItem(kwargs, key) orelse return error.PythonError;
                    if (py.PyList_Append(list, v) != 0) return error.PythonError;
                } else if (py.PyDict_SetItem(kwargs, key, v) != 0) return error.PythonError;
            }
            const no_args = py.c.PyTuple_New(0) orelse return error.PythonError;
            defer py.Py_DecRef(no_args);
            obj = py.c.PyObject_Call(cls, no_args, kwargs) orelse return error.PythonError;
        } else {
            const call_args = if (compiled.has_children[rid]) try collect(kids, true) else blk: {
                const text = py.PyUnicode_FromStringAndSize(text_ptr, text_len) orelse return error.PythonError;
                defer py.Py_DecRef(text);
                const tuple = py.c.PyTuple_New(1) orelse return error.PythonError;
                py.Py_IncRef(text);
                _ = py.c.PyTuple_SetItem(tuple, 0, text);
                break :blk tuple;
            };
            defer py.Py_DecRef(call_args);
            obj = py.c.PyObject_Call(cls, call_args, null) orelse return error.PythonError;
        }

        if (spans) {
            // Best effort: objects that can't take attributes go without
            const span = py.c.PyTuple_New(2);
            if (span) |sp| {
                _ = py.c.PyTuple_SetItem(sp, 0, py.c.PyLong_FromUnsignedLong(n.text_start));
                _ = py.c.PyTuple_SetItem(sp, 1, py.c.PyLong_FromUnsignedLong(n.text_end));
            }
            if (span == null or py.PyObject_SetAttr(obj, table.span_attr, span.?) != 0) py.c.PyErr_Clear();
            if (span) |sp| py.Py_DecRef(sp);
            const index = py.c.PyLong_FromUnsignedLong(i);
            if (index == null or py.PyObject_SetAttr(obj, table.node_attr, index.?) != 0) py.c.PyErr_Clear();
            if (index) |ix| py.Py_DecRef(ix);
        }
        return obj;
    }

    fn raise(exc: *pyoz.PyObject, msg: [*:0]const u8) ?Node {
        py.PyErr_SetString(exc, msg);
        return null;
    }

    fn callParse(f: abi.ParseFn, ptr: [*]const u8, len: usize, out: *abi.ParseOutput, start_rule: u32, flags: u32) i32 {
        return f(ptr, len, out, start_rule, flags);
    }

    /// Parse the whole input. Returns the root Node; raises ParseError on
    /// failure, unless recover=True (see parse_tree).
    pub fn parse(self: *GrammarParser, args: pyoz.Args(struct { input: *pyoz.PyObject, start: ?*pyoz.PyObject = null, recover: bool = false })) pyoz.Signature(anyerror!?Node, "Node") {
        return .{ .value = self.run(args.value.input, args.value.start, 0, true, args.value.recover) };
    }

    /// Match a prefix of the input. Returns the root Node (its end() is where
    /// the match stopped), or None if the start rule doesn't match.
    pub fn match(self: *GrammarParser, args: pyoz.Args(struct { input: *pyoz.PyObject, start: ?*pyoz.PyObject = null })) !?Node {
        return self.run(args.value.input, args.value.start, abi.FLAG_PREFIX, false, false);
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
        output.stack_limit = native_stack.limit(@frameAddress());
        const rc = if (input_len >= GIL_RELEASE_BYTES)
            pyoz.allowThreads(callParse, .{ validate, ptr, input_len, &output, start_rule, 0 })
        else
            validate(ptr, input_len, &output, start_rule, 0);
        if (rc < 0) return error.ParseFailed;
        if (output.status == 1 and output.error_kind != @intFromEnum(abi.ErrorKind.too_deep)) {
            self._last_error = .{};
            return true;
        }

        // Rejected: let the tree parser explain why (sets `error`). It
        // agrees with the validator, so it returns no node; if it ever did,
        // release the node's reference to its tree.
        const node = self.run(input, start, 0, false, false) catch {
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
        if (msg.ptr != &e._message) @memcpy(e._message[0..msg.len], msg);
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
        // (not the name error nodes get, after the grammar's rules)
        const rule_names = tbl.names[0..tbl.bytes.len];
        const list = py.c.PyList_New(@intCast(rule_names.len)) orelse return .{ .value = null };
        for (rule_names, 0..) |n, i| {
            py.Py_IncRef(n);
            _ = py.c.PyList_SetItem(list, @intCast(i), n);
        }
        return .{ .value = list };
    }

    /// The `-> name` action of each rule, by rule id (None for a rule without one).
    pub fn actions(self: *GrammarParser) pyoz.Signature(?*pyoz.PyObject, "list[str | None]") {
        const compiled = self._compiled orelse {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        const list = py.c.PyList_New(@intCast(compiled.action_names.len)) orelse return .{ .value = null };
        for (compiled.action_names, 0..) |name, i| {
            const item = if (name.len == 0) blk: {
                py.Py_IncRef(py.Py_None());
                break :blk py.Py_None();
            } else py.PyUnicode_FromStringAndSize(name.ptr, @intCast(name.len)) orelse {
                py.Py_DecRef(list);
                return .{ .value = null };
            };
            _ = py.c.PyList_SetItem(list, @intCast(i), item);
        }
        return .{ .value = list };
    }

    /// The labels of each rule, by rule id: `(label, many)` pairs, `many`
    /// when the label is inside `*`/`+` or used twice (a list in the AST).
    pub fn labels(self: *GrammarParser) pyoz.Signature(?*pyoz.PyObject, "list[list[tuple[str, bool]]]") {
        const compiled = self._compiled orelse {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        const tbl = self.ruleTable() catch {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        const n = compiled.label_start.len - 1;
        const list = py.c.PyList_New(@intCast(n)) orelse return .{ .value = null };
        for (0..n) |r| {
            const uses = compiled.labels[compiled.label_start[r]..compiled.label_start[r + 1]];
            const inner = py.c.PyList_New(@intCast(uses.len)) orelse {
                py.Py_DecRef(list);
                return .{ .value = null };
            };
            _ = py.c.PyList_SetItem(list, @intCast(r), inner);
            for (uses, 0..) |l, i| {
                const pair = py.c.PyTuple_New(2) orelse {
                    py.Py_DecRef(list);
                    return .{ .value = null };
                };
                const name = tbl.fields[l.field - 1];
                py.Py_IncRef(name);
                _ = py.c.PyTuple_SetItem(pair, 0, name);
                const many = if (l.many) py.Py_True() else py.Py_False();
                py.Py_IncRef(many);
                _ = py.c.PyTuple_SetItem(pair, 1, many);
                _ = py.c.PyList_SetItem(inner, @intCast(i), pair);
            }
        }
        return .{ .value = list };
    }

    /// The grammar's literals (`'let'`, `';'`, `'=='`), each once, in the
    /// order they first appear: an editor's keywords and operators.
    pub fn literals(self: *GrammarParser) pyoz.Signature(?*pyoz.PyObject, "list[str]") {
        const compiled = self._compiled orelse {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const grammar = grammar_parser.parseGrammar(arena.allocator(), compiled.text) catch {
            _ = py.c.PyErr_NoMemory();
            return .{ .value = null };
        };
        var found: std.ArrayList([]const u8) = .empty;
        for (grammar.rules) |rule| {
            collectLiterals(arena.allocator(), rule.expr, &found) catch {
                _ = py.c.PyErr_NoMemory();
                return .{ .value = null };
            };
        }
        const list = py.c.PyList_New(@intCast(found.items.len)) orelse return .{ .value = null };
        for (found.items, 0..) |lit, i| {
            const item = py.c.PyUnicode_DecodeUTF8(lit.ptr, @intCast(lit.len), "replace") orelse {
                py.Py_DecRef(list);
                return .{ .value = null };
            };
            _ = py.c.PyList_SetItem(list, @intCast(i), item);
        }
        return .{ .value = list };
    }

    /// The literals the grammar could take at byte `offset` of `input` (its
    /// end by default), given the text before it: what an editor completes
    /// there (`'while'`, `'let'` after a statement; `'-'`, `'('` after
    /// `let x =`). Empty when the text before has an error the parse can't
    /// get past.
    pub fn expected(self: *GrammarParser, args: pyoz.Args(struct { input: *pyoz.PyObject, offset: ?i64 = null, start: ?*pyoz.PyObject = null })) pyoz.Signature(?*pyoz.PyObject, "list[str]") {
        const compiled = self._compiled orelse {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        const input = args.value.input;
        var len: py.Py_ssize_t = 0;
        const ptr: [*]const u8 = blk: {
            if (py.PyUnicode_Check(input)) break :blk py.c.PyUnicode_AsUTF8AndSize(input, &len) orelse return .{ .value = null };
            if (py.PyBytes_Check(input)) {
                var p: [*]u8 = undefined;
                if (py.PyBytes_AsStringAndSize(input, &p, &len) < 0) return .{ .value = null };
                break :blk p;
            }
            py.PyErr_SetString(py.PyExc_TypeError(), "input must be str or bytes");
            return .{ .value = null };
        };
        const total: usize = @intCast(len);
        const offset: usize = if (args.value.offset) |o| @intCast(std.math.clamp(o, 0, @as(i64, @intCast(total)))) else total;
        var start_rule: usize = 0;
        if (args.value.start) |s| {
            if (s != py.Py_None()) {
                if (!py.PyUnicode_Check(s)) {
                    py.PyErr_SetString(py.PyExc_TypeError(), "start must be a rule name (str)");
                    return .{ .value = null };
                }
                var slen: py.Py_ssize_t = 0;
                const sptr = py.c.PyUnicode_AsUTF8AndSize(s, &slen) orelse return .{ .value = null };
                start_rule = compiled.ruleId(sptr[0..@intCast(slen)]) orelse {
                    py.PyErr_SetString(py.PyExc_ValueError(), "unknown start rule");
                    return .{ .value = null };
                };
            }
        }
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const grammar = grammar_parser.parseGrammar(arena.allocator(), compiled.text) catch {
            _ = py.c.PyErr_NoMemory();
            return .{ .value = null };
        };
        const found = diagnose.expectedLiterals(allocator, arena.allocator(), grammar, ptr[0..offset], start_rule) orelse &.{};
        const list = py.c.PyList_New(@intCast(found.len)) orelse return .{ .value = null };
        for (found, 0..) |lit, i| {
            const item = py.c.PyUnicode_DecodeUTF8(lit.ptr, @intCast(lit.len), "replace") orelse {
                py.Py_DecRef(list);
                return .{ .value = null };
            };
            _ = py.c.PyList_SetItem(list, @intCast(i), item);
        }
        return .{ .value = list };
    }

    fn collectLiterals(arena: std.mem.Allocator, expr: *const grammar_parser.Expr, out: *std.ArrayList([]const u8)) !void {
        switch (expr.tag) {
            .literal => {
                const lit = expr.literal_value orelse return;
                if (lit.len == 0) return;
                for (out.items) |seen| {
                    if (std.mem.eql(u8, seen, lit)) return;
                }
                try out.append(arena, lit);
            },
            .sequence, .alternative => for (expr.children orelse &.{}) |child| try collectLiterals(arena, child, out),
            .repetition => if (expr.rep_expr) |sub| try collectLiterals(arena, sub, out),
            .not_predicate, .and_predicate => if (expr.pred_expr) |sub| try collectLiterals(arena, sub, out),
            .reference, .char_class, .any_char => {},
        }
    }

    /// Names of the grammar's labels, in order of first use.
    pub fn fields(self: *GrammarParser) pyoz.Signature(?*pyoz.PyObject, "list[str]") {
        const tbl = self.ruleTable() catch {
            py.PyErr_SetString(py.PyExc_RuntimeError(), "parser not loaded");
            return .{ .value = null };
        };
        const list = py.c.PyList_New(@intCast(tbl.fields.len)) orelse return .{ .value = null };
        for (tbl.fields, 0..) |n, i| {
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
    pub const parse__doc__: [*:0]const u8 = "Parse a str (or UTF-8 bytes), which must match completely. start= names the start rule (default: the first). Returns the root Node, raises ParseError on failure; with recover=True a syntax error doesn't raise: the broken text becomes error nodes (rule '<error>') and node.tree.errors lists the errors.";
    pub const match__doc__: [*:0]const u8 = "Match the start rule at the beginning of the input without requiring it to consume everything. Returns the root Node (see end()), or None if it doesn't match.";
    pub const rules__doc__: [*:0]const u8 = "Names of the grammar's rules, in definition order.";
    pub const literals__doc__: [*:0]const u8 = "The grammar's literals ('let', ';', '=='), each once, in the order they first appear: an editor's keywords and operators.";
    pub const expected__doc__: [*:0]const u8 = "expected(input, offset=None, start=None): the literals the grammar could take at byte `offset` of `input` (its end by default), given the text before it: what an editor completes there. Empty when the text before has an error the parse can't get past.";
    pub const parse_tree__doc__: [*:0]const u8 = "Parse a str (or UTF-8 bytes) and return the Tree: root, nodes, input, rules, fields, and a capsule for native code. Raises ParseError on failure; with recover=True a syntax error doesn't raise: the broken text becomes error nodes (rule '<error>') and tree.errors lists the errors.";
    pub const bind__doc__: [*:0]const u8 = "Supply the classes named by `-> Class` actions: a dict, or an object with them as attributes (a module).";
    pub const bind__params__ = "ast";
    pub const parse_ast__doc__: [*:0]const u8 = "Parse a str (or UTF-8 bytes) and convert the tree to values as the rules' `-> name` actions say. Objects built by `-> Class` get __zspan__ = (start, end) and __znode__ = the node's index, unless spans=False. Raises ParseError on failure; with recover=True a syntax error doesn't raise and the broken text is None in the result.";
    pub const actions__doc__: [*:0]const u8 = "The `-> name` action of each rule, by rule id: a built-in, a class name, or None for a rule without an action.";
    pub const fields__doc__: [*:0]const u8 = "Names of the grammar's labels (label:rule), in order of first use.";
    pub const labels__doc__: [*:0]const u8 = "The labels of each rule, by rule id: a list of (label, many) pairs, many when the label is inside * / + or used twice (a list in the AST).";
    pub const matches__doc__: [*:0]const u8 = "Does the whole input match the grammar? Several times faster than parse(): builds no tree. On False, `error` explains the rejection. start= names the start rule.";
    pub const error__doc__: [*:0]const u8 = "ParseError from the last failed parse (message, line, column, offset), or None.";
};

// ============================================================================
// Module-level functions
// ============================================================================

/// zgram.llvm_capsule(): the "zgram.llvm.v1" capsule (llvm_capsule.zig):
/// zgram's LLVM for native code in other packages.
fn llvm_capsule() pyoz.Signature(?*pyoz.PyObject, "object") {
    return .{ .value = py.c.PyCapsule_New(@ptrCast(@constCast(&llvm_capsule_mod.view)), llvm_capsule_mod.CAPSULE_NAME, null) };
}

fn version() []const u8 {
    return @import("build_options").version;
}

fn compileNative(grammar_text: []const u8) !GrammarParser {
    return .{ ._compiled = try compileCompiled(grammar_text) };
}

/// Completion step of compile_async, on the event loop thread: bind the
/// classes named by `-> Class` actions, as compile(ast=...) does.
fn bindAfterCompile(compiled: GrammarParser, ast: ?*pyoz.PyObject) pyoz.Signature(?GrammarParser, "GrammarParser") {
    var parser = compiled;
    if (ast) |obj| {
        if (obj != py.Py_None() and !parser.bindClasses(obj)) {
            // The parser never reaches Python: release it here
            parser.__del__();
            return .{ .value = null };
        }
    }
    return .{ .value = parser };
}

/// Compile a grammar string into a native parser via LLVM JIT.
/// Runs with the GIL released; repeated grammars come from a cache.
/// `ast` supplies the classes named by `-> Class` actions (see bind()).
fn compile(args: pyoz.Args(struct { grammar: []const u8, ast: ?*pyoz.PyObject = null })) pyoz.Signature(anyerror!?GrammarParser, "GrammarParser") {
    var parser = pyoz.allowThreadsTry(compileNative, .{args.value.grammar}) catch |e| return .{ .value = e };
    if (args.value.ast) |ast| {
        if (ast != py.Py_None() and !parser.bindClasses(ast)) {
            parser.__del__();
            return .{ .value = null };
        }
    }
    return .{ .value = parser };
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

const error_mappings = [_]pyoz.ErrorMapping{
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
    pyoz.mapErrorMsg("UnknownAnnotation", .ValueError, "unknown annotation (expected @silent, @memo, or one of @left, @right, @postfix)"),
    pyoz.mapErrorMsg("InvalidFoldRule", .ValueError, "a @left, @right or @postfix rule must have the form `head (group)*` and can't be @silent"),
    pyoz.mapErrorMsg("ExpectedRuleAfterLabel", .ValueError, "expected a rule name after 'label:' (only rule references can be labelled)"),
    pyoz.mapErrorMsg("TooManyFields", .ValueError, "too many distinct labels (at most 255)"),
    pyoz.mapError("ExpectedRuleName", .ValueError),
    pyoz.mapError("ExpectedEquals", .ValueError),
    pyoz.mapError("ExpectedExpression", .ValueError),
    pyoz.mapError("ExpectedCloseParen", .ValueError),
    pyoz.mapError("UnterminatedString", .ValueError),
    pyoz.mapError("UnterminatedCharClass", .ValueError),
    pyoz.mapError("CompilationFailed", .RuntimeError),
};

pub const Module = pyoz.module(.{
    .name = "zgram",
    .doc = "zgram - PEG parser generator. Compiles grammars to native code via LLVM JIT.",
    .consts = &.{
        pyoz.constant("TREE_ABI", @as(i64, abi.TREE_ABI)),
        pyoz.constant("LLVM_ABI", @as(i64, llvm_capsule_mod.LLVM_ABI)),
    },
    .funcs = &.{
        pyoz.func("compile", compile, "Compile a grammar string into a native parser. ast= supplies the classes named by `-> Class` actions."),
        pyoz.func("compile_async", pyoz.asyncThen(compileNative, bindAfterCompile), "Compile a grammar on a worker thread; returns an awaitable GrammarParser. ast= supplies the classes named by `-> Class` actions.").withParams("grammar, ast"),
        pyoz.func("clear_cache", clear_cache, "Drop cached compiled grammars"),
        pyoz.func("dump_ir", dump_ir, "Dump LLVM IR text for a grammar").withParams("grammar"),
        pyoz.func("version", version, "Return zgram version string"),
        pyoz.func("llvm_capsule", llvm_capsule, "The 'zgram.llvm.v1' capsule: zgram's LLVM for native code in other packages (LLVM's C API to build modules in memory; zgram's JIT to compile them, or an object file). See src/llvm_capsule.zig for its layout."),
    },
    .classes = &.{
        pyoz.class("Node", Node),
        pyoz.class("NodeIter", NodeIter),
        pyoz.class("Tree", Tree),
        pyoz.class("GrammarParser", GrammarParser),
        pyoz.class("ParseErrorInfo", ParseError),
        pyoz.class("Diagnostic", Diagnostic),
    },
    .exceptions = &.{
        pyoz.exception("ParseError", .{ .doc = "Raised when parsing fails", .base = .ValueError }),
    },
    .error_mappings = &error_mappings,
});

// ============================================================================
// Windows: run C++ static constructors
// ============================================================================

/// On Windows, Zig's own DLL entry point (std.start's _DllMainCRTStartup)
/// doesn't initialize the C runtime or run C++ static constructors, so the
/// bundled LLVM libraries would start with every command-line option zeroed
/// (LLVM then loops forever uniquing value names). Declaring the entry point
/// here makes Zig skip its own; ours hands over to the MinGW CRT's
/// DllMainCRTStartup, which initializes the CRT, runs the constructors and
/// then calls DllMain, as in any MinGW-built DLL.
pub const _DllMainCRTStartup = if (builtin.os.tag == .windows) windows_entry.start else {};

const windows_entry = struct {
    const win = std.os.windows;
    extern fn DllMainCRTStartup(hinst: win.HINSTANCE, reason: win.DWORD, reserved: win.LPVOID) callconv(.winapi) win.BOOL;

    fn start(hinst: win.HINSTANCE, reason: win.DWORD, reserved: win.LPVOID) callconv(.winapi) win.BOOL {
        return DllMainCRTStartup(hinst, reason, reserved);
    }

    comptime {
        if (builtin.os.tag == .windows) @export(&start, .{ .name = "_DllMainCRTStartup" });
    }
};

// Required: forces analysis of all pub decls so PyInit_ is exported.
comptime {
    for (@typeInfo(@This()).@"struct".decls) |decl| {
        _ = @field(@This(), decl.name);
    }
}
