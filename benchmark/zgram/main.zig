const std = @import("std");
const core = @import("zgram_core");
const gp = core.grammar_parser;
const jc = core.jit_codegen;
const jr = core.jit_compiler;
const abi = core.parse_abi;

const JSON_GRAMMAR =
    \\value     = ws (object | array | string | number | true_lit | false_lit | null_lit) ws
    \\object    = '{' ws (pair (',' ws pair)*)? ws '}'
    \\pair      = ws string ws ':' value
    \\array     = '[' ws (value (',' ws value)*)? ws ']'
    \\string    = '"' chars '"'
    \\@silent chars  = char*
    \\@silent char   = escape | plain
    \\@silent escape = '\\' ["\\bfnrt/]
    \\@silent plain  = [^"\\]
    \\number    = int frac? exp?
    \\@silent int  = '-'? ('0' | [1-9] [0-9]*)
    \\@silent frac = '.' [0-9]+
    \\@silent exp  = [eE] [+\-]? [0-9]+
    \\@silent true_lit  = 'true'
    \\@silent false_lit = 'false'
    \\@silent null_lit  = 'null'
    \\@silent ws        = [ \t\n\r]*
    \\
;

/// The grammar under a new `root` rule, with every original rule @silent:
/// the parse produces only the root node, like a validate-only parser.
fn validateOnly(allocator: std.mem.Allocator, text: []const u8) ![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    try out.appendSlice(allocator, "root = value\n");
    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |line| {
        if (line.len > 0 and !std.mem.startsWith(u8, line, "@silent")) try out.appendSlice(allocator, "@silent ");
        try out.appendSlice(allocator, line);
        try out.append(allocator, '\n');
    }
    return out.items;
}

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    // Parse command line
    const args = try init.minimal.args.toSlice(init.arena.allocator());

    if (args.len < 2) {
        std.debug.print("Usage: zgram_bench <json_file> [--validate]\n", .{});
        std.process.exit(1);
    }

    // Read input file
    const input = try std.Io.Dir.cwd().readFileAlloc(io, args[1], allocator, .limited(1024 * 1024));
    defer allocator.free(input);

    // Compile grammar
    std.debug.print("Compiling JSON grammar...\n", .{});
    // --validate: build no tree beyond a root node, which is the work that
    // validate-only parsers (rust-peg, PEGTL, cpp-peglib here) do
    const validate = args.len > 2 and std.mem.eql(u8, args[2], "--validate");
    const grammar_text = if (validate) try validateOnly(allocator, JSON_GRAMMAR) else JSON_GRAMMAR;
    const grammar = try gp.parseGrammar(allocator, grammar_text);
    defer grammar.deinit(allocator);

    const codegen_result = try jc.generateModule(allocator, grammar);
    const jit_result = try jr.jitCompile(codegen_result.module, codegen_result.context);
    const parse_fn = jit_result.parse_fn;
    defer jr.releaseGrammar(jit_result.resource);

    // Verify parse works
    var output: abi.ParseOutput = .{};
    _ = parse_fn(input.ptr, input.len, &output, 0, 0);
    if (output.status != 1) {
        std.debug.print("Parse failed at offset {d} (line {d}, col {d})\n", .{ output.error_offset, output.error_line, output.error_col });
        std.process.exit(1);
    }
    std.debug.print("Parse OK ({d} nodes). Benchmarking...\n", .{output.node_count});

    // Auto-calibrate: find iteration count that takes >= 2 seconds
    var iters: u64 = 1000;
    while (true) {
        const timer_start = std.Io.Timestamp.now(io, .awake);
        for (0..iters) |_| {
            output.node_count = 0;
            _ = parse_fn(input.ptr, input.len, &output, 0, 0);
        }
        const elapsed_ns = timer_start.untilNow(io, .awake).toNanoseconds();
        const elapsed_s = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0;

        if (elapsed_s >= 2.0) {
            const per_parse_us = (elapsed_s * 1_000_000.0) / @as(f64, @floatFromInt(iters));
            const ops_per_sec = @as(f64, @floatFromInt(iters)) / elapsed_s;

            std.debug.print("\n-- zgram JIT{s} ({d} bytes) --\n", .{ if (validate) " validate-only" else "", input.len });
            std.debug.print("  Iterations:  {d}\n", .{iters});
            std.debug.print("  Total:       {d:.4}s\n", .{elapsed_s});
            std.debug.print("  Per-parse:   {d:.2}us\n", .{per_parse_us});
            std.debug.print("  Ops/sec:     {d:.0}\n", .{ops_per_sec});
            break;
        }

        // Scale up
        if (elapsed_s < 0.1) {
            iters *= 20;
        } else {
            iters = @intFromFloat(@as(f64, @floatFromInt(iters)) * 2.5 / elapsed_s);
        }
    }

    // Free node buffer (allocated by c_allocator inside JIT helpers)
    if (output.nodes_ptr) |nodes| {
        std.heap.c_allocator.free(nodes[0..output.node_capacity]);
    }
}
