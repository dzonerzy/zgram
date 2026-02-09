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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: zgram_bench <json_file>\n", .{});
        std.process.exit(1);
    }

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], 1024 * 1024);
    defer allocator.free(input);

    // Compile grammar
    std.debug.print("Compiling JSON grammar...\n", .{});
    const grammar = try gp.parseGrammar(allocator, JSON_GRAMMAR);
    defer grammar.deinit(allocator);

    const codegen_result = try jc.generateModule(allocator, grammar);
    const jit_result = try jr.jitCompile(codegen_result.module, codegen_result.context);
    const parse_fn = jit_result.parse_fn;
    defer jr.releaseGrammar(jit_result.resource);

    // Verify parse works
    var output: abi.ParseOutput = .{};
    _ = parse_fn(input.ptr, input.len, &output);
    if (output.status != 1) {
        std.debug.print("Parse failed: {s}\n", .{output.error_message[0..output.error_message_len]});
        std.process.exit(1);
    }
    std.debug.print("Parse OK ({d} nodes). Benchmarking...\n", .{output.node_count});

    // Auto-calibrate: find iteration count that takes >= 2 seconds
    var iters: u64 = 1000;
    while (true) {
        var timer = try std.time.Timer.start();
        for (0..iters) |_| {
            output.node_count = 0;
            _ = parse_fn(input.ptr, input.len, &output);
        }
        const elapsed_ns = timer.read();
        const elapsed_s = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000_000.0;

        if (elapsed_s >= 2.0) {
            const per_parse_us = (elapsed_s * 1_000_000.0) / @as(f64, @floatFromInt(iters));
            const ops_per_sec = @as(f64, @floatFromInt(iters)) / elapsed_s;

            std.debug.print("\n-- zgram JIT ({d} bytes) --\n", .{input.len});
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
