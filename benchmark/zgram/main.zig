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

/// Arithmetic expressions: precedence levels, right-associative ^, unary
/// minus, function calls. `call` is tried before `ident`, so every plain
/// identifier is parsed, rejected as a call, and parsed again (backtracking).
const EXPR_GRAMMAR =
    \\expr    = ws sum ws
    \\sum     = product (ws addop ws product)*
    \\product = power (ws mulop ws power)*
    \\power   = unary (ws '^' ws power)?
    \\unary   = '-' ws unary | primary
    \\@silent primary = number | call | ident | '(' ws sum ws ')'
    \\call    = ident ws '(' ws args? ws ')'
    \\@silent args = sum (ws ',' ws sum)*
    \\number  = [0-9]+ ('.' [0-9]+)? ([eE] [+\-]? [0-9]+)?
    \\ident   = [a-zA-Z_] [a-zA-Z0-9_]*
    \\@silent addop = [+\-]
    \\@silent mulop = [*/%]
    \\@silent ws    = [ \t\n\r]*
    \\
;

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    // Parse command line
    const args = try init.minimal.args.toSlice(init.arena.allocator());

    if (args.len < 2) {
        std.debug.print("Usage: zgram_bench <input_file> [--validate] [--expr]\n", .{});
        std.process.exit(1);
    }

    // Read input file
    const input = try std.Io.Dir.cwd().readFileAlloc(io, args[1], allocator, .limited(1024 * 1024));
    defer allocator.free(input);

    // Compile grammar
    // --validate: zgram's accept/reject-only parser (GrammarParser.matches),
    // which builds no tree, like validate-only parsers
    var validate = false;
    var expr = false;
    for (args[2..]) |a| {
        if (std.mem.eql(u8, a, "--validate")) validate = true;
        if (std.mem.eql(u8, a, "--expr")) expr = true;
    }
    std.debug.print("Compiling {s} grammar...\n", .{if (expr) "expression" else "JSON"});
    const grammar = try gp.parseGrammar(allocator, if (expr) EXPR_GRAMMAR else JSON_GRAMMAR);
    defer grammar.deinit(allocator);

    const codegen_result = try jc.generateModule(allocator, grammar, if (validate) .validate else .tree);
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
