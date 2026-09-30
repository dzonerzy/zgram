# zgram Benchmark

zgram against the fastest PEG and parser-combinator libraries in C, C++ and Rust, on two grammars: **JSON** and an **arithmetic expression language**.

Every benchmark parses the same file in a tight loop, auto-calibrating the iteration count until a batch takes at least 2 seconds, and reports wall-clock time per parse. All libraries use an equivalent grammar and reject invalid input. The whole suite runs pinned to one CPU core.

## What is compared

zgram builds a parse tree: a node per rule match with the rule, byte span, children and subtree size. So the main comparison is **building a tree**, with every library producing the tree it's designed to produce:

- **The same tree as zgram.** Spirit X3 (through a small custom directive that builds a flat node array, rolled back on backtracking: zgram's own design), rust-peg (tree built by rule actions), PEGTL (its built-in `parse_tree`, selecting the same rules) and, for expressions, pest all produce exactly zgram's nodes (pest adds its `EOI` token).
- **Their idiomatic typed AST** (JSON): Spirit X3 parsing into `x3::variant` / `std::vector` / `std::map` through attributes, and lexy's official JSON example parsing into its AST types. These build values rather than generic nodes, but they're what users of these libraries write.
- **Other built-in trees.** pest's token pair queue (JSON), and lexy's `parse_as_tree`, which also records every token.

The second comparison is **validation only**: accept or reject, build nothing. zgram's is `GrammarParser.matches()`, a separate parser zgram compiles for validation: no nodes, no error tracking, every rule except one per recursion cycle inlined. (When it rejects input, zgram runs the tree parser once more to explain why; that isn't timed here.)

## JSON

- **Small** (43 bytes): `{"name": "John", "age": 30, "active": true}`
- **Medium** (1,201 bytes): 20 user objects with id, name and email.
- **Large** (15,241 bytes): 100 user objects with id, name, a 10-number scores array and a flag.
- **Strings** (74,903 bytes): 200 log-like records with a 40-word message and a long path.

Tree nodes (zgram and the libraries building the same tree): 13 / 286 / 3,706 / 2,802.

Time per parse; in parentheses, how many times longer than zgram.

### Building a parse tree

| Library | Output | Small (43 B) | Medium (1.2 KB) | Large (15 KB) | Strings (75 KB) |
|---|---|---|---|---|---|
| **zgram** | flat node array | **0.04us** | **0.90us** | **15.39us** | **10.01us** |
| Spirit X3 | flat node array (custom directive, zgram's design) | 0.06us (1.5x) | 1.41us (1.6x) | 20.36us (1.3x) | 51.79us (5.2x) |
| rust-peg | tree via actions | 0.13us (3.2x) | 3.71us (4.1x) | 66.91us (4.3x) | 113us (11.3x) |
| Spirit X3 | typed AST (`variant` / `vector` / `map`) | 0.16us (4.0x) | 4.32us (4.8x) | 60.76us (3.9x) | 108us (10.8x) |
| lexy | typed AST (official JSON example) | 0.19us (4.8x) | 5.78us (6.4x) | 83.70us (5.4x) | 105us (10.5x) |
| pest | token pair queue | 0.76us (19.0x) | 18.41us (20.5x) | 171us (11.1x) | 1,043us (104.2x) |
| PEGTL | built-in `parse_tree` | 0.77us (19.2x) | 17.50us (19.4x) | 242us (15.7x) | 588us (58.8x) |
| lexy | built-in `parse_as_tree` (tokens included) | 0.35us (8.8x) | 18.17us (20.2x) | 252us (16.4x) | 291us (29.1x) |

### Validation only

| Library | Output | Small (43 B) | Medium (1.2 KB) | Large (15 KB) | Strings (75 KB) |
|---|---|---|---|---|---|
| **zgram** | `matches()` | **0.02us** | **0.47us** | **6.86us** | **5.67us** |
| lexy | `lexy::match` | 0.02us (1.0x) | 0.68us (1.4x) | 9.56us (1.4x) | 31.72us (5.6x) |
| Spirit X3 | parse without attribute | 0.03us (1.5x) | 0.74us (1.6x) | 8.85us (1.3x) | 40.45us (7.1x) |
| rust-peg | rules return `()` | 0.08us (4.0x) | 1.88us (4.0x) | 22.29us (3.2x) | 96.17us (17.0x) |
| PEGTL | `parse` without actions | 0.18us (9.0x) | 5.55us (11.8x) | 45.39us (6.6x) | 431us (76.1x) |
| PackCC | generated C parser | 2.45us (122.5x) | 70.65us (150.3x) | 592us (86.3x) | 14,833us (2616.1x) |
| cpp-peglib | runtime interpreter | 7.30us (365.0x) | 242us (514.2x) | 2,007us (292.6x) | 15,341us (2705.7x) |

## Expressions

Arithmetic with precedence levels (`+ -`, `* / %`, right-associative `^`), unary minus, parentheses and function calls:

```
expr    = ws sum ws
sum     = product (ws addop ws product)*
product = power (ws mulop ws power)*
power   = unary (ws '^' ws power)?
unary   = '-' ws unary | primary
@silent primary = number | call | ident | '(' ws sum ws ')'
call    = ident ws '(' ws args? ws ')'
@silent args = sum (ws ',' ws sum)*
number  = [0-9]+ ('.' [0-9]+)? ([eE] [+\-]? [0-9]+)?
ident   = [a-zA-Z_] [a-zA-Z0-9_]*
```

`call` is tried before `ident`, so every plain identifier is parsed, rejected as a call, and parsed again: a typical source of PEG backtracking. lexy is written the idiomatic lexy way (an `expression_production` with operator tables, deciding call vs. name by lookahead), so its tree has a different shape.

- **Small** (37 bytes), **Medium** (1,208 bytes), **Large** (15,019 bytes): random expressions mixing all constructs.
- **Deep** (1,221 bytes): 150 levels of nested calls and parentheses.

Tree nodes: 27 / 907 / 10,849 / 1,281.

### Building a parse tree

| Library | Output | Small (37 B) | Medium (1.2 KB) | Large (15 KB) | Deep (1.2 KB) |
|---|---|---|---|---|---|
| **zgram** | flat node array | **0.06us** | **2.47us** | **29.90us** | **7.32us** |
| Spirit X3 | flat node array (custom directive, zgram's design) | 0.10us (1.7x) | 3.78us (1.5x) | 46.82us (1.6x) | 6.88us (0.9x) |
| pest | token pair queue | 0.49us (8.2x) | 17.29us (7.0x) | 226us (7.6x) | 34.53us (4.7x) |
| rust-peg | tree via actions | 0.41us (6.8x) | 22.19us (9.0x) | 298us (10.0x) | 31.10us (4.2x) |
| lexy | built-in `parse_as_tree` (own shape, tokens included) | 0.41us (6.8x) | 35.55us (14.4x) | 429us (14.4x) | 43.31us (5.9x) |
| PEGTL | built-in `parse_tree` | 1.24us (20.7x) | 52.98us (21.4x) | 896us (30.0x) | 106us (14.5x) |

### Validation only

| Library | Output | Small (37 B) | Medium (1.2 KB) | Large (15 KB) | Deep (1.2 KB) |
|---|---|---|---|---|---|
| **zgram** | `matches()` | **0.03us** | **1.13us** | **13.85us** | **3.36us** |
| Spirit X3 | parse without attribute | 0.03us (1.0x) | 1.64us (1.5x) | 19.01us (1.4x) | 3.97us (1.2x) |
| lexy | `expression_production` + `lexy::match` | 0.08us (2.7x) | 2.93us (2.6x) | 36.24us (2.6x) | 4.02us (1.2x) |
| rust-peg | rules return `()` | 0.10us (3.3x) | 3.62us (3.2x) | 51.19us (3.7x) | 8.45us (2.5x) |
| PEGTL | `parse` without actions | 0.15us (5.0x) | 5.31us (4.7x) | 65.09us (4.7x) | 7.33us (2.2x) |

## Analysis

**zgram is the fastest in 15 of the 16 comparisons** (two grammars, four inputs, tree and validation); validating the smallest inputs, it ties Spirit X3 and lexy. The exception: building the deeply nested expression tree, Spirit X3 with a flat node array like zgram's is 6% faster.

**Building trees**, the closest competitor on both grammars is Spirit X3 building the same flat node array as zgram, through a directive written for this benchmark: zgram is 1.3-1.7x faster on the regular inputs and 5x faster on long strings. Built the way these libraries are normally used, trees cost much more: rust-peg with tree actions takes 3-4x longer than zgram on JSON and 4-10x on expressions; the X3 and lexy typed ASTs 4-6x on JSON; pest, PEGTL's and lexy's built-in trees 5-30x.

**Validating**, zgram's `matches()` is 1.2-2.7x faster than Spirit X3 and lexy on the regular inputs, and 6-7x faster on long strings.

**Why zgram is fast:**

- The grammar is JIT-compiled with LLVM for the exact CPU it runs on.
- Trees go into one flat, contiguous array (16 bytes per node), with no allocation per node. That's the main gap to rust-peg's tree actions and the typed ASTs, which allocate vectors, strings and maps per node.
- Character classes are tested with a range compare or a few equality tests where possible, and loops over them test the first 8 bytes one at a time before switching to 16/32-byte SIMD. Most runs in real input are short (a space, a few digits); long ones, like the strings input, are where SIMD pays off.
- The validator drops all tree and error bookkeeping and inlines everything except one rule per recursion cycle, as compile-time parser generators do.

**Compile time is not included.** zgram compiles a grammar at runtime: about 90 ms for these grammars (cached afterwards), plus about 0.1 s for the validator on first use of `matches()`. The other libraries compile ahead of time.

**Compiler matters for the C++ libraries.** They are built with GCC, which is 1.6-3x faster than Clang for lexy and Spirit X3 here. zgram generates its code with LLVM.

### Key Takeaway

> zgram compiles a grammar at runtime into a parser that is faster than the fastest compile-time parser generators in C++ and Rust, whether they build a tree or only validate.

## Reproducing

```bash
# Prerequisites: Zig 0.16+, CMake 3.14+, a C++20 compiler, Boost 1.70+ (Spirit X3), Rust/Cargo, Python 3
bash benchmark/run_all.sh
```

## Environment

- CPU: AMD Ryzen 9 9950X3D 16-Core Processor (WSL2, Linux 6.18), run pinned to one core
- Zig 0.16.0, GCC 11.4 (C/C++), Rust 1.90.0, CMake 3.30, Boost 1.74
- Library versions: lexy c1358c4, Spirit X3 (Boost 1.74), PackCC 3.1.0, PEGTL 3.2.8, pest 2.7, rust-peg 0.8, cpp-peglib 1.9.1
- All native builds use maximum optimization (`-O3` / `ReleaseFast` / `--release` with LTO)
