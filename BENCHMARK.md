# zgram Benchmark

zgram against the fastest PEG and parser-combinator libraries in C, C++ and Rust, on two grammars: **JSON** and an **arithmetic expression language**.

Every benchmark parses the same file in a tight loop, auto-calibrating the iteration count until a batch takes at least 2 seconds, and reports wall-clock time per parse. All libraries use an equivalent grammar and reject invalid input. The whole suite runs pinned to one CPU core.

## What is compared

zgram builds a parse tree: a node per rule match with the rule, byte span, children and subtree size. So the main comparison is **building a tree**, with every library producing the tree it's designed to produce:

- **The same tree as zgram.** Spirit X3 (through a small custom directive that builds a flat node array, rolled back on backtracking: zgram's own design), rust-peg (tree built by rule actions), PEGTL (its built-in `parse_tree`, selecting the same rules) and, for expressions, pest all produce exactly zgram's nodes (pest adds its `EOI` token).
- **Their idiomatic typed AST** (JSON): Spirit X3 parsing into `x3::variant` / `std::vector` / `std::map` through attributes, and lexy's official JSON example parsing into its AST types. These build values rather than generic nodes, but they're what users of these libraries write.
- **Other built-in trees.** pest's token pair queue (JSON), and lexy's `parse_as_tree`, which also records every token.

The second comparison is **validation only**: accept or reject, build nothing. zgram's is `GrammarParser.matches()`, a separate parser zgram compiles for validation: no nodes, no error tracking, every rule except one per recursion cycle inlined (for both grammars here; on large grammars, only as long as no function grows past a size limit). (When it rejects input, zgram runs the tree parser once more to explain why; that isn't timed here.)

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
| **zgram** | flat node array | **0.04us** | **0.84us** | **12.45us** | **9.20us** |
| Spirit X3 | flat node array (custom directive, zgram's design) | 0.07us (1.8x) | 1.46us (1.7x) | 21.50us (1.7x) | 53.67us (5.8x) |
| rust-peg | tree via actions | 0.13us (3.2x) | 3.79us (4.5x) | 70.41us (5.7x) | 115us (12.6x) |
| Spirit X3 | typed AST (`variant` / `vector` / `map`) | 0.16us (4.0x) | 4.49us (5.3x) | 69.11us (5.6x) | 113us (12.3x) |
| lexy | typed AST (official JSON example) | 0.19us (4.8x) | 5.26us (6.3x) | 75.90us (6.1x) | 104us (11.3x) |
| pest | token pair queue | 0.78us (19.5x) | 19.37us (23.1x) | 182us (14.7x) | 1,112us (120.9x) |
| PEGTL | built-in `parse_tree` | 0.78us (19.5x) | 17.86us (21.3x) | 258us (20.7x) | 607us (66.0x) |
| lexy | built-in `parse_as_tree` (tokens included) | 0.29us (7.2x) | 61.49us (73.2x) | 284us (22.8x) | 260us (28.3x) |

### Validation only

| Library | Output | Small (43 B) | Medium (1.2 KB) | Large (15 KB) | Strings (75 KB) |
|---|---|---|---|---|---|
| **zgram** | `matches()` | **0.02us** | **0.49us** | **7.13us** | **5.81us** |
| lexy | `lexy::match` | 0.02us (1.0x) | 0.66us (1.3x) | 9.96us (1.4x) | 31.40us (5.4x) |
| Spirit X3 | parse without attribute | 0.03us (1.5x) | 0.78us (1.6x) | 9.29us (1.3x) | 41.84us (7.2x) |
| rust-peg | rules return `()` | 0.08us (4.0x) | 1.92us (3.9x) | 23.10us (3.2x) | 98.86us (17.0x) |
| PEGTL | `parse` without actions | 0.19us (9.5x) | 6.13us (12.5x) | 47.58us (6.7x) | 444us (76.4x) |
| PackCC | generated C parser | 2.58us (129.0x) | 75.48us (154.0x) | 690us (96.8x) | 15,957us (2746.5x) |
| cpp-peglib | runtime interpreter | 7.56us (378.0x) | 252us (513.4x) | 2,117us (296.9x) | 16,497us (2839.4x) |

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
| **zgram** | flat node array | **0.06us** | **2.65us** | **32.05us** | **6.60us** |
| Spirit X3 | flat node array (custom directive, zgram's design) | 0.11us (1.8x) | 3.95us (1.5x) | 48.36us (1.5x) | 6.89us (1.04x) |
| pest | token pair queue | 0.53us (8.8x) | 18.24us (6.9x) | 229us (7.1x) | 35.73us (5.4x) |
| rust-peg | tree via actions | 0.42us (7.0x) | 23.60us (8.9x) | 309us (9.6x) | 31.39us (4.8x) |
| lexy | built-in `parse_as_tree` (own shape, tokens included) | 0.41us (6.8x) | 97.24us (36.7x) | 447us (14.0x) | 44.37us (6.7x) |
| PEGTL | built-in `parse_tree` | 1.29us (21.5x) | 55.51us (20.9x) | 1,009us (31.5x) | 106us (16.1x) |

### Validation only

| Library | Output | Small (37 B) | Medium (1.2 KB) | Large (15 KB) | Deep (1.2 KB) |
|---|---|---|---|---|---|
| **zgram** | `matches()` | **0.04us** | **1.20us** | **14.59us** | **3.56us** |
| Spirit X3 | parse without attribute | 0.04us (1.0x) | 1.66us (1.4x) | 19.62us (1.3x) | 4.05us (1.1x) |
| lexy | `expression_production` + `lexy::match` | 0.09us (2.3x) | 2.97us (2.5x) | 40.50us (2.8x) | 3.54us (1.0x) |
| rust-peg | rules return `()` | 0.10us (2.5x) | 3.77us (3.1x) | 52.36us (3.6x) | 8.48us (2.4x) |
| PEGTL | `parse` without actions | 0.15us (3.8x) | 5.31us (4.4x) | 62.59us (4.3x) | 7.16us (2.0x) |

## Analysis

**zgram is the fastest or tied in all 16 comparisons** (two grammars, four inputs, tree and validation). It builds every tree fastest; validating, it ties Spirit X3 and lexy on the smallest inputs, and lexy on the deeply nested expressions (3.54us against 3.56us).

**Building trees**, the closest competitor on both grammars is Spirit X3 building the same flat node array as zgram, through a directive written for this benchmark: zgram is 1.5-1.8x faster on the regular inputs, 4% faster on the deeply nested expressions and 5.8x faster on long strings. Built the way these libraries are normally used, trees cost much more: rust-peg with tree actions takes 3-6x longer than zgram on JSON and 5-10x on expressions; the X3 and lexy typed ASTs 4-6x on JSON; pest, PEGTL's and lexy's built-in trees 7-120x.

**Validating**, zgram's `matches()` is 1.3-2.8x faster than Spirit X3 and lexy on the regular inputs, and 5-7x faster on long strings.

**Why zgram is fast:**

- The grammar is JIT-compiled with LLVM for the exact CPU it runs on.
- Trees go into one flat, contiguous array (16 bytes per node), with no allocation per node. That's the main gap to rust-peg's tree actions and the typed ASTs, which allocate vectors, strings and maps per node.
- Character classes are tested with a range compare or a few equality tests where possible, and loops over them test the first 8 bytes one at a time before switching to 16/32-byte SIMD. Most runs in real input are short (a space, a few digits); long ones, like the strings input, are where SIMD pays off.
- An alternative that starts with a rule's call is tried only when the next byte can start it (`number` isn't called at a letter), and a rule called from one place is compiled into it: one call less per match.
- The validator drops all tree and error bookkeeping and inlines everything except one rule per recursion cycle, as compile-time parser generators do (up to a size limit per function, which these grammars stay well under).

**Compile time is not included.** zgram compiles a grammar at runtime: about 90 ms for these grammars, plus about 0.1 s for the validator on first use of `matches()`; the compiled code is kept on disk, so later processes load it in a few milliseconds. The other libraries compile ahead of time.

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
