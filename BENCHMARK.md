# zgram Benchmark — JSON Parse Performance

zgram against the fastest PEG and parser-combinator libraries in C, C++ and Rust, parsing four JSON inputs.

Every benchmark parses the same file in a tight loop, auto-calibrating the iteration count until a batch takes at least 2 seconds, and reports wall-clock time per parse. All libraries use an equivalent JSON grammar and reject invalid JSON. The whole suite runs pinned to one CPU core.

## What is compared

zgram always builds a parse tree: a node per rule match with the rule, byte span, children and subtree size. So the main comparison is **building a tree**, with every library producing the tree it's designed to produce:

- **The same tree as zgram.** Spirit X3 (through a small custom directive that builds a flat node array, rolled back on backtracking: zgram's own design), rust-peg (tree built by rule actions) and PEGTL (its built-in `parse_tree`, with a selector for the same rules) all produce exactly zgram's nodes: 13 / 286 / 3,706 / 2,802 for the four inputs.
- **Their idiomatic typed AST.** Spirit X3 parsing into `x3::variant` / `std::vector` / `std::map` through attributes, and lexy's official JSON example parsing into its AST types. These build values rather than generic nodes, but they're what users of these libraries write.
- **Other built-in trees.** pest's token pair queue, and lexy's `parse_as_tree`, which also records every token (about 3x more nodes).

A second table compares **validation only** (build nothing, just accept or reject), where zgram runs in `--validate` mode: the grammar wrapped in a `root = value` rule with every other rule `@silent`.

## Inputs

- **Small** (43 bytes): `{"name": "John", "age": 30, "active": true}`
- **Medium** (1,201 bytes): 20 user objects with id, name and email.
- **Large** (15,241 bytes): 100 user objects with id, name, a 10-number scores array and a flag.
- **Strings** (74,903 bytes): 200 log-like records with a 40-word message and a long path.

## Results

Time per parse; in parentheses, how many times longer than zgram.

### Building a parse tree

| Library | Output | Small (43 B) | Medium (1.2 KB) | Large (15 KB) | Strings (75 KB) |
|---|---|---|---|---|---|
| **zgram** | flat node array | **0.05us** | **1.05us** | **18.65us** | **12.53us** |
| Spirit X3 | flat node array (custom directive, zgram's design) | 0.07us (1.4x) | 1.41us (1.3x) | 21.17us (1.1x) | 64.94us (5.2x) |
| rust-peg | tree via actions (`Vec` of children per node) | 0.14us (2.8x) | 3.74us (3.6x) | 69.09us (3.7x) | 118us (9.4x) |
| Spirit X3 | typed AST (`variant` / `vector` / `map`) | 0.15us (3.0x) | 4.40us (4.2x) | 84.19us (4.5x) | 110us (8.8x) |
| lexy | typed AST (official JSON example) | 0.19us (3.8x) | 5.37us (5.1x) | 75.34us (4.0x) | 105us (8.3x) |
| pest | token pair queue | 0.77us (15.4x) | 19.79us (18.8x) | 223us (12.0x) | 1,045us (83.4x) |
| PEGTL | built-in `parse_tree` | 0.78us (15.6x) | 17.47us (16.6x) | 243us (13.0x) | 606us (48.4x) |
| lexy | built-in `parse_as_tree` (tokens included) | 0.28us (5.6x) | 19.85us (18.9x) | 358us (19.2x) | 326us (26.0x) |

### Validation only

| Library | Output | Small (43 B) | Medium (1.2 KB) | Large (15 KB) | Strings (75 KB) |
|---|---|---|---|---|---|
| **zgram** | `--validate` mode | **0.03us** | **0.80us** | **15.40us** | **10.43us** |
| lexy | `lexy::match` | 0.03us (1.0x) | 0.65us (0.8x) | 10.39us (0.7x) | 33.22us (3.2x) |
| Spirit X3 | parse without attribute | 0.03us (1.0x) | 0.74us (0.9x) | 9.04us (0.6x) | 42.24us (4.0x) |
| rust-peg | rules return `()` | 0.08us (2.7x) | 1.92us (2.4x) | 22.85us (1.5x) | 95.55us (9.2x) |
| PEGTL | `parse` without actions | 0.19us (6.3x) | 5.61us (7.0x) | 47.20us (3.1x) | 440us (42.2x) |
| PackCC | generated C parser | 2.60us (86.7x) | 75.18us (94.0x) | 694us (45.1x) | 15,264us (1463.4x) |
| cpp-peglib | runtime interpreter | 7.43us (247.7x) | 244us (305.4x) | 2,069us (134.4x) | 15,764us (1511.4x) |

## Analysis

**Building a tree, zgram is the fastest library on every input.** The closest competitor is Spirit X3 building the same flat node array as zgram, through a custom directive written for this benchmark: zgram is 1.1-1.4x faster on the regular inputs and 5x faster on long strings. Built the way these libraries are normally used, the trees cost far more: rust-peg with tree actions and the X3 and lexy typed ASTs take 3-5x longer than zgram on the regular inputs, and pest, PEGTL's and lexy's built-in trees 5-20x longer.

**Validating only, compile-time C++ is faster on medium and large inputs.** lexy and Spirit X3 inline the whole grammar into a few functions and do no bookkeeping, and they validate the medium and large inputs 1.1-1.7x faster than zgram's validate mode. zgram matches them on small input, is 3-4x faster on long strings, and is faster than rust-peg, PEGTL, PackCC and cpp-peglib throughout. zgram's validate mode is a benchmark mode, though, not what zgram is for: it keeps tracking child counts and the furthest failure position, and building its full tree costs little more.

**Why zgram's trees are cheap:**

- The grammar is JIT-compiled with LLVM for the exact CPU it runs on, with each rule a native function and node allocation inlined.
- Nodes go into one flat, contiguous array (16 bytes per node), with no allocation per node. That's the main gap to rust-peg's tree actions and the typed ASTs, which allocate vectors, strings and maps per node.
- Character-class loops test the first 8 bytes one at a time and switch to 16/32-byte SIMD only for longer runs. Most runs in JSON are short (a space, a few digits), where a vector step costs more than it saves; long runs, like the strings input, are where SIMD pays off.

**Compiler matters for the C++ libraries.** They are built with GCC, which is 1.6-3x faster than Clang for lexy and Spirit X3 on these inputs. zgram generates its code with LLVM.

### Key Takeaway

> zgram compiles a grammar at runtime into a parser that builds a full parse tree faster than the fastest compile-time parser generators in C++ and Rust build theirs.

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
