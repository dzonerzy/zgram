# zgram Benchmark — JSON Parse Performance

Comparison of **zgram** against established PEG parser generators in Rust and C++, on four JSON inputs.

All benchmarks parse the same JSON files in a tight loop, auto-calibrating the iteration count until total time exceeds 2 seconds, and report wall-clock time per parse. Every library parses JSON with an equivalent grammar.

## Two fair comparisons

Parsers differ in how much work a parse does, so the results are split in two:

- **Building a parse tree.** zgram always records a node per rule match (rule, byte span, children, subtree size): 13 / 286 / 3,706 / 2,802 nodes for the four inputs. rust-peg builds the *same* tree here through rule actions (identical node counts), and pest builds its token-pair queue.
- **Validation only.** rust-peg's default benchmark, PEGTL and cpp-peglib only check that the input matches; they build nothing. zgram's `--validate` mode wraps the grammar in a `root = value` rule and makes every other rule `@silent`, so it also builds nothing beyond one root node.

In both groups zgram also tracks the furthest failure position for error messages ("line L, col C: expected X").

## Libraries Under Test

| Library | Language | Strategy | Output |
|---------|----------|----------|--------|
| [zgram](https://github.com/dzonerzy/zgram) | Zig | Runtime JIT: grammar compiled to native code via LLVM at runtime | Flat node array (or validation only with `--validate`) |
| [rust-peg](https://github.com/kevinmehall/rust-peg) (tree actions) | Rust | Compile time: `peg::parser!` macro generates parser code | Tree of `Node { rule, start, end, children }` built by actions |
| [pest](https://pest.rs/) | Rust | Compile time: derive macro generates parser from a `.pest` grammar | Token pair queue (start/end pairs per rule) |
| [rust-peg](https://github.com/kevinmehall/rust-peg) | Rust | Compile time, as above | Validation only (rules return `()`) |
| [PEGTL](https://github.com/taocpp/PEGTL) | C++ | Compile time: header-only template metaprogramming | Validation only |
| [cpp-peglib](https://github.com/yhirose/cpp-peglib) | C++ | Runtime interpreter: grammar parsed and interpreted at runtime | Validation only |

## Inputs

- **Small** (43 bytes): `{"name": "John", "age": 30, "active": true}`
- **Medium** (1,201 bytes): 20 user objects with id, name and email.
- **Large** (15,241 bytes): 100 user objects with id, name, a 10-number scores array and a flag.
- **Strings** (74,903 bytes): 200 log-like records with a 40-word message and a long path, typical of string-heavy payloads.

## Results

Time per parse; in parentheses, how many times slower than zgram.

### Building a parse tree

| Library | Small (43 B) | Medium (1.2 KB) | Large (15 KB) | Strings (75 KB) |
|---|---|---|---|---|
| **zgram** | **0.06us** | **1.22us** | **20.20us** | **13.02us** |
| rust-peg (tree actions) | 0.16us (2.7x) | 4.37us (3.6x) | 79.78us (3.9x) | 126us (9.7x) |
| pest | 0.90us (15.0x) | 22.78us (18.7x) | 202us (10.0x) | 1,249us (95.9x) |

### Validation only

| Library | Small (43 B) | Medium (1.2 KB) | Large (15 KB) | Strings (75 KB) |
|---|---|---|---|---|
| **zgram** (validate-only mode) | **0.04us** | **0.85us** | **13.13us** | **9.36us** |
| rust-peg | 0.09us (2.2x) | 2.25us (2.6x) | 24.66us (1.9x) | 112us (12.0x) |
| PEGTL | 0.19us (4.8x) | 5.73us (6.7x) | 46.58us (3.5x) | 447us (47.8x) |
| cpp-peglib | 8.72us (218.0x) | 291us (342.7x) | 2,388us (181.8x) | 18,251us (1949.9x) |

## Analysis

**zgram is the fastest in both groups on every input.** Building the same 3,706-node tree for the large input, rust-peg with tree actions takes 3.9x longer than zgram, and pest 10x longer. Doing validation only, rust-peg takes 1.9-2.6x longer on the regular inputs, and PEGTL, a compile-time C++ template parser, 3.5-6.7x.

**Why zgram is fast:**

- The grammar is JIT-compiled with LLVM for the exact CPU it runs on, with each rule a native function and node allocation inlined.
- Nodes go into one flat, contiguous array (16 bytes per node); there is no allocation per node. That's most of the gap to rust-peg's tree variant, which allocates a `Vec` of children per node.
- Character-class loops test the first 8 bytes one at a time and switch to 16/32-byte SIMD only for longer runs. Most runs in JSON are short (a space, a few digits), where a vector step costs more than it saves; long runs, like the strings input, are where SIMD pays off: zgram is 9.7x faster than tree-building rust-peg there and 12x faster than validation-only rust-peg.

**cpp-peglib** is the slowest by a wide margin: as a runtime *interpreter* (walking the grammar), it pays heavy per-character overhead. zgram is also a runtime approach, but it compiles to machine code instead of interpreting.

### Key Takeaway

> zgram compiles a grammar at runtime into a parser that beats compile-time parser generators in Rust and C++, whether or not they build a tree -- and zgram builds one.

## Reproducing

```bash
# Prerequisites: Zig 0.16+, CMake 3.14+, Rust/Cargo, Python 3
bash benchmark/run_all.sh
```

## Environment

- CPU: AMD Ryzen 9 9950X3D 16-Core Processor (WSL2, Linux 6.18), run pinned to one core
- Zig 0.16.0, GCC 11.4, Rust 1.90.0, CMake 3.22
- All native builds use maximum optimization (`-O3` / `ReleaseFast` / `--release` with LTO)
