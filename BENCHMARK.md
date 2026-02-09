# zgram Benchmark — JSON Parse Performance

Comparison of **zgram** against four established PEG parsing libraries across three JSON input sizes.

All benchmarks parse the same JSON files in a tight loop, auto-calibrating iteration count until total time exceeds 2 seconds. Results are wall-clock time per parse and throughput (ops/sec).

## Libraries Under Test

| Library | Language | Strategy | Parse Output |
|---------|----------|----------|--------------|
| [zgram](https://github.com/dzonerzy/zgram) | Zig | Runtime JIT — grammar compiled to native code via LLVM at runtime | Flat node array (rule, span, children, subtree size) |
| [rust-peg](https://github.com/kevinmehall/rust-peg) | Rust | Compile-time — PEG macro generates parser code at compile time | Validation only (no tree) |
| [PEGTL](https://github.com/taocpp/PEGTL) | C++ | Compile-time — header-only template metaprogramming PEG | Validation only (no tree) |
| [pest](https://pest.rs/) | Rust | Compile-time — derive macro generates parser from `.pest` grammar | Token pair queue (start/end pairs per rule) |
| [cpp-peglib](https://github.com/yhirose/cpp-peglib) | C++ | Runtime interpreted — grammar parsed and interpreted at runtime | Validation only (no tree) |

## Results

### Small JSON (43 bytes)

```json
{"name": "John", "age": 30, "active": true}
```

| Library | Per-parse | Ops/sec | vs zgram |
|---------|-----------|---------|----------|
| **zgram JIT** | **0.07us** | **13,388,369** | — |
| rust-peg | 0.07us | 13,360,492 | 1.00x |
| PEGTL | 0.19us | 5,373,907 | 0.40x |
| pest | 0.76us | 1,321,527 | 0.10x |
| cpp-peglib | 7.25us | 138,023 | 0.01x |

### Medium JSON (1,201 bytes)

20 user objects with id, name, and email fields.

| Library | Per-parse | Ops/sec | vs zgram |
|---------|-----------|---------|----------|
| rust-peg | 1.87us | 533,511 | 1.12x |
| **zgram JIT** | **2.10us** | **477,115** | — |
| PEGTL | 5.47us | 182,918 | 0.38x |
| pest | 18.65us | 53,609 | 0.11x |
| cpp-peglib | 240.04us | 4,166 | 0.01x |

### Large JSON (15,241 bytes)

100 user objects with id, name, scores array, and active flag.

| Library | Per-parse | Ops/sec | vs zgram |
|---------|-----------|---------|----------|
| rust-peg | 22.33us | 44,791 | 1.44x |
| **zgram JIT** | **32.13us** | **31,128** | — |
| PEGTL | 44.68us | 22,383 | 0.72x |
| pest | 172.56us | 5,795 | 0.19x |
| cpp-peglib | 1,981.44us | 505 | 0.02x |

## Fairness Note

Not all parsers do the same amount of work:

- **zgram** and **pest** build a parse tree during parsing. zgram produces a flat array of `FlatNode` structs (rule name, start/end position, child count, subtree size) — 3,706 nodes for the large JSON input. pest eagerly builds a `Vec<QueueableToken>` of start/end token pairs for every matched rule.
- **rust-peg**, **PEGTL**, and **cpp-peglib** perform validation only — they return success/failure without constructing any data structure. In rust-peg, rules without a `-> Type` annotation return `()`.

This means zgram is doing strictly more work than rust-peg, PEGTL, and cpp-peglib on every parse.

## Analysis

**zgram** and **rust-peg** are the two fastest parsers, effectively tied on small inputs. On larger inputs rust-peg pulls ahead — but rust-peg is doing validation only, while zgram is building a full parse tree with 3,706 nodes. The fact that zgram remains within 1.4x of a validate-only parser while constructing a complete, traversable parse tree is notable.

Among tree-building parsers, **zgram is ~6x faster than pest** despite pest being compiled at Rust compile time while zgram JIT-compiles the grammar at runtime.

**PEGTL**, a compile-time C++ template approach doing validation only, lands in third place at roughly 2-3x slower than zgram — slower despite doing less work.

**pest** builds token pairs like zgram builds nodes, but is significantly slower — likely due to the overhead of its `Rc<Vec<QueueableToken>>` pair architecture vs zgram's contiguous flat array written directly by JIT'd code.

**cpp-peglib** is the slowest by a wide margin. As a runtime *interpreter* (tree-walking the grammar), it pays heavy per-character overhead.

### Key Takeaway

> zgram builds a full parse tree at runtime-JIT speed, matching or exceeding compile-time validate-only parsers on small inputs and staying competitive on large inputs — while actually producing usable output.

## Reproducing

```bash
# Prerequisites: Zig 0.15+, CMake 3.14+, Rust/Cargo, Python 3
bash benchmark/run_all.sh
```

## Environment

- CPU: AMD Ryzen 9 9950X3D 16-Core Processor (WSL2, Linux 6.6)
- Zig 0.15.2, GCC 11.4, Rust 1.90.0, CMake 3.22
- All native builds use maximum optimization (`-O3` / `ReleaseFast` / `--release` with LTO)
