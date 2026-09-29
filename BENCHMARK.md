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
| **zgram JIT** | **0.06us** | **16,666,667** | — |
| rust-peg | 0.08us | 12,500,000 | 0.75x |
| PEGTL | 0.19us | 5,263,158 | 0.32x |
| pest | 0.78us | 1,282,051 | 0.08x |
| cpp-peglib | 7.32us | 136,612 | 0.01x |

### Medium JSON (1,201 bytes)

20 user objects with id, name, and email fields.

| Library | Per-parse | Ops/sec | vs zgram |
|---------|-----------|---------|----------|
| rust-peg | 1.89us | 529,101 | 1.06x |
| **zgram JIT** | **2.00us** | **500,000** | — |
| PEGTL | 5.65us | 176,991 | 0.35x |
| pest | 19.63us | 50,942 | 0.10x |
| cpp-peglib | 243.82us | 4,101 | 0.01x |

### Large JSON (15,241 bytes)

100 user objects with id, name, scores array, and active flag.

| Library | Per-parse | Ops/sec | vs zgram |
|---------|-----------|---------|----------|
| rust-peg | 22.53us | 44,385 | 1.46x |
| **zgram JIT** | **32.98us** | **30,321** | — |
| PEGTL | 44.01us | 22,722 | 0.75x |
| pest | 177.39us | 5,637 | 0.19x |
| cpp-peglib | 2,013.31us | 497 | 0.02x |

## Fairness Note

Not all parsers do the same amount of work:

- **zgram** and **pest** build a parse tree during parsing. zgram produces a flat array of `FlatNode` structs (rule name, start/end position, child count, subtree size) — 3,706 nodes for the large JSON input. pest eagerly builds a `Vec<QueueableToken>` of start/end token pairs for every matched rule.
- **rust-peg**, **PEGTL**, and **cpp-peglib** perform validation only — they return success/failure without constructing any data structure. In rust-peg, rules without a `-> Type` annotation return `()`.

This means zgram is doing strictly more work than rust-peg, PEGTL, and cpp-peglib on every parse.

## Analysis

**zgram** and **rust-peg** are the two fastest parsers; zgram is slightly ahead on small inputs. On larger inputs rust-peg pulls ahead — but rust-peg is doing validation only, while zgram is building a full parse tree with 3,706 nodes. The fact that zgram remains within 1.5x of a validate-only parser while constructing a complete, traversable parse tree is notable.

Among tree-building parsers, **zgram is 5-13x faster than pest** despite pest being compiled at Rust compile time while zgram JIT-compiles the grammar at runtime.

**PEGTL**, a compile-time C++ template approach doing validation only, lands in third place at 1.3-3x slower than zgram — slower despite doing less work.

**pest** builds token pairs like zgram builds nodes, but is significantly slower — likely due to the overhead of its `Rc<Vec<QueueableToken>>` pair architecture vs zgram's contiguous flat array written directly by JIT'd code.

**cpp-peglib** is the slowest by a wide margin. As a runtime *interpreter* (tree-walking the grammar), it pays heavy per-character overhead.

### Key Takeaway

> zgram builds a full parse tree at runtime-JIT speed, matching or exceeding compile-time validate-only parsers on small inputs and staying competitive on large inputs — while actually producing usable output.

## Reproducing

```bash
# Prerequisites: Zig 0.16+, CMake 3.14+, Rust/Cargo, Python 3
bash benchmark/run_all.sh
```

## Environment

- CPU: AMD Ryzen 9 9950X3D 16-Core Processor (WSL2, Linux 6.18)
- Zig 0.16.0, GCC 11.4, Rust 1.90.0, CMake 3.22
- All native builds use maximum optimization (`-O3` / `ReleaseFast` / `--release` with LTO)
