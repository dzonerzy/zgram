# Changelog

All notable changes to zgram are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - Unreleased

First release on PyPI, as **`zgram-py`** (`pip install zgram-py`; the module is still `import zgram`). Wheels are abi3 (CPython 3.10+) for x86_64 Linux (manylinux_2_17) and x86_64 Windows.

### Added
- **`@memo` annotation** for packrat memoization of a rule: each rule runs at most once per input position, turning exponential backtracking into linear time (14 nesting levels of a classic ambiguous expression grammar: 56 ms → 0.014 ms). Results and errors are identical to the unmemoized grammar.
- `a / b` is accepted as ordered choice, as documented (only `a | b` worked).
- **`node.to_tuple(spans=False)`** builds the whole subtree as nested `(rule, text, children)` tuples in one native pass, 6-7x faster than walking it through the Node API.
- **Compile cache:** the 16 most recently compiled grammars are cached, so compiling a grammar again takes microseconds instead of ~100 ms. `zgram.clear_cache()` empties it.
- **`zgram.compile_async(grammar)`** compiles on a worker thread without blocking the event loop.
- **`parser.parse(input, start="rule")`** parses with any rule as the start rule, and **`parser.match(input, start=None)`** matches a prefix of the input (like `re.match`), returning `None` if it doesn't match.
- **`parser.rules()`** lists the grammar's rule names.
- **`zgram.ParseError` has `line`, `column`, `offset` and `message` attributes.**
- **Parallel parsing:** `compile()` releases the GIL, and so do `parse()`/`match()` for inputs of 16 KB or more. A `GrammarParser` can be shared between threads.
- **Faster generated code:** character-class loops test the first 8 bytes inline and hand longer runs to an out-of-line 16/32-byte (SSE2/AVX2) vector loop, and loops like JSON's `(escape | plain)*` are vectorized through `@silent` rules and alternatives. The 15 KB benchmark JSON parses in 20 µs instead of 33 µs, and string-heavy JSON about 5x faster. zgram now beats rust-peg on every benchmark input, whether rust-peg builds a tree or only validates.

### Changed
- **`parser.get_error()` is now the property `parser.error`,** which returns a `ParseErrorInfo` (previously a class that shared the name `ParseError` with the exception). PyOZ exposes `get_X` methods as properties.
- **Each `parse()` returns an independent tree.** Nodes reference their own tree, which holds the input and a copy of the parse result, instead of the parser's reusable buffers.
- `parse()` accepts `bytes` (UTF-8) as well as `str`, and reads the string's UTF-8 buffer in place instead of copying the input.
- `find()` scans the node's subtree linearly and returns matches in document order, including the node itself.
- `rule()` returns one interned string per rule, so `a.rule() is b.rule()` holds.
- Requires Python 3.10+ and builds with Zig 0.16 and PyOZ 0.13.

### Fixed
- **Nodes from an earlier parse broke after the next `parse()`** on the same parser. They returned text from the new input, or read freed memory once the buffers grew.
- **Iterating a node's children was O(n²).** Each step re-walked the siblings from the first child. Iteration, `children()` and in-order indexing are now O(1) per child: iterating 2000 children went from 3 ms to 30 µs.
- **Nested loops over the same node interfered.** `iter(node)` now returns a separate iterator each time.
- **Memory leaks:** `children()`, `find()` and `dump_ir()` leaked their buffers on every call, and compiled parsers were never freed.
- A `ParseError` example in the README called methods that the raised exception doesn't have.
- **Wrong parse trees after backtracking,** found by a new differential test against a reference PEG interpreter (random grammars and inputs, now part of the test suite). In every case the tree was silently wrong; no error was raised:
  - When a branch of an alternative (or a repetition, optional or predicate) failed after matching a child, the parent's child count wasn't rolled back, e.g. `item = word ':' num / word` reported 3 children for 2.
  - Backtracking over a `@silent` rule that produces nodes left stale nodes in the tree.
  - `x+` where `x` can match the empty string produced `x` twice.
  - A lookahead `&e` that failed part-way through `e` left `e`'s nodes behind.

### Known issues
- Each distinct grammar compiled leaves about 130 KB inside LLVM's JIT after its parsers are freed. Repeated grammars are served from the cache.
- Rules without `@memo` still backtrack exponentially on adversarial grammars and input; annotate the re-tried rules.
- Free-threaded CPython (3.14t) isn't supported: the wheels use the stable ABI.
- ARM (aarch64 Linux, Windows on ARM, Apple Silicon) is not supported yet: the bundled LLVM only includes the x86 code generator.
