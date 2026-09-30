# Changelog

All notable changes to zgram are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Labels:** `label:rule` names a child. The label is stored in the node: `node.field()`, `node.get(label)`, `node.get_all(label)`, `parser.fields()`.
- **Chain folding:** `@left`, `@right` and `@postfix` on a rule `head (group)*` make the parser produce nested nodes (`1+2-3` → `sum(sum(1 + 2) - 3)`), and no node at all when nothing repeats. `@postfix` lets each suffix node (call, index, member access) adopt what is on its left.
- **AST mapping:** `-> name` after a rule and `parser.parse_ast(text)` convert the tree to values in one native pass: built-ins `str`, `int`, `float`, `True`, `False`, `None`, `list`, `tuple`, `dict`, `first`, `drop`, or any class/callable supplied with `zgram.compile(grammar, ast=...)` or `parser.bind(ast)`. Labelled children become keyword arguments; built objects get `__zspan__ = (start, end)` and `__znode__` (their node's index). `node.to_ast()` converts a subtree of an existing tree. `-> unquote` turns a quoted string literal into its text, replacing backslash escapes.
- **Precise syntax errors:** a missing `;` or `)` is reported where it was expected, as `expected ';'` or `expected ',' or ')'`, instead of at the start of the enclosing rule; a rule that failed where it started is expected by name, the outermost one (`expected expr`). A failed parse is re-run in an interpreter to find the furthest failure; successful parses are unaffected.
- **Display names:** `expr "expression" = ...` makes error messages say `expected expression` instead of the rule's name.
- **`-> Name()`** calls a class with no arguments, for rules such as `break_stmt` that carry no information.
- **`parser.actions()`** lists each rule's `-> name` action.
- **`compile_async(grammar, ast=None)`** accepts `ast`, like `compile()`.
- **`examples/tiny`:** a small language (grammar, AST, interpreter, diagnostics) built on the features above.
- **Up to 4096 rules** per grammar (was 256).

- **`parser.parse_tree(text)`, `Tree`, `node.tree`, `node.index`:** the whole result of a parse (`root`, `nodes`, `input`, `rules`, `fields`), and `tree.capsule`, a `zgram.tree.v1` PyCapsule that lets native code in other packages read the tree in place. `zgram.TREE_ABI` versions the layout.
- **`zgram.Diagnostic`:** an error, warning or note with a code, a span, notes and `render(source, filename)`; `ParseError.diagnostic` is the syntax error as one.

### Changed
- **"expected" messages** list everything expected at the furthest failure rather than the first rule that failed there: `{"a": }` with the README's JSON grammar now says `expected value`, not `expected object`.
- **Errors when the start rule matches only part of the input** now report the furthest failure beyond the match, if there is one: `let b = ;` gives `expected num` at the `;` instead of `unexpected input after match` at the start of the statement.
- **Node layout:** the last word of a node now holds the child count in 12 bits (saturating), the rule id in 12 bits and the label's field id in 8 bits. Only native code reading the node array directly is affected.
- `-> name` after a rule, which was parsed and ignored, now has a meaning for `parse_ast()`. `parse()` is unaffected.

### Fixed
- **Nodes with more than 65,535 children** reported a wrong `len()`, stopped iterating early, and could report the wrong `rule()`: the child count overflowed its 16 bits into the rule id. The stored count now saturates, and larger nodes are counted by stepping through their children.
- **Left recursion through the 65th or later alternative** of a rule went undetected, and the compiled parser would overflow the stack.
- `parser.error.message()` returned NUL bytes for "unexpected input after match".
- Formatting an "expected <rule>" error copied a buffer onto itself, which aborted debug builds of zgram.

## [0.1.0] - 2026-09-30

First release on PyPI, as **`zgram-py`** (`pip install zgram-py`; the module is still `import zgram`). Wheels are abi3 (CPython 3.10+) for x86_64 Linux (manylinux_2_17) and x86_64 Windows.

### Added
- **`parser.matches(input, start=None) -> bool`**: validation without building a tree, through a separate parser compiled on first use (no nodes, no error tracking, every rule except one per recursion cycle inlined). About 2x faster than `parse()`, and faster than Spirit X3 and lexy validating the same input in the benchmarks. On rejection, `parser.error` explains why.
- **Expression grammar benchmark** alongside JSON, and comparisons with Spirit X3, lexy, PEGTL, rust-peg, pest, PackCC and cpp-peglib, building trees and validating (BENCHMARK.md).
- **`@memo` annotation** for packrat memoization of a rule: each rule runs at most once per input position, turning exponential backtracking into linear time (14 nesting levels of a classic ambiguous expression grammar: 56 ms → 0.014 ms). Results and errors are identical to the unmemoized grammar.
- `a / b` is accepted as ordered choice, as documented (only `a | b` worked).
- **`node.to_tuple(spans=False)`** builds the whole subtree as nested `(rule, text, children)` tuples in one native pass, 6-7x faster than walking it through the Node API.
- **Compile cache:** the 16 most recently compiled grammars are cached, so compiling a grammar again takes microseconds instead of ~100 ms. `zgram.clear_cache()` empties it.
- **`zgram.compile_async(grammar)`** compiles on a worker thread without blocking the event loop.
- **`parser.parse(input, start="rule")`** parses with any rule as the start rule, and **`parser.match(input, start=None)`** matches a prefix of the input (like `re.match`), returning `None` if it doesn't match.
- **`parser.rules()`** lists the grammar's rule names.
- **`zgram.ParseError` has `line`, `column`, `offset` and `message` attributes.**
- **Parallel parsing:** `compile()` releases the GIL, and so do `parse()`/`match()` for inputs of 16 KB or more. A `GrammarParser` can be shared between threads.
- **Faster generated code:** character-class loops test the first 8 bytes inline and hand longer runs to an out-of-line 16/32-byte (SSE2/AVX2) vector loop, and loops like JSON's `(escape | plain)*` are vectorized through `@silent` rules and alternatives. The 15 KB benchmark JSON parses in 20 µs instead of 33 µs, and string-heavy JSON about 5x faster. Building a parse tree, zgram is now faster than rust-peg, Spirit X3, lexy, PEGTL and pest on every benchmark input.

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
