# Changelog

All notable changes to zgram are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed
- **Recovery on real files.** One stray token could end recovery and make the rest of a large file an error node; measured over nmap's Lua library joined into one 3 MB file with errors injected (a stray `)` at the start of random lines), each error is now one error, the file's structure kept:
  - Brackets in comments and strings no longer count as the code's: a `(` in a comment made a later stray `)` look like the closing bracket of something, so recovery stopped there instead of skipping it. The recovering parser notes where it matched brackets as literals and counts those.
  - The look for a place to resume skips what follows an element (whitespace, comments: `(stmt ws)*`) and quoted strings, so it no longer resumes in the middle of a comment or a string (a `http:` in a comment read as a statement, and every line after it).
  - A list of statements ends at a word of the grammar where no statement starts (`end`, `return`, `until`, `else`): broken text before a block's `end` no longer swallows the `end`.
  - A statement that matched short of a known error (`x` of `x = ) 5`, where `x` alone is a statement) is undone, and the whole broken statement skipped, rather than the block around failing.
  - Chains (operators, suffixes, arguments) recover from an error inside one of their elements only: one where their next element would begin is the next statement's (`f(x)` then a stray `)` on the next line no longer let the call's suffixes skip to the next `.`).
  - A missing closing literal (`end`, `)`, `}`) found later on its line is matched there, the broken text before it an error node (`)   end,` closes the function).
  - A list's first element in brackets (a table's first field) recovers as the others do (a stray `)` before it no longer closes the table).

## [0.4.2] - 2026-10-07

### Fixed
- **Windows: Python's exceptions as PyOZ maps errors to them.** The Windows wheel held, for each `PyExc_*` exception, the address of its import slot instead of the exception (PyOZ took the address of Python's data for known while compiling, and the linker filled a table of them with the slots'): raising one of them from that table handed Python a bad pointer. Built with PyOZ 0.13.10, which loads Python's data at run time on Windows and refuses a build with such constants.

## [0.4.1] - 2026-10-07

### Changed
- Built with PyOZ 0.13.9.

## [0.4.0] - 2026-10-05

### Added
- **`parser.labels()`**: each rule's labels, as `(label, many)` pairs, `many` when the label is a list in the AST (inside `*`/`+`, on a silent rule that repeats, or used twice). For tools that read labelled fields without building the AST (zrun).
- **`zgram.llvm_capsule()`** (`"zgram.llvm.v1"`, `zgram.LLVM_ABI`): zgram's LLVM for native code in other packages, so a package generating code (zrun) uses the LLVM zgram carries instead of a second copy. It exports LLVM's C API, its functions looked up by name, to build modules in memory as zgram's own code generator does. A module handed over is verified, optimized for this CPU and added to zgram's JIT, with native functions callable by name and each module's code freed on its own; or it is emitted as an object file for any x86-64 target. An object file made for this process loads back into the JIT (`load_object`: compiled code kept between runs, a cache), and the host's CPU name and features are exported to key such a cache by. LLVM's bitcode functions are exported too, to copy a module into a context of its own (to compile it on another thread). The layout is `src/llvm_capsule.zig` (ABI 2).

## [0.3.5] - 2026-10-04

### Fixed
- **Recovery picks the repair the rest of the statement agrees with.** The punctuation that begins an optional part (`(ws '=' ws value)?`, `(ws ':' ws type)?`) is now guessed when it's missing at an error, and a sequence that fails after a guess is tried again without it. `let end start.plus(3);` was `let end` with a `;` made up and `start.plus(3);` a statement of its own; it is now one statement with the value `start.plus(3)` (a missing `=`), and `let x int;` one with the type `int` (a missing `:`). Parsing without recovery is unchanged; recovering from errors costs about 5% more.

## [0.3.4] - 2026-10-01

### Added
- **`parser.expected(input, offset=None, start=None)`**: the literals the grammar could take at a position, given the text before it, in the order they are tried: an editor's keyword completion (`else` only after an `if`'s block, `{` after a `while`'s condition). Lookaheads' literals (`!keyword`) and those of rules that can match nothing (whitespace) aren't included.

## [0.3.3] - 2026-10-01

### Added
- **`parser.literals()`**: the grammar's literals (`'let'`, `';'`, `'=='`), each once, in the order they first appear: the keywords and operators an editor highlights and completes.

## [0.3.2] - 2026-10-01

### Fixed
Error recovery (`recover=True`) made nonsense trees or messages in some cases:
- **An invented first item.** `let end start.plus(3);`, with the grammar's `(ws ':' ws type)?`, had a `:` inserted (the `ws` before it counted as the sequence's first item), which made `start` a type and broke the rest of the statement. The first item of a sequence that consumes input is now never inserted, except a list's punctuation separator (`f(a 2)` still gets its `,`).
- **An invented opening bracket.** `f a, 2)` had a `(` inserted after `a`, starting a call that then needed closing. Opening brackets are never inserted.
- **A missing closer before whitespace.** `fn f(a -> int {`: the furthest failure is at the space before `->`, the `)` is missing after it, and the function fell apart into three errors. A literal is now inserted when a known error is only whitespace away, and the error is reported where the literal is missing.
- **A stray closing bracket ended recovery.** A `)` that nothing opened stopped skipping and made the rest of the file one error node. Stray closers are now skipped.
- **Messages.** The error `parse()` raises is now always among the recovered errors, with the same position and message (and `parser.error` is it): it is diagnosed the way `parse()` does, from the start of the input. Errors where a literal was inserted are diagnosed from the node that expected it (a missing `end` says `expected ... or 'end'`, not just what the block inside could contain).

### Performance
- The recovering parser compiles in about half the time (240 to 120 ms for the benchmark grammar): its probe copies of the rules are no longer forced inline.
- Recovering from errors now diagnoses the first one from the start of the input, as a failing `parse()` does: about 2 ms for an error 50 KB into a file.

## [0.3.1] - 2026-10-01

### Fixed
- **`matches()` and `recover=True` never finished compiling on some large grammars.** The validator inlined every rule except one per recursion cycle into its callers, which grows exponentially when rules share sub-rules the way precedence levels do (`and_exp = cmp_exp ('and' cmp_exp)*`): with a Lua grammar, LLVM spent minutes optimizing, and the recovering parser, which carries validator copies of the rules, inherited the problem. Rules are now inlined smallest first, each only if no function grows past a size limit. The Lua grammar's validator compiles in 0.56 s and its recovering parser in 0.96 s; grammars under the limit, such as the benchmark's JSON and expression grammars, compile to the same code as before and validate at the same speed.

## [0.3.0] - 2026-09-30

### Added
- **Error recovery:** `parse()`, `parse_tree()` and `parse_ast()` take `recover=True`. A syntax error then doesn't raise: the broken text becomes an error node (rule `"<error>"`, a leaf) and parsing goes on after it; `tree.errors` lists every error as a `Diagnostic`, in source order, with the message a plain parse gives it. It works on any grammar without changes. Repetitions of elements that make nodes (statements, items, arguments) skip a broken element to the next place, outside the brackets it opened, where an element matches; skipping stops at a closing bracket the broken text didn't open. A literal that isn't the first of its sequence is taken as present when it is missing right at an error (`let a 1;` keeps its statement with `expected '='`; an unclosed block at the end gets `expected '}'`). Up to 100 errors per parse. `parse_ast()` converts error nodes to `None`.
- **`@recover(expr)`** on a rule: a broken element that begins with that rule is skipped through the first match of `expr` (`@recover(';') stmt = ...`).
- **`Tree.errors`**: the syntax errors a recovered tree was parsed from (empty otherwise).

### Changed
- **A grammar can have up to 4095 rules** (was 4096): the last rule id is reserved for error nodes.
- **`Diagnostic` repr** shows its strings as Python reprs, so quotes in messages (`"expected '='"`) are escaped.

### Performance
- Parsing without errors, with or without `recover=True`, is unchanged: the recovering parser only runs after a parse fails. It is compiled the first time that happens and cached with the grammar. A recovered parse costs about one more parse per error, each stopping at the next error (1 error in a 0.55 MB file: 2.5 ms against 1.4 ms for a valid one).

## [0.2.1] - 2026-09-30

### Fixed
- **Deeply nested input no longer crashes the process.** The parser recurses once per level of nesting, and input such as 100,000 open parentheses (or 3,000 on Windows, whose threads have 1 MB stacks) overflowed the native stack. Recursive rules now check their frame against the thread's stack bounds and fail the parse with a `ParseError` ("nested too deeply to parse") at the position reached; `match()` returns `None` and `matches()` `False` with `parser.error` set. The error diagnosis pass stops at the same bound. The check costs 0.5-2% of parse time.

## [0.2.0] - 2026-09-30

### Added
- **Labels:** `label:rule` names a child. The label is stored in the node: `node.field()`, `node.get(label)`, `node.get_all(label)`, `parser.fields()`.
- **Chain folding:** `@left`, `@right` and `@postfix` on a rule `head (group)*` make the parser produce nested nodes (`1+2-3` → `sum(sum(1 + 2) - 3)`), and no node at all when nothing repeats. `@postfix` lets each suffix node (call, index, member access) adopt what is on its left.
- **AST mapping:** `-> name` after a rule and `parser.parse_ast(text)` convert the tree to values in one native pass: built-ins `str`, `int`, `float`, `True`, `False`, `None`, `list`, `tuple`, `dict`, `first`, `drop`, or any class/callable supplied with `zgram.compile(grammar, ast=...)` or `parser.bind(ast)`. Labelled children become keyword arguments; built objects get `__zspan__ = (start, end)` and `__znode__` (their node's index). `node.to_ast()` converts a subtree of an existing tree. `-> unquote` turns a quoted string literal into its text, replacing backslash escapes.
- **Precise syntax errors:** a missing `;` or `)` is reported where it was expected, as `expected ';'` or `expected ',' or ')'`, instead of at the start of the enclosing rule; a rule that failed where it started is expected by name, the outermost one (`expected expr`). A failed parse is re-run in an interpreter to find the furthest failure; successful parses are unaffected.
- **Display names:** `expr "expression" = ...` makes error messages say `expected expression` instead of the rule's name.
- **`-> Name()`** calls a class with no arguments, for rules such as `break_stmt` that carry no information.
- **`parser.actions()`** lists each rule's `-> name` action.
- **`tree.node(index)`** returns the node at an index of the node array.
- **`node.parent()`** returns a node's parent.
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
