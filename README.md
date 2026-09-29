<div align="center">

<img src="https://raw.githubusercontent.com/dzonerzy/zgram/main/docs/assets/logo.svg" alt="zgram Logo" width="150">

# zgram

**JIT-compiled PEG parser generator for Python.**

Compiles grammars to SIMD-accelerated native code via LLVM JIT, callable from Python with zero-copy text access and a rich Pythonic API.

[![GitHub Stars](https://img.shields.io/github/stars/dzonerzy/zgram?style=flat)](https://github.com/dzonerzy/zgram)
[![Python](https://img.shields.io/badge/python-3.10+-blue)](https://www.python.org/)
[![Zig](https://img.shields.io/badge/zig-0.16+-orange)](https://ziglang.org/)
[![License](https://img.shields.io/badge/license-MIT-green)](https://github.com/dzonerzy/zgram/blob/main/LICENSE)

Built with [PyOZ](https://github.com/pyozig/PyOZ)

</div>

---

## Performance

zgram compiles PEG grammars into SIMD-accelerated native code via LLVM JIT at runtime. No subprocess, no disk cache, no `.so` files -- grammars compile in-process in milliseconds.

On a JSON parsing benchmark (from Python, including call overhead):

```
Small JSON (43 bytes):    0.1us  -  8x faster than json.loads
Medium JSON (1.2KB):      1.3us  -  3x faster than json.loads
Large JSON (15KB):       21.2us  -  4x faster than json.loads
```

Compared to other Python parser generators:

| Parser | Type | Small (43B) | Medium (1.2KB) | Large (15KB) |
|--------|------|-------------|----------------|--------------|
| **zgram** | **PEG, LLVM JIT** | **0.1us** | **1.3us** | **21.2us** |
| json.loads | Hand-tuned C | 0.9us | 4.1us | 81.1us |
| pe | PEG, C ext | 12.4us (107x) | 248us (191x) | 4,069us (192x) |
| parsimonious | PEG, pure Python | 96.6us (835x) | 3,257us (2507x) | 44,615us (2108x) |
| pyparsing | Combinator | 102us (879x) | 2,017us (1552x) | 31,566us (1491x) |
| lark | Earley | 634us (5478x) | 17,231us (13262x) | 373,682us (17653x) |

Against native parser generators, zgram is the fastest on every input whether or not the others build a tree: building the same tree, rust-peg takes 2.7-9.7x longer and pest 10-96x; doing validation only, rust-peg takes 1.9-12x longer and PEGTL 3.5-48x (see [BENCHMARK.md](https://github.com/dzonerzy/zgram/blob/main/BENCHMARK.md)). String-heavy input is where the SIMD code shines: a 75 KB JSON document of long strings parses in 13us (5.8 GB/s).

> `json.loads` does **more** work (parses + builds Python dicts/lists). zgram returns a zero-copy parse tree.

### SQL-to-MongoDB Converter

The included [sql2mongo example](https://github.com/dzonerzy/zgram/tree/main/examples/sql2mongo) demonstrates zgram as a real-time query translator.
It walks the tree through `to_tuple()`, so zgram's share of each conversion (parse plus tree export) is about 1-1.5us; the rest is the Python walk and formatting the MongoDB query with `json.dumps`:

```
Query                    Parse (us)   Convert (us)   Overhead      Ops/sec
---------------------- ------------ -------------- ---------- ------------
Simple SELECT *               0.1us          7.1us      7.0us     140,434
WHERE filter                  0.3us         12.5us     12.2us      80,184
AND + comparisons             0.3us         15.4us     15.2us      64,739
BETWEEN range                 0.1us         10.9us     10.7us      91,836
IN list                       0.2us         12.4us     12.3us      80,329
LIKE pattern                  0.2us         10.6us     10.4us      94,768
IS NOT NULL                   0.2us          9.9us      9.8us     100,631
ORDER + LIMIT                 0.3us         15.3us     15.0us      65,262
DISTINCT                      0.1us          1.2us      1.1us     854,145
COUNT aggregate               0.3us         12.7us     12.4us      78,905
Nested boolean                0.4us         22.3us     21.9us      44,795
Pagination                    0.2us         12.8us     12.7us      77,836
```

## Installation

```bash
pip install zgram-py
```

The package is named `zgram-py` on PyPI (`zgram` is taken); the module is `zgram`:

```python
import zgram
```

Prebuilt wheels cover CPython 3.10+ on **x86_64 Linux** (glibc 2.17+) and **x86_64 Windows**. ARM (aarch64 Linux, Windows on ARM, Apple Silicon) isn't supported yet: the bundled LLVM only includes the x86 code generator.

### From source

Requires [Zig](https://ziglang.org/) 0.16. `pip install .` builds through the [PyOZ](https://github.com/pyozig/PyOZ) build backend.

```bash
pip install .
```

## Quick Start

```python
import zgram

# Define a grammar using PEG syntax
parser = zgram.compile("""
    value   = object | array | string | number | 'true' | 'false' | 'null'
    object  = '{' (pair (',' pair)*)? '}'
    pair    = string ':' value
    array   = '[' (value (',' value)*)? ']'
    string  = '"' (escape | plain)* '"'
    @silent escape = '\\\\' ["\\\\/bfnrt]
    @silent plain  = [^"\\\\]+
    number  = '-'? ('0' | [1-9] [0-9]*) ('.' [0-9]+)?
""")

# Parse input - returns the root Node
tree = parser.parse('{"name": "Alice", "scores": [100, 200]}')

# Navigate the tree
print(tree.rule())    # 'value'
print(tree.text())    # '{"name": "Alice", "scores": [100, 200]}'
print(len(tree))      # number of direct children
```

## Grammar Syntax

zgram uses PEG (Parsing Expression Grammar) syntax:

```
rule_name = expression
```

| Syntax | Meaning |
|--------|---------|
| `'literal'` | Match exact string |
| `[a-z]` | Character class |
| `[^a-z]` | Negated character class |
| `.` | Any character |
| `a b` | Sequence (match a then b) |
| `a / b` or `a \| b` | Ordered alternative |
| `e*` | Zero or more |
| `e+` | One or more |
| `e?` | Optional |
| `(e)` | Grouping |
| `!e` | Negative lookahead (not predicate) |
| `&e` | Positive lookahead (and predicate) |
| `@silent` | Annotation: suppress node in parse tree |
| `@memo` | Annotation: cache the rule's result per position (packrat) |

The first rule is the start rule.

### `@silent` Annotation

Rules annotated with `@silent` match input but produce no node in the parse tree. Use this for whitespace, delimiters, and other structural rules you don't need in the tree:

```
value  = ws (number | string) ws
number = [0-9]+
string = '"' chars '"'
@silent chars = [^"]*
@silent ws    = [ \t\n\r]*
```

### `@memo` Annotation

PEG parsers backtrack: when an alternative fails, the next one re-parses the same input. Usually that's cheap, but a rule tried several times at the same position on every level of nesting makes parsing exponential:

```
expr = term '+' expr / term '-' expr / term
@memo term = '(' expr ')' / 'x'
```

`@memo` caches each rule result per input position (packrat parsing), so the rule runs at most once per position. On 14 levels of parentheses the grammar above parses in 0.014 ms with `@memo` and 56 ms without, and each two extra levels multiply the unmemoized time by 10. Results and error messages are identical either way. Memoization costs a table lookup per call, so add it to the rules that are re-tried, not everywhere. Annotations combine in any order: `@silent @memo ws = ...`.

Predicates (`!e`, `&e`) are composable: `!!e`, `!&e`, `&!e` all work as expected.

## API Reference

### Module Functions

```python
zgram.compile(grammar: str) -> GrammarParser
```
Compile a PEG grammar string into a native parser via LLVM JIT. Compilation happens in-process -- no subprocess, no disk I/O -- and releases the GIL. The 16 most recently compiled grammars are cached, so compiling the same grammar again returns in microseconds.

```python
await zgram.compile_async(grammar: str) -> GrammarParser
```
Compile on a worker thread without blocking the event loop (a cold compile takes ~100 ms of LLVM work).

```python
zgram.clear_cache() -> None
```
Drop the compiled-grammar cache. Existing parsers keep working.

```python
zgram.dump_ir(grammar: str) -> str
```
Return the LLVM IR text for a grammar (useful for debugging/optimization).

```python
zgram.version() -> str
```
Return the zgram version string.

### GrammarParser

```python
parser = zgram.compile("start = [a-z]+")
tree = parser.parse("hello")
```

- **`parse(input: str | bytes, start: str | None = None) -> Node`** -- Parse the whole input and return the root node. Raises `ParseError` on failure. `start` picks the start rule (default: the first rule). `bytes` input must be UTF-8 if you call `text()`.
- **`match(input: str | bytes, start: str | None = None) -> Node | None`** -- Match the start rule at the beginning of the input without requiring it to consume everything (like `re.match`). The root node's `end()` is where the match stopped. Returns `None` if it doesn't match.
- **`rules() -> list[str]`** -- The grammar's rule names, in definition order.
- **`error -> ParseErrorInfo | None`** -- Property with error details from the last failed `parse()`/`match()`.

```python
parser = zgram.compile(json_grammar)
parser.parse("42", start="number")      # parse with another start rule
m = parser.match('{"a": 1} trailing')   # prefix match
print(m.end())                           # 9
```

### Node

A node in the parse tree. Supports the full Python sequence and iterator protocols.

Each `parse()` call produces its own tree, which keeps the input string and the parser alive. Nodes stay valid after the parser variable goes out of scope and after later `parse()` calls on the same parser:

```python
node = zgram.compile("root = [a-z]+").parse("hello")
print(node.text())  # "hello" -- the node keeps its tree, input and parser alive

first = parser.parse("[1, 2]")
second = parser.parse("[3]")
print(first.text())  # still "[1, 2]"
```

#### Methods

```python
node.rule()        # Grammar rule name that matched: 'string'
node.text()        # Matched text (zero-copy): '"hello"'
node.start()       # Byte offset of match start: 0
node.end()         # Byte offset of match end: 7
node.child_count() # Number of direct children: 2
node.child(i)      # Get child by index, or None
node.children()    # All children as a list[Node]
node.find("name")  # This node and its descendants matching a rule -> list[Node]
node.to_tuple()    # Whole subtree as nested tuples, built natively (see below)
```

`start()` and `end()` are byte offsets into the UTF-8 encoded input. They equal string indices only for ASCII input; for other text, use `text()` or slice `input.encode()`.

`rule()` returns the same interned string object for every node of a rule, so `node.rule() is other.rule()` holds and comparisons are cheap.

#### Protocols

```python
len(node)           # Same as child_count()
node[0]             # Indexing with negative index support
node[-1]            # Last child

for child in node:  # Iteration over direct children (O(1) per step;
    print(child)    # nested loops over the same node are independent)

str(node)           # Matched text
repr(node)          # "Node('rule', 0..7, 2 children)"
bool(node)          # Always True (a Node means the parse succeeded)
node1 == node2      # Equality by position and parse identity
```

#### Bulk export: `to_tuple()`

Creating a Python object per node is the main cost of walking a tree from Python. `to_tuple()` builds the whole subtree in one native pass as nested `(rule, text, children)` tuples -- or `(rule, start, end, text, children)` with `spans=True`:

```python
tree = parser.parse("[1, true]")
tree.to_tuple()
# ('value', '[1, true]', (('array', '[1, true]', (('value', '1', (('number', '1', ()),)), ('value', 'true', ()))),))
```

On the 15 KB benchmark JSON, converting the tree with `to_tuple()` takes 145 us, against 929 us for the equivalent walk through the Node API.

#### Tree Search

```python
# Find all nodes matching a rule name, in document order
strings = tree.find("string")
for s in strings:
    print(s.text())

# Nested iteration
for child in tree:
    for grandchild in child:
        print(grandchild.rule(), grandchild.text())
```

### ParseError

Raised when parsing fails. Error position uses high-water mark tracking -- it points to the furthest position the parser reached, not just position 0.

`zgram.ParseError` is a `ValueError` subclass whose message includes the location:

```python
try:
    tree = parser.parse('{"name": }')
except zgram.ParseError as e:
    print(e)  # "line 1, col 10: expected object"
```

The exception also carries the details as attributes:

```python
except zgram.ParseError as e:
    print(e.message)  # "expected object"
    print(e.line)     # 1
    print(e.column)   # 10 (1-based, in bytes)
    print(e.offset)   # 9 (byte offset)
```

The same details stay available from the `parser.error` property (a `ParseErrorInfo`) after a failed parse:

```python
err = parser.error
print(err.message(), err.line(), err.column(), err.offset())
```

## Architecture

```
Grammar string
     |
     v
[Grammar Parser]   -- PEG syntax -> IR (grammar_parser.zig)
     |                 Left-recursion detection, @silent annotation
     v
[LLVM Codegen]     -- IR -> LLVM IR in memory (jit_codegen.zig)
     |                 SIMD char scanning, inline node allocation,
     |                 high-water mark error tracking
     v
[LLVM JIT]         -- LLVM IR -> native code via ORC LLJIT (jit_compiler.zig)
     |                 O3 optimization, vectorization, host CPU targeting
     v
[Python API]       -- Call JIT'd function, expose Node/GrammarParser (lib.zig)
                       Zero-copy input and text, per-parse trees, freelist pooling
```

Key implementation details:

- **LLVM JIT compilation**: Grammars compile to native x86-64 code in-process via LLVM's ORC LLJIT. No subprocess, no `.so` files, no disk cache. Each grammar gets its own ResourceTracker for independent cleanup.
- **SIMD character scanning**: Character class repetitions (`[a-z]+`, `[^"\\]*`) test the first 8 bytes one at a time (most runs are a space or a few digits) and continue in an out-of-line 16-byte (SSE2) or 32-byte (AVX2) vector loop only for longer runs. Single ranges, small included sets and small excluded sets are vectorized, including through `@silent` rules and in loops like JSON's `(escape | plain)*`: when the other branches can't start with a byte of the class, runs of it are scanned in bulk and the other branches are tried only where a run stops.
- **Inline node allocation**: Rule functions reserve nodes via an inlined fast path (compare + increment) with a slow path fallback to `zgram_ensure_capacity`. Node filling is also inlined -- no function call overhead per node.
- **High-water mark errors**: Every rule failure updates `max_pos = max(max_pos, pos)`. On parse failure, the error is reported at the furthest position reached with `"expected <rule_name>"`.
- **Flat node tree**: 16-byte `FlatNode` structs in pre-order with subtree sizes. Iterating children steps from sibling to sibling in O(1), and `find()` is a linear scan, because a node's descendants are contiguous.
- **Per-parse trees**: each parse writes into its own node buffer, which becomes a tree object holding a reference to the input `str`/`bytes`. Parsing reads the string's own UTF-8 buffer (no input copy), and `text()` slices it without copying. Nodes are small (tree reference + index) and reference the tree, so they stay valid across later parses.
- **Compile cache**: compiled grammars are shared and reference-counted; the 16 most recent stay cached.

## Threads and Async

- `compile()` releases the GIL, and compiles from several threads (or `compile_async()` tasks) run in parallel.
- `parse()`/`match()` release the GIL for inputs of 16 KB or more, so threads can parse in parallel: four threads parsing a 410 KB document take about as long as one.
- A `GrammarParser` can be shared between threads. Its `error` property reflects the most recent failed parse from any thread.
- Free-threaded CPython (3.14t) isn't supported yet: the wheels use the stable ABI, which doesn't cover it.

## Known Issues

- Each distinct grammar compiled leaves about 130 KB inside LLVM's JIT after its parsers are freed. The compile cache means repeated grammars cost nothing, but compiling many *different* grammars in a long-running process grows memory slowly.

## Project Structure

```
src/
  lib.zig               # Python module: Node, GrammarParser, compile cache, ParseError
  grammar_parser.zig    # PEG grammar -> IR (with left-recursion detection)
  jit_codegen.zig       # IR -> LLVM IR (SIMD, inline alloc, HWM tracking)
  jit_compiler.zig      # LLVM ORC LLJIT compilation + ResourceTracker
  jit_helpers.zig       # Runtime helpers called by JIT code (node alloc, errors)
  llvm_builder.zig      # Ergonomic wrapper over LLVM C API
  parse_abi.zig         # FlatNode/ParseOutput C ABI structs (16 bytes per node)
test/
  conftest.py                  # Shared fixtures (JSON/list grammars)
  test_node_api.py             # Node/GrammarParser Python API tests
  test_features.py             # Compile cache, async, start rules, match, to_tuple, @memo, threads, SIMD loops
  test_differential.py         # Random grammars/inputs checked against peg_reference.py
  peg_reference.py             # Reference PEG interpreter in Python (for differential tests)
  test_grammar_correctness.py  # Grammar pattern correctness
  test_json_parsing.py         # JSON parsing: values, structures, errors
  test_edge_cases.py           # 115 edge case tests (@silent, backtracking, GC, etc.)
  test_hwm.py                  # High-water mark error position tests
  test_benchmark_json.py       # Multi-parser comparative benchmark
  test_benchmark_sql2mongo.py  # SQL-to-MongoDB latency benchmark
examples/sql2mongo/
  sql2mongo.py          # SQL SELECT -> MongoDB query converter example
build.zig               # Zig build configuration
pyproject.toml          # Python package configuration
```

## License

MIT
