<div align="center">

<img src="docs/assets/logo.svg" alt="zgram Logo" width="150">

# zgram

**JIT-compiled PEG parser generator for Python.**

Compiles grammars to SIMD-accelerated native code via LLVM JIT, callable from Python with zero-copy text access and a rich Pythonic API.

[![GitHub Stars](https://img.shields.io/github/stars/dzonerzy/zgram?style=flat)](https://github.com/dzonerzy/zgram)
[![Python](https://img.shields.io/badge/python-3.8+-blue)](https://www.python.org/)
[![Zig](https://img.shields.io/badge/zig-0.15+-orange)](https://ziglang.org/)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

Built with [PyOZ](https://github.com/pyozig/PyOZ)

</div>

---

## Performance

zgram compiles PEG grammars into SIMD-accelerated native code via LLVM JIT at runtime. No subprocess, no disk cache, no `.so` files -- grammars compile in-process in milliseconds.

On a JSON parsing benchmark:

```
Small JSON (43 bytes):    0.1us  -  6x faster than json.loads
Medium JSON (1.2KB):      2.1us  -  2x faster than json.loads
Large JSON (15KB):       32.3us  -  2x faster than json.loads
```

Compared to other Python parser generators:

| Parser | Type | Small (43B) | Medium (1.2KB) | Large (15KB) |
|--------|------|-------------|----------------|--------------|
| **zgram** | **PEG, LLVM JIT** | **0.1us** | **2.1us** | **32.3us** |
| json.loads | Hand-tuned C | 0.8us | 3.9us | 76.7us |
| pe | PEG, C ext | 9.3us (74x) | 204us (99x) | 3,375us (104x) |
| pyparsing | Combinator | 68.6us (546x) | 1,266us (615x) | 19,896us (615x) |
| parsimonious | PEG, pure Python | 68.4us (544x) | 2,438us (1185x) | 34,871us (1079x) |
| lark | Earley | 516us (4107x) | 13,330us (6478x) | 312,022us (9651x) |

> `json.loads` does **more** work (parses + builds Python dicts/lists). zgram returns a zero-copy parse tree.

### SQL-to-MongoDB Converter

The included [sql2mongo example](sql2mongo/) demonstrates zgram as a real-time query translator.
Parse latency is sub-microsecond; the Python tree-walking dominates total conversion time:

```
Query                    Parse (us)   Convert (us)   Overhead      Ops/sec
---------------------- ------------ -------------- ---------- ------------
Simple SELECT *               0.1us          7.8us      7.7us     128,260
WHERE filter                  0.4us         13.5us     13.1us      73,974
AND + comparisons             0.5us         17.9us     17.3us      55,937
BETWEEN range                 0.3us         13.3us     13.0us      75,129
IN list                       0.4us         13.5us     13.1us      74,234
LIKE pattern                  0.3us         11.8us     11.6us      84,500
IS NOT NULL                   0.3us         11.1us     10.9us      89,714
ORDER + LIMIT                 0.6us         17.9us     17.2us      56,015
DISTINCT                      0.2us          2.0us      1.8us     507,398
COUNT aggregate               0.5us         16.4us     16.0us      60,835
Nested boolean                0.8us         34.8us     34.0us      28,741
Pagination                    0.3us         14.6us     14.3us      68,532
```

## Installation

### From source

Requires [Zig](https://ziglang.org/) (0.15+) and [PyOZ](https://github.com/pyozig/PyOZ).

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

Predicates (`!e`, `&e`) are composable: `!!e`, `!&e`, `&!e` all work as expected.

## API Reference

### Module Functions

```python
zgram.compile(grammar: str) -> GrammarParser
```
Compile a PEG grammar string into a native parser via LLVM JIT. Compilation happens in-process -- no subprocess, no disk I/O.

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

- **`parse(input: str) -> Node`** -- Parse input and return the root node. Raises `ParseError` on failure.
- **`get_error() -> ParseError | None`** -- Get error details from the last failed parse.

### Node

A node in the parse tree. Supports the full Python sequence and iterator protocols.

Nodes hold a strong reference to their parser, so they remain valid even if the parser variable goes out of scope:

```python
# This is safe -- the node keeps the parser alive
node = zgram.compile("root = [a-z]+").parse("hello")
print(node.text())  # "hello"
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
node.find("name")  # Search descendants by rule name -> list[Node]
```

#### Protocols

```python
len(node)           # Same as child_count()
node[0]             # Indexing with negative index support
node[-1]            # Last child

for child in node:  # Iteration over direct children
    print(child)

str(node)           # Matched text
repr(node)          # "Node('rule', 0..7, 2 children)"
bool(node)          # Always True (a Node means the parse succeeded)
node1 == node2      # Equality by position and parse identity
```

#### Tree Search

```python
# Find all nodes matching a rule name (depth-first)
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

```python
try:
    tree = parser.parse('{"name": }')
except zgram.ParseError as e:
    print(e)           # "line 1, col 10: expected value"
    print(e.message()) # "expected value"
    print(e.line())    # 1
    print(e.column())  # 10
    print(e.offset())  # 9 (byte offset)
```

Also available via `parser.get_error()` after a failed parse.

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
                       Zero-copy text, Ref(T) node safety, freelist pooling
```

Key implementation details:

- **LLVM JIT compilation**: Grammars compile to native x86-64 code in-process via LLVM's ORC LLJIT. No subprocess, no `.so` files, no disk cache. Each grammar gets its own ResourceTracker for independent cleanup.
- **SIMD character scanning**: Character class repetitions (`[a-z]+`, `[^"\\]*`) use SSE2 vector operations to process 16 bytes per cycle. Single ranges, small included sets, and small excluded sets are all vectorized.
- **Inline node allocation**: Rule functions reserve nodes via an inlined fast path (compare + increment) with a slow path fallback to `zgram_ensure_capacity`. Node filling is also inlined -- no function call overhead per node.
- **High-water mark errors**: Every rule failure updates `max_pos = max(max_pos, pos)`. On parse failure, the error is reported at the furthest position reached with `"expected <rule_name>"`.
- **Flat node tree**: 16-byte `FlatNode` structs with subtree-size navigation. O(children) child access, zero-copy text slicing from the input buffer.
- **Node safety**: Nodes hold a `pyoz.Ref(GrammarParser)` that INCREFs the parser, preventing use-after-free when the parser is garbage collected while nodes are alive.

## Thread Safety

zgram is **not thread-safe**. Do not share `GrammarParser` instances across threads. For multi-threaded workloads, compile a separate parser per thread.

## Project Structure

```
src/
  lib.zig               # Python module: Node, GrammarParser, ParseError
  grammar_parser.zig    # PEG grammar -> IR (with left-recursion detection)
  jit_codegen.zig       # IR -> LLVM IR (SIMD, inline alloc, HWM tracking)
  jit_compiler.zig      # LLVM ORC LLJIT compilation + ResourceTracker
  jit_helpers.zig       # Runtime helpers called by JIT code (node alloc, errors)
  llvm_builder.zig      # Ergonomic wrapper over LLVM C API
  parse_abi.zig         # FlatNode/ParseOutput C ABI structs (16 bytes per node)
test/
  conftest.py                  # Shared fixtures (JSON/list grammars)
  test_node_api.py             # Node/GrammarParser Python API tests
  test_grammar_correctness.py  # Grammar pattern correctness
  test_json_parsing.py         # JSON parsing: values, structures, errors
  test_edge_cases.py           # 115 edge case tests (@silent, backtracking, GC, etc.)
  test_hwm.py                  # High-water mark error position tests
  test_benchmark_json.py       # Multi-parser comparative benchmark
  test_benchmark_sql2mongo.py  # SQL-to-MongoDB latency benchmark
sql2mongo/
  sql2mongo.py          # SQL SELECT -> MongoDB query converter example
build.zig               # Zig build configuration
pyproject.toml          # Python package configuration
```

## License

MIT
