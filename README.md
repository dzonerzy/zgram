# zgram

A comptime-optimized PEG parser generator for Python. Compiles grammars to native shared libraries via Zig, callable from Python with zero-copy text access and a rich Pythonic API.

Built with [PyOZ](https://github.com/dzonerzy/PyOZ).

## Performance

zgram compiles PEG grammars into SIMD-accelerated native code. On a JSON parsing benchmark:

```
Small JSON (43 bytes):    0.3us  -  3x faster than json.loads
Medium JSON (1.2KB):      5.3us  -  1.4x slower than json.loads
Large JSON (15KB):       72.4us  -  ~same as json.loads
```

Compared to other Python parser generators:

| Parser | Type | Small (43B) | Medium (1.2KB) | Large (15KB) |
|--------|------|-------------|----------------|--------------|
| **zgram** | **PEG, native .so** | **0.3us** | **5.3us** | **72.4us** |
| json.loads | Hand-tuned C | 0.8us | 3.8us | 75.5us |
| pe | PEG, C ext | 9.0us (34x) | 200us (38x) | 3,369us (47x) |
| pyparsing | Combinator | 69.6us (263x) | 1,257us (238x) | 19,734us (273x) |
| parsimonious | PEG, pure Python | 69.9us (264x) | 2,452us (464x) | 33,194us (459x) |
| lark | Earley | 516.9us (1,952x) | 12,987us (2,459x) | 305,548us (4,221x) |

> `json.loads` does **more** work (parses + builds Python dicts/lists). zgram returns a zero-copy parse tree.

### SQL-to-MongoDB Converter

The included [sql2mongo example](examples/sql2mongo/) demonstrates zgram as a real-time query translator.
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

Even the most complex queries (nested booleans with OR) convert in under 35us (28K ops/sec), making zgram suitable for real-time SQL-to-MongoDB translation.

## Installation

### From source

Requires [Zig](https://ziglang.org/) (0.14+) and [PyOZ](https://github.com/dzonerzy/PyOZ).

```bash
pyoz build --release
pip install dist/zgram-0.1.0-cp310-cp310-linux_x86_64.whl
```

## Quick Start

```python
import zgram

# Define a grammar using PEG syntax
parser = zgram.compile("""
    value   = object / array / string / number / 'true' / 'false' / 'null'
    object  = '{' (pair (',' pair)*)? '}'
    pair    = string ':' value
    array   = '[' (value (',' value)*)? ']'
    string  = '"' (escape / plain)* '"'
    escape  = '\\\\' ["\\\\/bfnrt]
    plain   = [^"\\\\]+
    number  = '-'? ('0' / [1-9] [0-9]*) ('.' [0-9]+)?
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

The first rule is the start rule. Rules referenced only inside repetitions are automatically silenced (their nodes are inlined into the parent).

## API Reference

### Module Functions

```python
zgram.compile(grammar: str) -> GrammarParser
```
Compile a PEG grammar string into a native parser. The compiled `.so` is cached in `~/.cache/zgram/` for instant reuse.

```python
zgram.clear_cache() -> int
```
Clear all cached grammar `.so` files. Returns the number of files removed.

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
- **`get_error() -> ParseError | None`** -- Get error details from the last failed parse (alternative to exception handling).
- **`error`** -- Property alias for `get_error()`.

### Node

A node in the parse tree. Supports the full Python sequence and iterator protocols.

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

Raised when parsing fails. Also available via `parser.get_error()`.

```python
try:
    tree = parser.parse("invalid input")
except zgram.ParseError as e:
    print(e)           # "line 1, col 5: failed to match start rule"
    print(e.message()) # "failed to match start rule"
    print(e.line())    # 1
    print(e.column())  # 5
    print(e.offset())  # 4 (byte offset)
```

## Architecture

```
Grammar string
     |
     v
[Grammar Parser] -- PEG syntax -> IR (grammar_parser.zig)
     |
     v
[Code Generator] -- IR -> Zig source (codegen.zig)
     |
     v
[Zig Compiler]   -- Zig source -> native .so (compiler.zig)
     |                (ReleaseFast, SIMD-optimized)
     v
[Cache]          -- SHA-256 keyed, ~/.cache/zgram/ (cache.zig)
     |
     v
[Python API]     -- dlopen .so, expose Node/GrammarParser (lib.zig)
```

The parser engine (`parser_engine.zig`) uses comptime code generation to specialize the parser for each grammar. Character class matching uses SIMD vector operations. The flat node tree uses a compact 16-byte-per-node layout with subtree-size-based navigation for O(children) child access.

## Project Structure

```
src/
  lib.zig                  # Python module: Node, GrammarParser, ParseError
  grammar_parser.zig       # PEG grammar -> IR
  codegen.zig              # IR -> Zig source code
  compiler.zig             # Invoke zig build on generated source
  cache.zig                # Grammar .so caching (SHA-256 keyed)
  parse_abi.zig            # FlatNode C ABI (16 bytes per node)
  templates.zig            # @embedFile bridge for template files
  zig_templates/
    parser_engine.zig      # Comptime-specialized PEG engine (SIMD)
    parse_abi.zig          # ABI copy for grammar .so builds
    grammar_types.zig      # Grammar IR types
    grammar_lib.zig        # Build scaffold for grammar .so
test/
  conftest.py              # Shared fixtures, cache cleanup
  test_node_api.py         # Python API tests (Node, GrammarParser, ParseError)
  test_grammar_correctness.py  # Grammar correctness across PEG patterns
  test_json_parsing.py     # JSON parsing: values, complex structures, errors
  test_benchmark_json.py   # Multi-parser comparative benchmark
  test_benchmark_sql2mongo.py  # SQL-to-MongoDB latency benchmark
examples/
  sql2mongo/sql2mongo.py   # SQL SELECT -> MongoDB query converter
build.zig                  # Zig build configuration
pyproject.toml             # Python package configuration
```

## License

MIT
