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

zgram compiles PEG grammars into SIMD-accelerated native code via LLVM JIT at runtime. No subprocess, no `.so` files -- grammars compile in-process in milliseconds, and the compiled code is kept on disk: the next process loads it instead.

On a JSON parsing benchmark (from Python, including call overhead):

```
Small JSON (43 bytes):    0.1us  -  6x faster than json.loads
Medium JSON (1.2KB):      1.0us  -  4x faster than json.loads
Large JSON (15KB):       16.1us  -  5x faster than json.loads
```

Compared to other Python parser generators:

| Parser | Type | Small (43B) | Medium (1.2KB) | Large (15KB) |
|--------|------|-------------|----------------|--------------|
| **zgram** | **PEG, LLVM JIT** | **0.1us** | **1.0us** | **16.1us** |
| json.loads | Hand-tuned C | 0.8us | 3.7us | 74.2us |
| pe | PEG, C ext | 9.2us (77x) | 199us (192x) | 3,218us (200x) |
| parsimonious | PEG, pure Python | 69.7us (582x) | 2,340us (2259x) | 31,672us (1966x) |
| pyparsing | Combinator | 87.1us (727x) | 1,494us (1442x) | 25,041us (1554x) |
| lark | Earley | 511us (4269x) | 12,696us (12253x) | 261,962us (16257x) |

Against the fastest parsing libraries in C++ and Rust (Spirit X3, lexy, PEGTL, rust-peg, pest), zgram is the fastest in 15 of 16 benchmark comparisons across a JSON and an expression grammar, whether they build a parse tree or only validate: Spirit X3 building the same flat node array takes 1.3-1.7x longer on typical input, rust-peg with tree actions 3-10x, and for validation only (`matches()`) compile-time C++ takes 1.2-2.7x longer. The exception is a deeply nested expression tree, where Spirit X3 is 9% faster. See [BENCHMARK.md](https://github.com/dzonerzy/zgram/blob/main/BENCHMARK.md). String-heavy input is where zgram's SIMD code shines: a 75 KB JSON document of long strings parses in 10us (7.5 GB/s).

> `json.loads` does **more** work (parses + builds Python dicts/lists). zgram returns a zero-copy parse tree.

### A small language

[examples/tiny](https://github.com/dzonerzy/zgram/tree/main/examples/tiny) is a complete language in about 300 lines: functions, `if`/`while`/`break`/`return`, variables and expressions. The grammar builds the AST directly (labels, `@left` folding, `-> Class` actions), a tree-walking interpreter runs it, and syntax and runtime errors come out as the same kind of diagnostic:

```
program.tiny:1:13: error: expected expression [syntax]
    1 | let x = 1 + ;
      |             ^
program.tiny:2:7: error: undefined name 'y' [undefined-name]
    2 | print(y);
      |       ^
```

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
| `label:rule` | Label a child: `node.get("label")`, and a keyword argument in `parse_ast()` |
| `@left`, `@right`, `@postfix` | Annotation: fold a chain `head (group)*` into nested nodes |
| `-> name` | After a rule: what `parse_ast()` converts its node to |
| `name "display name" = ...` | What error messages call the rule (`expected expression`) |
| `@recover(expr)` | Annotation: with `recover=True`, a broken element of this rule ends after `expr` (see [Error Recovery](#error-recovery)) |

The first rule is the start rule. A grammar can have up to 4095 rules.

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

### Labels

`label:rule` on a rule reference names that child. The label is stored in the child's node:

```
if_stmt = 'if' ws cond:expr ws then:block (ws 'else' ws else_:block)?
```

```python
node.get("cond")      # the child labelled cond, or None
node.get_all("body")   # all children with that label (labels inside * or +)
child.field()         # 'cond', or None for an unlabelled node
parser.fields()       # every label of the grammar
```

Only rule references can be labelled. A label on a `@silent` rule applies to every node that rule produces. A grammar can use up to 255 distinct labels. Labels cost one extra write per labelled child; a grammar without labels compiles to the same code as before.

### Folding chains: `@left`, `@right`, `@postfix`

PEG has no left recursion, so `1 + 2 - 3` is written `product (addop product)*` and parses to a flat node `sum[1, +, 2, -, 3]`, with a `sum` wrapper around every lone operand. The fold annotations make the parser produce the nested tree instead:

```
@left sum     = left:product (ws op:addop ws right:product)*
@left product = left:atom (ws op:mulop ws right:atom)*
```

| Input | `@left` | `@right` |
|-------|---------|----------|
| `7` | `7` (no `sum` node: the operand stands in) | `7` |
| `1+2` | `sum(1 + 2)` | `sum(1 + 2)` |
| `1+2-3` | `sum(sum(1 + 2) - 3)` | `sum(1 + sum(2 - 3))` |

`@postfix` is for suffix chains where each suffix is its own kind of node: the suffix node adopts everything to its left as its first child, under the head's label.

```
@postfix post = target:primary (call | index | member)*
call   = '(' args:arglist? ')'
index  = '[' index:expr ']'
member = '.' name:ident
```

`a.b(c)[d]` becomes `index(target=call(target=member(target=a, name=b), args=c), index=d)`.

A folded rule must have the form `head (group)*` (the group may also use `?` or `+`) and can't be `@silent`. If the head isn't exactly one node, or a `@postfix` repetition isn't, the rule's own node is used as the wrapper. Folding happens during the parse, in place. Compared with the flat grammar, a folded one takes the same time or less where no operator matches, about 1.3x on input made only of single operators (`a + b`), and about 1.6x on input made only of longer chains.

### Building an AST: `-> name` and `parse_ast()`

`-> name` after a rule says what its node becomes. `parser.parse_ast(text)` parses, then converts the tree bottom-up in one native pass:

```python
from dataclasses import dataclass

@dataclass
class BinOp:
    left: object
    op: str
    right: object

@dataclass
class Number:
    text: str

parser = zgram.compile("""
    @left sum = left:number (op:addop right:number)*   -> BinOp
    number    = [0-9]+                                 -> Number
    addop     = [+\-]                                  -> str
""", ast={"BinOp": BinOp, "Number": Number})

parser.parse_ast("1+2-3")
# BinOp(left=BinOp(left=Number(text='1'), op='+', right=Number(text='2')), op='-', right=Number(text='3'))
```

| Action | Value |
|--------|-------|
| `-> str`, `-> int`, `-> float` | The matched text, converted |
| `-> unquote` | A quoted string literal's text: without its first and last character, with backslash escapes replaced (`\n \t \r \b \f \0 \xHH \uHHHH`; any other escaped character stands for itself) |
| `-> True`, `-> False`, `-> None` | That constant |
| `-> list`, `-> tuple` | The children's values |
| `-> dict` | A dict from children that are `(key, value)` tuples |
| `-> first` | The first child's value (`None` without children) |
| `-> drop` | No value: left out of the parent's children |
| `-> Name` | `Name(...)` called with the node's children, `Name` being a class or any callable from `ast` |
| `-> Name()` | `Name()` called with no arguments (`break_stmt = 'break' ';' -> Break()`) |
| (none) | A leaf's text; an only child's value; otherwise a list of the children's values |

For `-> Name`, labelled children become keyword arguments: a label inside `*`/`+` (or used twice) is always a list, any other label is the value or `None` when absent. Unlabelled children are not passed. A rule without labels passes its children's values positionally, or its matched text if it can't have children (`Number(text='1')` above).

`ast` is a dict or any object with the names as attributes (a module); `parser.bind(ast)` sets it after compiling. Objects built by `-> Name` get two attributes, unless they can't take attributes (or `parse_ast(text, spans=False)`): `__zspan__ = (start, end)`, their byte offsets, and `__znode__`, the index of their node in the tree.

`parse_ast(text)` is `parse(text).to_ast()`. Use the two-step form to keep the tree as well: `node.to_ast()` converts any subtree, and `__znode__` then identifies each object's node in `node.tree`.

A JSON grammar with `-> dict`, `-> list`, `-> float` and `-> unquote` converts the 16 KB benchmark document in about 1.5x the time of `json.loads` (85 us against 55 us).

## API Reference

### Module Functions

```python
zgram.compile(grammar: str, ast=None) -> GrammarParser
```
Compile a PEG grammar string into a native parser via LLVM JIT. `ast` supplies the classes named by `-> Name` actions (see [Building an AST](#building-an-ast---name-and-parse_ast)). Compilation happens in-process -- no subprocess -- and releases the GIL. The 16 most recently compiled grammars are cached, so compiling the same grammar again returns in microseconds. The compiled code is also kept on disk (see `configure()`): another process compiling the same grammar loads it (Lua's grammar: 8 ms instead of 0.5 s).

```python
await zgram.compile_async(grammar: str, ast=None) -> GrammarParser
```
Compile on a worker thread without blocking the event loop (a cold compile takes ~100 ms of LLVM work). `ast` is bound when the compile finishes, as in `compile()`; a missing class raises `ValueError` from the `await`. `grammar` is positional-only here.

```python
zgram.clear_cache(disk=False) -> None
```
Drop the compiled-grammar cache. Existing parsers keep working. `disk=True` deletes the compiled code kept on disk too.

```python
zgram.configure(cache=None, cache_size=None) -> None
```
Where compiled grammars are kept between processes: `cache=True` (the default: `%LOCALAPPDATA%\zgram\Cache` on Windows, `~/Library/Caches/zgram` on macOS, `$XDG_CACHE_HOME/zgram` or `~/.cache/zgram` elsewhere), `False` (none: every process compiles), or a directory. `cache_size` is the most it takes, in bytes (256 MiB by default, 0 for no limit): past it, the code used least recently is deleted, down to 80% of the limit. Each parser kind (`parse`, `matches()`, recovery) is kept as its own object file, keyed by what zgram generated for the grammar and by the zgram version, LLVM version and CPU it was compiled for: a file made for another of these is never loaded, and a damaged one is compiled again. Settings not given stay.

```python
zgram.dump_ir(grammar: str) -> str
```
Return the LLVM IR text for a grammar (useful for debugging/optimization).

```python
zgram.version() -> str
```
Return the zgram version string.

```python
zgram.llvm_capsule() -> PyCapsule   # "zgram.llvm.v1"
```
zgram's LLVM for native code in other packages, so a package that generates code (zrun) doesn't carry a second copy. `function("LLVMBuildAdd")` gives LLVM's C API functions by name, to build a module in memory the way zgram's own code generator does. `compile` takes the module, verifies it, optimizes it for this CPU and adds it to zgram's JIT (`lookup` then gives a function's address, `release` frees the module's code); `define` makes native functions callable from compiled code by name; `emit_object` compiles a module to an object file for this or another x86-64 target, and `load_object` adds one made for this process to the JIT (compiled code kept between runs: a cache, keyed with `LLVMGetHostCPUName`/`LLVMGetHostCPUFeatures`). The JIT functions are thread-safe and need no GIL; the C API follows LLVM's rules (a context is used by one thread at a time). The layout is `LlvmView` in [`src/llvm_capsule.zig`](https://github.com/dzonerzy/zgram/blob/main/src/llvm_capsule.zig); check its `abi` against `zgram.LLVM_ABI` (currently `2`) first.

### GrammarParser

```python
parser = zgram.compile("start = [a-z]+")
tree = parser.parse("hello")
```

- **`parse(input: str | bytes, start: str | None = None, recover: bool = False) -> Node`** -- Parse the whole input and return the root node. Raises `ParseError` on failure; with `recover=True`, a syntax error doesn't raise (see [Error Recovery](#error-recovery)). `start` picks the start rule (default: the first rule). `bytes` input must be UTF-8 if you call `text()`.
- **`match(input: str | bytes, start: str | None = None) -> Node | None`** -- Match the start rule at the beginning of the input without requiring it to consume everything (like `re.match`). The root node's `end()` is where the match stopped. Returns `None` if it doesn't match.
- **`matches(input: str | bytes, start: str | None = None) -> bool`** -- Does the whole input match? Runs a separate validation-only parser that builds no tree (about 2x faster than `parse()`). On `False`, `error` explains the rejection. The validator is compiled on the first call (0.1-1 s depending on grammar size) and cached with the grammar.
- **`parse_ast(input: str | bytes, start: str | None = None, spans: bool = True, recover: bool = False) -> object`** -- Parse the whole input and convert the tree to values as the rules' `-> name` actions say. Raises `ParseError` on failure; with `recover=True`, broken text converts to `None`.
- **`parse_tree(input: str | bytes, start: str | None = None, recover: bool = False) -> Tree`** -- Parse the whole input and return the `Tree` (see [Tree](#tree)); with `recover=True`, `tree.errors` lists the syntax errors.
- **`bind(ast) -> None`** -- Supply (or replace) the classes named by `-> Name` actions.
- **`rules() -> list[str]`** -- The grammar's rule names, in definition order.
- **`fields() -> list[str]`** -- The grammar's labels, in order of first use.
- **`labels() -> list[list[tuple[str, bool]]]`** -- Each rule's labels, by rule id, as `(label, many)` pairs: `many` when the label is a list in the AST (inside `*`/`+`, on a silent rule that repeats, or used twice).
- **`literals() -> list[str]`** -- The grammar's literals (`'let'`, `';'`, `'=='`), each once, in order of first appearance: an editor's keywords and operators.
- **`expected(input, offset=None, start=None) -> list[str]`** -- The literals the grammar could take at byte `offset` of `input` (its end by default), given the text before it, in the order they are tried: what an editor completes there (`let`, `if` after a statement; `else` after an `if`'s block; `-` after `let x =`). Empty when the text before has an error the parse can't get past.
- **`actions() -> list[str | None]`** -- Each rule's `-> name` action, by rule id (`None` for a rule without one).
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
node.parent()      # The node this one is a child of, or None for the root
node.tree          # The Tree this node belongs to (property)
node.index         # Index of this node in the tree's node array (property)
node.field()       # Label this node was matched under ('cond'), or None
node.get("cond")   # First child with that label, or None
node.get_all("arg") # All children with that label -> list[Node]
node.to_tuple()    # Whole subtree as nested tuples, built natively (see below)
node.to_ast()      # Subtree converted by the rules' -> actions (see parse_ast)
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

### Tree

`parser.parse_tree(text)` (or `node.tree`) gives the whole result of a parse, for code that reads the node array itself:

```python
tree = parser.parse_tree("ab=12")
tree.root      # the root Node
tree.node(i)   # the Node at index i of the node array (the inverse of node.index)
len(tree)      # number of nodes
tree.nodes     # bytes: a copy of the node array, 16 bytes per node, in pre-order
tree.input     # bytes: the parsed text as UTF-8 (node offsets index into it)
tree.rules     # rule names by rule id
tree.fields    # label names by field id - 1
tree.errors    # list[Diagnostic]: the syntax errors recovered from (recover=True)
tree.capsule   # PyCapsule "zgram.tree.v1" for native code
```

Each node is four little-endian `uint32`: `text_start`, `text_end`, `subtree_size` (number of descendants, which follow the node directly) and `meta` = child count (bits 0-11, 4095 meaning "4095 or more") | rule id (bits 12-23) | field id (bits 24-31, 0 = unlabelled). An error node of a recovered tree has rule id `len(tree.rules)` (one past the last rule).

The capsule points to this C struct (`TreeView` in `src/parse_abi.zig`), which reads the nodes and input in place, without copying. The capsule keeps the tree alive. `zgram.TREE_ABI` (currently `1`) is the struct's `abi` field; native code should check it before reading anything else.

```c
typedef struct { const char *ptr; size_t len; } zgram_str;   /* not NUL-terminated */
typedef struct { uint32_t text_start, text_end, subtree_size, meta; } zgram_node;
typedef struct {
    uint32_t abi;               /* zgram.TREE_ABI */
    uint32_t node_count;
    const zgram_node *nodes;    /* node 0 is the root */
    const char *input;
    size_t input_len;
    uint32_t rule_count;
    uint32_t field_count;
    const zgram_str *rule_names;
    const zgram_str *field_names;
} zgram_tree_v1;
```

### Diagnostic

`zgram.Diagnostic` is one error, warning or note about a source text. zgram reports syntax errors with it, and it is meant to be shared by everything built on top (semantic checks, runtime errors, editor tooling), so a language's errors look the same whichever stage finds them.

```python
d = zgram.Diagnostic("error", "break-outside-loop", "'break' outside loop", (30, 35))
print(d.render(source, "program.z"))
# program.z:4:5: error: 'break' outside loop [break-outside-loop]
#     4 |     break;
#       |     ^^^^^
```

`Diagnostic(severity, code, message, span, line=0, column=0, notes=())`: `severity` is `"error"`, `"warning"` or `"note"`; `span` is `(start, end)` in bytes of the UTF-8 source; `line` and `column` are 1-based (`0` = let `render()` work them out from the source); `notes` are further `Diagnostic`s rendered after it ("previous definition is here"). The same names are read-only properties, and diagnostics compare equal by value.

### ParseError

Raised when parsing fails. The error points to the furthest position the parser reached, and says what was expected there:

| Input | Error |
|-------|-------|
| `let a = 1` | `line 1, col 10: expected ';'` |
| `f(1 2);` | `line 1, col 5: expected ',' or ')'` |
| `let a = ;` | `line 1, col 9: expected expr` |
| `let = 1;` | `line 1, col 5: expected name` |
| `let a = 1; ?` | `line 1, col 12: unexpected input after match` |

What is expected is worked out as follows:

- **Literals and character classes** that failed at the furthest position are listed (`expected ',' or ')'`). Character classes are left out when a literal or a rule is expected too.
- **A rule name** replaces them when a rule that makes a node failed right where it started: `let a = ;` expects `expr`, not everything an expression can begin with. The outermost such rule is named, so errors read the way the grammar names its rules.
- **Display names** say it better than rule names: with `expr "expression" = ...` and `addop "operator" = ...` the messages are `expected expression` and `expected operator or ';'`. Rules sharing a display name are listed once.
- **Never reported:** failures inside predicates, inside `@silent` rules that can match nothing (whitespace, comments), and what could have made a matched token longer (another digit after `1`).
- **`unexpected input after match`**: the start rule matched part of the input and nothing failed beyond it.

The generated parser only tracks which rule failed furthest; the detail comes from re-running a failed parse in an interpreter (`src/diagnose.zig`). Successful parses pay nothing; a failing parse takes about 40x a successful one (0.8 ms for 16 KB). On very deep nesting or pathological backtracking the interpreter gives up and the rule-level error (`expected <rule>`) is reported.

`zgram.ParseError` is a `ValueError` subclass whose message includes the location:

```python
try:
    tree = parser.parse('{"name": }')
except zgram.ParseError as e:
    print(e)  # "line 1, col 9: expected value"
```

The exception also carries the details as attributes:

```python
except zgram.ParseError as e:
    print(e.message)  # "expected value"
    print(e.line)     # 1
    print(e.column)   # 9 (1-based, in bytes)
    print(e.offset)   # 8 (byte offset)
    print(e.diagnostic.render(source))   # the same error as a Diagnostic
```

The same details stay available from the `parser.error` property (a `ParseErrorInfo`) after a failed parse:

```python
err = parser.error
print(err.message(), err.line(), err.column(), err.offset())
```

## Error Recovery

An editor, a linter or a compiler that reports every error at once needs a tree even for broken text. With `recover=True`, `parse()`, `parse_tree()` and `parse_ast()` don't raise on a syntax error: the broken text becomes **error nodes** (rule `"<error>"`) and the parse goes on after it. `tree.errors` lists every error as a [`Diagnostic`](#diagnostic), in source order. No grammar changes are needed:

```python
parser = zgram.compile(r"""
    program       = ws (stmt ws)*
    @silent stmt  = let_stmt | if_stmt | expr_stmt
    let_stmt      = 'let' kw ws name:ident ws '=' ws value:expr ws ';'
    if_stmt       = 'if' kw ws cond:expr ws block
    block         = '{' ws (stmt ws)* '}'
    @silent expr_stmt = expr ws ';'
    @left expr    = left:atom (ws op:binop ws right:atom)*
    binop         = [+*]
    @silent atom  = num | call | ident | '(' ws expr ws ')'
    call          = name:ident '(' ws (args:expr (ws ',' ws args:expr)*)? ws ')'
    num           = [0-9]+
    ident         = !('let' kw | 'if' kw) [a-z]+
    @silent kw    = ![a-z]
    @silent ws    = [ \n]*
""")

src = """let a = 1;
let b = ;
if a {
  let c = f(a 2);
  let d = 4
}
"""
tree = parser.parse_tree(src, recover=True)
[(n.rule(), n.text()) for n in tree.root]
# [('let_stmt', 'let a = 1;'), ('<error>', 'let b = ;\n'),
#  ('if_stmt', 'if a {\n  let c = f(a 2);\n  let d = 4\n}')]
[n.rule() for n in tree.root[2].child(1)]     # the block keeps both statements
# ['let_stmt', 'let_stmt']
for d in tree.errors:
    print(d.render(src, "prog.txt"))
# prog.txt:2:9: error: expected expr [syntax]
#     2 | let b = ;
#       |         ^
# prog.txt:4:15: error: expected binop, ',' or ')' [syntax]
#     4 |   let c = f(a 2);
#       |               ^
# prog.txt:6:1: error: expected binop or ';' [syntax]
#     6 | }
#       | ^
```

How the parser recovers:

- **Skipping.** A repetition whose elements make nodes (statements, items, arguments) skips an element that breaks at a known error into an error node and goes on at the next place, outside any brackets the broken text opened, where an element matches (`let b = ;` above). Skipped text stops at a closing bracket it didn't open, which belongs to the construct around it (`{ let b = + }` keeps its block); a stray closing bracket that nothing before it opened is skipped like any other text. Repetitions of characters (whitespace, the digits of a number) never skip.
- **Insertion.** A literal is taken as present when it is missing at an error (or only whitespace away from it): `let a 1;` parses as a `let_stmt` with `expected '='`, `let d = 4` above as one with `expected binop or ';'`, `fn f(a -> int {` keeps its function with `expected ',' or ')'`, and a block left open at the end of the input gets its `}` (`expected '}'`). What is never made up: the first item of a sequence that consumes input, which decides whether the sequence applies (`a = 1;` isn't turned into a `let` statement), except a list's punctuation separator (`f(a 2)` gets its `,`), and the punctuation beginning an optional part, which is guessed; and an opening bracket, which would start a construct that needs closing in turn.
- **Guesses.** In `let_stmt = 'let' ws name (ws ':' ws type)? (ws '=' ws value)? ws ';'`, `let end start.plus(3);` is missing a `:` or an `=`. The optional parts' first punctuation is guessed in order: with `:`, `start` would be the type and then `.plus` can't follow, so the statement is tried again without that guess, and `=` makes `start.plus(3)` the value. `let x int;` keeps the `:` guess (`int` is the type). A word (`else`) beginning an optional part is never guessed.
- **`@recover(expr)`** on a rule: a broken element that begins with it ends after the first match of `expr` (after the brackets the broken text opened), and the parse looks for the next element from there. `@recover(';') @silent stmt = ...` skips a broken statement through its `;`, where the default could resume at a fragment of it (`2;` in `let d = + 2;`). `expr` is any expression: `@recover([;\n])`.
- **Messages:** the error `parse()` without `recover` raises is among them, identical (and `parser.error` is it); each other one is worked out from the nodes around it, the way a plain parse would.
- **Error nodes** are leaves; their rule id is the grammar's rule count, one past the last rule, and `"<error>"` isn't in `parser.rules()` or `tree.rules`. `node.find("<error>")` finds them. `to_ast()`/`parse_ast()` convert them to `None` (to get both the values and the errors, use `parser.parse_tree(src, recover=True)`, then `tree.root.to_ast()` and `tree.errors`).
- **Limits:** after 100 errors, the rest of the input is one error node. When no repetition can skip an error (nothing to resume at, as when the start rule isn't a repetition), the part of the input the start rule matched keeps its nodes and the rest becomes one error node. Input nested too deeply still raises `ParseError`.

**Cost.** Valid input parses at full speed with or without `recover=True`: the recovering parser only runs after a parse fails. It is compiled the first time that happens (about half as long as the grammar's own compile) and cached with the grammar. The first error is diagnosed as a failing `parse()` does, by re-running the input up to it in an interpreter (about 2 ms for an error 50 KB into a file); recovery then parses again once per error found, each parse stopping at the next error. On a 0.55 MB file (1.4 ms to parse): an error in the middle takes 24 ms, 30 errors 33 ms.

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

- **LLVM JIT compilation**: Grammars compile to native x86-64 code in-process via LLVM's ORC LLJIT. No subprocess, no `.so` files. Each grammar gets its own ResourceTracker for independent cleanup.
- **Disk cache** (`src/disk_cache.zig`): each parser module's object file, keyed by the module's bitcode before optimizing (salted with the zgram and LLVM versions and the host CPU), loaded into the JIT instead of compiling. Optimizing and code generation are 93% of a compile.
- **SIMD character scanning**: Character class repetitions (`[a-z]+`, `[^"\\]*`) test the first 8 bytes one at a time (most runs are a space or a few digits) and continue in an out-of-line 16-byte (SSE2) or 32-byte (AVX2) vector loop only for longer runs. Single ranges, small included sets and small excluded sets are vectorized, including through `@silent` rules and in loops like JSON's `(escape | plain)*`: when the other branches can't start with a byte of the class, runs of it are scanned in bulk and the other branches are tried only where a run stops.
- **Inline node allocation**: Rule functions reserve nodes via an inlined fast path (compare + increment) with a slow path fallback to `zgram_ensure_capacity`. Node filling is also inlined -- no function call overhead per node.
- **High-water mark errors**: Every rule failure updates `max_pos = max(max_pos, pos)`. On parse failure, the error is reported at the furthest position reached with `"expected <rule_name>"`.
- **Flat node tree**: 16-byte `FlatNode` structs in pre-order with subtree sizes; the last word packs the child count (12 bits, saturating: larger nodes are counted by stepping through their children), the rule id (12 bits) and the label's field id (8 bits). Iterating children steps from sibling to sibling in O(1), and `find()` is a linear scan, because a node's descendants are contiguous.
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
  disk_cache.zig        # Compiled grammars kept on disk between processes
  jit_helpers.zig       # Runtime helpers called by JIT code (node alloc, errors)
  llvm_builder.zig      # Ergonomic wrapper over LLVM C API
  parse_abi.zig         # FlatNode/ParseOutput C ABI structs (16 bytes per node)
  diagnose.zig          # PEG interpreter that re-runs failed parses for precise errors
test/
  conftest.py                  # Shared fixtures (JSON/list grammars)
  test_node_api.py             # Node/GrammarParser Python API tests
  test_features.py             # Compile cache, async, start rules, match, to_tuple, @memo, threads, SIMD loops
  test_disk_cache.py           # Compiled grammars kept on disk between processes
  test_differential.py         # Random grammars/inputs checked against peg_reference.py
  peg_reference.py             # Reference PEG interpreter in Python (for differential tests)
  test_grammar_correctness.py  # Grammar pattern correctness
  test_json_parsing.py         # JSON parsing: values, structures, errors
  test_edge_cases.py           # 115 edge case tests (@silent, backtracking, GC, etc.)
  test_hwm.py                  # High-water mark error position tests
  test_labels.py               # label:rule, Node.field/get/get_all
  test_fold.py                 # @left / @right / @postfix chain folding
  test_ast.py                  # -> actions and parse_ast()
  test_tree.py                 # parse_tree(), Tree, the zgram.tree.v1 capsule
  test_diagnostic.py           # Diagnostic, ParseError.diagnostic
  test_error_messages.py       # "expected ';'" errors from the diagnosis pass
  test_recover.py              # Error recovery: recover=True, error nodes, @recover
  test_deep.py                 # Deeply nested input fails cleanly
  test_example_tiny.py         # The tiny example language
  test_benchmark_json.py       # Multi-parser comparative benchmark
  test_benchmark_sql2mongo.py  # SQL-to-MongoDB latency benchmark
examples/sql2mongo/
  sql2mongo.py          # SQL SELECT -> MongoDB query converter example
examples/tiny/
  tiny.py               # A small language: grammar -> AST -> interpreter, with diagnostics
  fib.tiny              # A program in it
build.zig               # Zig build configuration
pyproject.toml          # Python package configuration
```

## License

MIT
