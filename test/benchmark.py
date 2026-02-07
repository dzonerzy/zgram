"""
Benchmark: zgram vs other Python parser generators vs json.loads (C reference).

Parsers tested:
  - zgram:         PEG, compiles grammar to native .so via Zig (this project)
  - pe:            PEG, C-extension accelerated packrat parser
  - parsimonious:  PEG, pure Python
  - lark:          Earley/LALR, pure Python
  - pyparsing:     Combinator-style, pure Python
  - json.loads:    Hand-tuned C (parse + build Python objects — does MORE work)
"""

import json
import time

import lark
import parsimonious
import pe
import zgram
from pyparsing import (
    Forward,
    Group,
    Keyword,
    Optional,
    Regex,
    Suppress,
)

# ============================================================================
# Grammar definitions
# ============================================================================

# ── zgram (PEG) ──
zgram_json_grammar = (
    "value     = ws (object | array | string | number | true_lit | false_lit | null_lit) ws\n"
    "object    = '{' ws (pair (',' ws pair)*)? ws '}'\n"
    "pair      = ws string ws ':' value\n"
    "array     = '[' ws (value (',' ws value)*)? ws ']'\n"
    "string    = '\"' chars '\"'\n"
    "chars     = char*\n"
    "char      = escape | plain\n"
    "escape    = '\\\\' [\"\\\\bfnrt/]\n"
    'plain     = [^"\\\\]\n'
    "number    = int frac? exp?\n"
    "int       = '-'? ('0' | [1-9] [0-9]*)\n"
    "frac      = '.' [0-9]+\n"
    "exp       = [eE] [+\\-]? [0-9]+\n"
    "true_lit  = 'true'\n"
    "false_lit = 'false'\n"
    "null_lit  = 'null'\n"
    "ws        = [ \\t\\n\\r]*\n"
)

# ── pe (PEG) ──
pe_json_grammar = r"""
    value     <- ws (object / array / string / number / true_lit / false_lit / null_lit) ws
    object    <- '{' ws (pair (',' ws pair)*)? ws '}'
    pair      <- ws string ws ':' value
    array     <- '[' ws (value (',' ws value)*)? ws ']'
    string    <- '"' chars '"'
    chars     <- char*
    char      <- escape / plain
    escape    <- '\\' ["\\/bfnrt]
    plain     <- !["\\] .
    number    <- int frac? exp?
    int       <- '-'? ('0' / [1-9] [0-9]*)
    frac      <- '.' [0-9]+
    exp       <- [eE] [+\055]? [0-9]+
    true_lit  <- 'true'
    false_lit <- 'false'
    null_lit  <- 'null'
    ws        <- [ \t\n\r]*
"""

# ── parsimonious (PEG) ──
parsimonious_json_grammar = r"""
    value     = ws (object / array / string / number / true_lit / false_lit / null_lit) ws
    object    = "{" ws (pair ("," ws pair)*)? ws "}"
    pair      = ws string ws ":" value
    array     = "[" ws (value ("," ws value)*)? ws "]"
    string    = "\"" chars "\""
    chars     = char*
    char      = escape / plain
    escape    = "\\" ~"[\"\\\\bfnrt/]"
    plain     = ~"[^\"\\\\]"
    number    = int frac? exp?
    int       = "-"? int_core
    int_core  = "0" / ~"[1-9][0-9]*"
    frac      = "." ~"[0-9]+"
    exp       = ~"[eE]" ~"[+\-]?" ~"[0-9]+"
    true_lit  = "true"
    false_lit = "false"
    null_lit  = "null"
    ws        = ~"[ \t\n\r]*"
"""

# ── lark (EBNF/Earley) ──
lark_json_grammar = r"""
    ?value: object | array | string | number | "true" | "false" | "null"
    object: "{" (pair ("," pair)*)? "}"
    pair: string ":" value
    array: "[" (value ("," value)*)? "]"
    string: ESCAPED_STRING
    number: SIGNED_NUMBER

    %import common.ESCAPED_STRING
    %import common.SIGNED_NUMBER
    %import common.WS
    %ignore WS
"""


# ── pyparsing (combinator) ──
def build_pyparsing_parser():
    LBRACE, RBRACE, LBRACK, RBRACK, COLON, COMMA = map(Suppress, "{}[]:,")
    value = Forward()
    string = Regex(r'"(?:[^"\\]|\\.)*"')
    number = Regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")
    true_lit = Keyword("true")
    false_lit = Keyword("false")
    null_lit = Keyword("null")
    array = Group(LBRACK + Optional(value + (COMMA + value)[...]) + RBRACK)  # type: ignore[index]
    pair = Group(string + COLON + value)
    obj = Group(LBRACE + Optional(pair + (COMMA + pair)[...]) + RBRACE)  # type: ignore[index]
    value <<= obj | array | string | number | true_lit | false_lit | null_lit
    return value


# ============================================================================
# Build parsers
# ============================================================================

print("Compiling parsers...")

t0 = time.perf_counter()
zgram_parser = zgram.compile(zgram_json_grammar)
print(f"  zgram:         {time.perf_counter() - t0:.4f}s")

t0 = time.perf_counter()
pe_parser = pe.compile(pe_json_grammar)
print(f"  pe:            {time.perf_counter() - t0:.4f}s")

t0 = time.perf_counter()
parsimonious_parser = parsimonious.Grammar(parsimonious_json_grammar)
print(f"  parsimonious:  {time.perf_counter() - t0:.4f}s")

t0 = time.perf_counter()
lark_parser = lark.Lark(lark_json_grammar, start="value", parser="earley")
print(f"  lark:          {time.perf_counter() - t0:.4f}s")

t0 = time.perf_counter()
pyparsing_parser = build_pyparsing_parser()
print(f"  pyparsing:     {time.perf_counter() - t0:.4f}s")

# ============================================================================
# Test data
# ============================================================================

small_obj = {"name": "John", "age": 30, "active": True}
small_json = json.dumps(small_obj)

medium_obj = {
    "users": [
        {"id": i, "name": f"user{i}", "email": f"user{i}@example.com"}
        for i in range(20)
    ]
}
medium_json = json.dumps(medium_obj)

large_obj = {
    "users": [
        {
            "id": i,
            "name": f"user{i}",
            "scores": [j * 1.1 for j in range(10)],
            "active": i % 2 == 0,
        }
        for i in range(100)
    ]
}
large_json = json.dumps(large_obj)

# ============================================================================
# Parse functions
# ============================================================================


def zgram_parse(s):
    return zgram_parser.parse(s)


def pe_parse(s):
    return pe_parser.match(s)


def parsimonious_parse(s):
    return parsimonious_parser.parse(s)


def lark_parse(s):
    return lark_parser.parse(s)


def pyparsing_parse(s):
    return pyparsing_parser.parseString(s, parseAll=True)


def json_parse(s):
    return json.loads(s)


# ============================================================================
# Benchmark
# ============================================================================


def bench(name, func, data, iterations):
    for _ in range(min(10, iterations)):
        func(data)

    t0 = time.perf_counter()
    for _ in range(iterations):
        func(data)
    elapsed = time.perf_counter() - t0
    us_per = elapsed / iterations * 1e6
    ops_per_sec = iterations / elapsed
    return elapsed, us_per, ops_per_sec


# Verify all parsers work before benchmarking
print("\nVerifying parsers on small JSON...")
for name, func in [
    ("zgram", zgram_parse),
    ("pe", pe_parse),
    ("parsimonious", parsimonious_parse),
    ("lark", lark_parse),
    ("pyparsing", pyparsing_parse),
    ("json.loads", json_parse),
]:
    try:
        r = func(small_json)
        print(f"  {name}: OK")
    except Exception as e:
        print(f"  {name}: FAIL ({e})")

print("\n" + "=" * 80)
print(f"{'Benchmark':^80}")
print("=" * 80)

#                              zgram   pe   parsimonious  lark  pyparsing  json
test_configs = [
    ("Small JSON", small_json, 10000, 5000, 1000, 2000, 1000, 10000),
    ("Medium JSON", medium_json, 2000, 500, 50, 100, 50, 2000),
    ("Large JSON", large_json, 500, 50, 5, 10, 5, 500),
]

for label, data, z_it, pe_it, pars_it, lark_it, pyp_it, j_it in test_configs:
    print(f"\n── {label} ({len(data)} bytes) ──")
    print(
        f"  {'Parser':<24} {'Iters':>6} {'Total':>10} {'Per-parse':>12} {'Ops/sec':>12}"
    )
    print(f"  {'-' * 24} {'-' * 6} {'-' * 10} {'-' * 12} {'-' * 12}")

    results = {}
    parsers = [
        ("zgram", zgram_parse, z_it),
        ("pe (C ext)", pe_parse, pe_it),
        ("parsimonious", parsimonious_parse, pars_it),
        ("lark (Earley)", lark_parse, lark_it),
        ("pyparsing", pyparsing_parse, pyp_it),
        ("json.loads (parse+build)", json_parse, j_it),
    ]

    for pname, pfunc, iters in parsers:
        try:
            elapsed, us_per, ops = bench(pname, pfunc, data, iters)
            results[pname] = us_per
            print(
                f"  {pname:<24} {iters:>6} {elapsed:>9.4f}s {us_per:>10.1f}us {ops:>11,.0f}"
            )
        except Exception as e:
            print(f"  {pname:<24} ERROR: {e}")

    # Ratios vs zgram
    print()
    zus = results.get("zgram")
    if zus:
        for pname, us in sorted(results.items(), key=lambda x: x[1]):
            if pname == "zgram":
                continue
            ratio = us / zus
            if ratio >= 1:
                print(f"  -> zgram is {ratio:.0f}x faster than {pname}")
            else:
                print(f"  -> {pname} is {1 / ratio:.1f}x faster than zgram")
