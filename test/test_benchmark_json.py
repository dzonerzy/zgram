"""
Benchmark: zgram vs other Python parser generators vs json.loads (C reference).

Parsers tested:
  - zgram:         PEG, compiles grammar to native .so via Zig (this project)
  - pe:            PEG, C-extension accelerated packrat parser
  - parsimonious:  PEG, pure Python
  - lark:          Earley/LALR, pure Python
  - pyparsing:     Combinator-style, pure Python
  - json.loads:    Hand-tuned C (parse + build Python objects — does MORE work)

Run with:  pytest test/test_benchmark_json.py -v -s
"""

import json
import time

import pytest
import zgram

try:
    import pe

    HAS_PE = True
except ImportError:
    HAS_PE = False

try:
    import parsimonious

    HAS_PARSIMONIOUS = True
except ImportError:
    HAS_PARSIMONIOUS = False

try:
    import lark

    HAS_LARK = True
except ImportError:
    HAS_LARK = False

try:
    from pyparsing import Forward, Group, Keyword, Optional, Regex, Suppress

    HAS_PYPARSING = True
except ImportError:
    HAS_PYPARSING = False

from test.conftest import (
    JSON_GRAMMAR,
    make_large_json,
    make_medium_json,
    make_small_json,
)

# ── Grammar definitions for each parser ──

PE_JSON_GRAMMAR = r"""
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

PARSIMONIOUS_JSON_GRAMMAR = r"""
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
    exp       = ~"[eE]" ~"[+-]?" ~"[0-9]+"
    true_lit  = "true"
    false_lit = "false"
    null_lit  = "null"
    ws        = ~"[ \t\n\r]*"
"""

LARK_JSON_GRAMMAR = r"""
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


def _build_pyparsing_parser():
    LBRACE, RBRACE, LBRACK, RBRACK, COLON, COMMA = map(Suppress, "{}[]:,")
    value = Forward()
    string = Regex(r'"(?:[^"\\]|\\.)*"')
    number = Regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")
    true_lit = Keyword("true")
    false_lit = Keyword("false")
    null_lit = Keyword("null")
    array = Group(LBRACK + Optional(value + (COMMA + value)[...]) + RBRACK)
    pair = Group(string + COLON + value)
    obj = Group(LBRACE + Optional(pair + (COMMA + pair)[...]) + RBRACE)
    value <<= obj | array | string | number | true_lit | false_lit | null_lit
    return value


# ── Benchmark helper ──


def bench(func, data, iterations):
    """Run warmup then timed iterations. Returns (total_s, us_per_parse, ops_per_sec)."""
    for _ in range(min(10, iterations)):
        func(data)

    t0 = time.perf_counter()
    for _ in range(iterations):
        func(data)
    elapsed = time.perf_counter() - t0
    us_per = elapsed / iterations * 1e6
    ops_per_sec = iterations / elapsed
    return elapsed, us_per, ops_per_sec


# ── Test data ──

SMALL_JSON = make_small_json()
MEDIUM_JSON = make_medium_json()
LARGE_JSON = make_large_json()

#                              zgram   pe   parsimonious  lark  pyparsing  json
TEST_CONFIGS = [
    ("Small JSON", SMALL_JSON, 10000, 5000, 1000, 2000, 1000, 10000),
    ("Medium JSON", MEDIUM_JSON, 2000, 500, 50, 100, 50, 2000),
    ("Large JSON", LARGE_JSON, 500, 50, 5, 10, 5, 500),
]


class TestBenchmarkJsonParsers:
    """Full comparative benchmark: zgram vs all other parsers on JSON."""

    @pytest.fixture(scope="class", autouse=True)
    def parsers(self):
        """Compile all parsers once."""
        p = {"zgram": zgram.compile(JSON_GRAMMAR)}
        if HAS_PE:
            p["pe"] = pe.compile(PE_JSON_GRAMMAR)
        if HAS_PARSIMONIOUS:
            p["parsimonious"] = parsimonious.Grammar(PARSIMONIOUS_JSON_GRAMMAR)
        if HAS_LARK:
            p["lark"] = lark.Lark(LARK_JSON_GRAMMAR, start="value", parser="earley")
        if HAS_PYPARSING:
            p["pyparsing"] = _build_pyparsing_parser()
        return p

    def _parse_fn(self, parsers, name, data):
        if name == "zgram":
            return parsers["zgram"].parse(data)
        if name == "pe":
            return parsers["pe"].match(data)
        if name == "parsimonious":
            return parsers["parsimonious"].parse(data)
        if name == "lark":
            return parsers["lark"].parse(data)
        if name == "pyparsing":
            return parsers["pyparsing"].parseString(data, parseAll=True)
        if name == "json.loads":
            return json.loads(data)

    def test_all_parsers_verify(self, parsers):
        """Verify all parsers can parse small JSON before benchmarking."""
        for name in parsers:
            self._parse_fn(parsers, name, SMALL_JSON)
        json.loads(SMALL_JSON)

    def test_benchmark_all(self, parsers):
        """Run the full benchmark suite and print results."""
        parser_configs = [
            ("zgram", "zgram", lambda d: parsers["zgram"].parse(d)),
        ]
        if HAS_PE:
            parser_configs.append(
                ("pe (C ext)", "pe", lambda d: parsers["pe"].match(d))
            )
        if HAS_PARSIMONIOUS:
            parser_configs.append(
                (
                    "parsimonious",
                    "parsimonious",
                    lambda d: parsers["parsimonious"].parse(d),
                )
            )
        if HAS_LARK:
            parser_configs.append(
                ("lark (Earley)", "lark", lambda d: parsers["lark"].parse(d))
            )
        if HAS_PYPARSING:
            parser_configs.append(
                (
                    "pyparsing",
                    "pyparsing",
                    lambda d: parsers["pyparsing"].parseString(d, parseAll=True),
                )
            )
        parser_configs.append(
            ("json.loads (parse+build)", "json.loads", lambda d: json.loads(d))
        )

        iter_keys = ["zgram", "pe", "parsimonious", "lark", "pyparsing", "json.loads"]

        print()
        print("=" * 80)
        print(f"{'Benchmark':^80}")
        print("=" * 80)

        for label, data, z_it, pe_it, pars_it, lark_it, pyp_it, j_it in TEST_CONFIGS:
            iters_map = dict(
                zip(iter_keys, [z_it, pe_it, pars_it, lark_it, pyp_it, j_it])
            )

            print(f"\n-- {label} ({len(data)} bytes) --")
            print(
                f"  {'Parser':<24} {'Iters':>6} {'Total':>10} {'Per-parse':>12} {'Ops/sec':>12}"
            )
            print(f"  {'-' * 24} {'-' * 6} {'-' * 10} {'-' * 12} {'-' * 12}")

            results = {}
            for pname, key, pfunc in parser_configs:
                iters = iters_map.get(key, 100)
                try:
                    elapsed, us_per, ops = bench(pfunc, data, iters)
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
