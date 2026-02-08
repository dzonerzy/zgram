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
from typing import Any

import pytest
import zgram

try:
    import pe as _pe
except ImportError:
    _pe = None

try:
    import parsimonious as _parsimonious
except ImportError:
    _parsimonious = None

try:
    import lark as _lark
except ImportError:
    _lark = None

try:
    import pyparsing as _pyparsing
except ImportError:
    _pyparsing = None

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
    assert _pyparsing is not None
    LBRACE, RBRACE, LBRACK, RBRACK, COLON, COMMA = map(_pyparsing.Suppress, "{}[]:,")
    value = _pyparsing.Forward()
    string = _pyparsing.Regex(r'"(?:[^"\\]|\\.)*"')
    number = _pyparsing.Regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?")
    true_lit = _pyparsing.Keyword("true")
    false_lit = _pyparsing.Keyword("false")
    null_lit = _pyparsing.Keyword("null")
    array = _pyparsing.Group(
        LBRACK + _pyparsing.Optional(value + (COMMA + value)[...]) + RBRACK
    )
    pair = _pyparsing.Group(string + COLON + value)
    obj = _pyparsing.Group(
        LBRACE + _pyparsing.Optional(pair + (COMMA + pair)[...]) + RBRACE
    )
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

#                              zgram  pe  parsimonious lark pyparsing json
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
        p: dict[str, Any] = {
            "zgram": zgram.compile(JSON_GRAMMAR),
        }
        if _pe is not None:
            p["pe"] = _pe.compile(PE_JSON_GRAMMAR)
        if _parsimonious is not None:
            p["parsimonious"] = _parsimonious.Grammar(PARSIMONIOUS_JSON_GRAMMAR)
        if _lark is not None:
            p["lark"] = _lark.Lark(LARK_JSON_GRAMMAR, start="value", parser="earley")
        if _pyparsing is not None:
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
        if _pe is not None:
            parser_configs.append(
                ("pe (C ext)", "pe", lambda d: parsers["pe"].match(d))
            )
        if _parsimonious is not None:
            parser_configs.append(
                (
                    "parsimonious",
                    "parsimonious",
                    lambda d: parsers["parsimonious"].parse(d),
                )
            )
        if _lark is not None:
            parser_configs.append(
                ("lark (Earley)", "lark", lambda d: parsers["lark"].parse(d))
            )
        if _pyparsing is not None:
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

        iter_keys = [
            "zgram",
            "pe",
            "parsimonious",
            "lark",
            "pyparsing",
            "json.loads",
        ]

        print()
        print("=" * 80)
        print(f"{'Benchmark':^80}")
        print("=" * 80)

        for (
            label,
            data,
            z_it,
            pe_it,
            pars_it,
            lark_it,
            pyp_it,
            j_it,
        ) in TEST_CONFIGS:
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
