"""Differential tests: random grammars and inputs, zgram vs a Python reference
PEG interpreter (test/peg_reference.py), with and without @memo on every rule.

Checks parse trees (with spans), failure kind, error offset and the rule
named in "expected <rule>" messages.
"""

import random

import pytest
import zgram
from test.peg_reference import Grammar, TooSlow, run

ALPHABET = "abc"


def random_expr(rnd, names, depth):
    choices = ["lit", "cls", "ncls", "ref", "ref", "seq", "alt", "rep", "rep", "pred", "any"]
    if depth <= 0:
        choices = ["lit", "cls", "ncls", "ref"]
    k = rnd.choice(choices)
    if k == "lit":
        return ("lit", "".join(rnd.choice(ALPHABET) for _ in range(rnd.randint(1, 2))))
    if k in ("cls", "ncls"):
        return (k, set(rnd.sample(ALPHABET, rnd.randint(1, 2))))
    if k == "any":
        return ("any",)
    if k == "ref":
        return ("ref", rnd.choice(names))
    if k == "seq":
        return ("seq", [random_expr(rnd, names, depth - 1) for _ in range(rnd.randint(2, 3))])
    if k == "alt":
        return ("alt", [random_expr(rnd, names, depth - 1) for _ in range(rnd.randint(2, 3))])
    if k == "rep":
        return ("rep", rnd.choice("*+?"), random_expr(rnd, names, depth - 1))
    return (rnd.choice(["not", "and"]), random_expr(rnd, names, depth - 1))


def random_grammar(rnd):
    n = rnd.randint(2, 5)
    names = [f"r{i}" for i in range(n)]
    rules = []
    for i, name in enumerate(names):
        silent = i > 0 and rnd.random() < 0.3
        memo = rnd.random() < 0.3
        rules.append((name, random_expr(rnd, names, 3), silent, memo))
    g = Grammar(rules)
    for name in names:
        if rnd.random() < 0.2:
            g.display[name] = f"a {name} thing"
    return g


def zgram_result(parser, text):
    try:
        return ("ok", parser.parse(text).to_tuple(spans=True))
    except zgram.ParseError as e:
        return classify(e.message, e.offset)
    except ValueError as e:
        if "produced no nodes" in str(e):
            return ("nonodes",)
        raise


def classify(message, offset):
    """An error in the reference's terms: trailing input, or what was expected."""
    if message == "unexpected input after match":
        return ("trailing", offset)
    return ("terminal", offset, message)


def error_of(parser):
    """The error left in parser.error, as classify() describes it."""
    e = parser.error
    return classify(e.message(), e.offset())


def compile_or_skip(source):
    try:
        return zgram.compile(source)
    except ValueError:
        # Left recursion, or a rule that can loop without consuming input
        return None


@pytest.mark.parametrize("seed", range(40))
def test_random_grammars_match_reference(seed):
    rnd = random.Random(seed)
    compared = 0
    for _ in range(25):
        g = random_grammar(rnd)
        plain = compile_or_skip(g.text())
        if plain is None:
            continue
        memo = zgram.compile(g.text(memo_all=True))
        for i in range(30):
            if i % 3 == 0:
                # long runs of one character exercise the 16/32-byte SIMD loops
                text = "".join(rnd.choice(ALPHABET) * rnd.randint(1, 40) for _ in range(rnd.randint(1, 3)))
            else:
                text = "".join(rnd.choice(ALPHABET) for _ in range(rnd.randint(0, 10)))
            try:
                expected = run(g, text)
            except (RecursionError, TooSlow):
                continue
            got = zgram_result(plain, text)
            if got == ("nonodes",):
                continue
            assert got == expected, f"grammar:\n{g.text()}input: {text!r}"
            # The validator must agree, and leave the same error behind
            assert plain.matches(text) == (expected[0] == "ok"), f"matches() grammar:\n{g.text()}input: {text!r}"
            if expected[0] != "ok":
                assert error_of(plain) == expected, f"matches() error, grammar:\n{g.text()}input: {text!r}"
            assert zgram_result(memo, text) == expected, f"@memo grammar:\n{g.text(True)}input: {text!r}"
            compared += 1
    assert compared > 0
