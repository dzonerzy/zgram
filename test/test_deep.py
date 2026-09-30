"""Input nested deeper than the native stack allows is a ParseError, not a crash."""

import threading

import pytest
import zgram

GRAMMAR = r"""
expr = term (ws [+] ws term)*
term = atom (ws [*] ws atom)*
atom = num | list | '(' ws expr ws ')'
list = '[' ws (expr (ws ',' ws expr)*)? ws ']'
num  = [0-9]+
@silent ws = [ ]*
"""

TOO_DEEP = "nested too deeply"


@pytest.fixture(scope="module")
def parser():
    return zgram.compile(GRAMMAR)


def nested(depth, open_="(", close=")", closed=None):
    return open_ * depth + "1" + close * (depth if closed is None else closed)


def test_moderate_nesting_parses(parser):
    tree = parser.parse_tree(nested(500))
    assert len(tree) == 3 * 500 + 4
    assert parser.matches(nested(500))


@pytest.mark.parametrize("depth", [200_000, 2_000_000])
def test_too_deep_is_a_parse_error(parser, depth):
    text = nested(depth)
    for method in (parser.parse, parser.parse_tree, parser.parse_ast):
        with pytest.raises(zgram.ParseError, match=TOO_DEEP):
            method(text)


def test_error_position_and_diagnostic(parser):
    with pytest.raises(zgram.ParseError) as info:
        parser.parse(nested(2_000_000))
    diagnostic = info.value.diagnostic
    assert TOO_DEEP in diagnostic.message
    assert diagnostic.line == 1 and 1 < diagnostic.column < 2_000_000
    assert diagnostic.code == "syntax"


def test_methods_that_do_not_raise(parser):
    text = nested(2_000_000)
    assert parser.match(text) is None
    assert TOO_DEEP in str(parser.error)
    assert parser.matches(text) is False
    assert TOO_DEEP in str(parser.error)


def test_unbalanced_and_too_deep(parser):
    # the missing parenthesis is never reached: the depth is the error
    with pytest.raises(zgram.ParseError, match=TOO_DEEP):
        parser.parse(nested(2_000_000, closed=5))


def test_other_brackets_and_mixed_nesting(parser):
    with pytest.raises(zgram.ParseError, match=TOO_DEEP):
        parser.parse(nested(2_000_000, "[", "]"))
    with pytest.raises(zgram.ParseError, match=TOO_DEEP):
        parser.parse("([" * 1_000_000 + "1" + "])" * 1_000_000)


def test_the_parser_works_afterwards(parser):
    with pytest.raises(zgram.ParseError, match=TOO_DEEP):
        parser.parse(nested(2_000_000))
    assert parser.parse("1 + 2 * (3 + [4, 5])").text() == "1 + 2 * (3 + [4, 5])"
    assert parser.error is None


def test_an_optional_part_does_not_hide_it():
    # `tail?` fails inside and would let the rule match without it
    parser = zgram.compile(
        r"""
        item = '<' tail? '>'?
        tail = item
        """
    )
    assert parser.parse("<<>>").text() == "<<>>"
    text = "<" * 2_000_000
    with pytest.raises(zgram.ParseError, match=TOO_DEEP):
        parser.parse(text)
    assert parser.match(text) is None
    assert parser.matches(text) is False


def test_memo_rules():
    parser = zgram.compile(
        r"""
        expr = atom ('+' atom)*
        @memo atom = num | '(' expr ')'
        num = [0-9]+
        """
    )
    assert parser.parse(nested(200)).text() == nested(200)
    with pytest.raises(zgram.ParseError, match=TOO_DEEP):
        parser.parse(nested(2_000_000))


def test_folded_rules():
    parser = zgram.compile(
        r"""
        @left expr = left:atom (op:plus right:atom)*
        @silent atom = num | '(' expr ')'
        num = [0-9]+
        plus = '+'
        """
    )
    assert parser.parse(nested(200) + "+2").text() == nested(200) + "+2"
    with pytest.raises(zgram.ParseError, match=TOO_DEEP):
        parser.parse(nested(2_000_000))


def test_a_small_stack_gets_less_deep_and_does_not_crash(parser):
    results = {}

    def work():
        results["shallow"] = len(parser.parse_tree(nested(50)))
        try:
            parser.parse(nested(50_000))
            results["deep"] = "parsed"
        except zgram.ParseError as e:
            results["deep"] = str(e)
        results["after"] = parser.parse("1+1").text()

    old = threading.stack_size(256 * 1024)
    try:
        thread = threading.Thread(target=work)
        thread.start()
        thread.join()
    finally:
        threading.stack_size(old)
    assert results["shallow"] == 3 * 50 + 4
    assert TOO_DEEP in results["deep"]
    assert results["after"] == "1+1"


def test_long_input_that_is_not_deep(parser):
    # repetition is a loop, not recursion
    text = " + ".join(["1"] * 200_000)
    assert len(parser.parse_tree(text)) > 400_000


def test_parser_compiled_asynchronously():
    import asyncio

    async def compiled():
        return await asyncio.wait_for(zgram.compile_async(GRAMMAR), 60)

    parser = asyncio.run(compiled())
    with pytest.raises(zgram.ParseError, match=TOO_DEEP):
        parser.parse(nested(2_000_000))
