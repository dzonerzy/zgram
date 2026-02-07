"""Tests for grammar correctness across different PEG patterns."""

import pytest
import zgram


def assert_parses(parser, input_str):
    """Assert that input parses successfully and matches the full input."""
    result = parser.parse(input_str)
    assert result is not None, f"expected match on {input_str!r}"
    assert result.text() == input_str, (
        f"expected full match: {input_str!r}, got {result.text()!r}"
    )


def assert_fails(parser, input_str):
    """Assert that input fails to parse."""
    try:
        result = parser.parse(input_str)
    except Exception:
        return  # parse error raised, as expected
    assert result is None, f"expected failure on {input_str!r}, got match"


# ── JSON ──


class TestJsonGrammar:
    @pytest.fixture(scope="class")
    def parser(self, json_parser):
        return json_parser

    @pytest.mark.parametrize(
        "input_str",
        [
            "true",
            "false",
            "null",
            "42",
            "-3.14",
            "1e10",
            "0",
            '"hello"',
            '"with \\"escape\\""',
            '"\\n\\t"',
            "[]",
            "[1,2,3]",
            '[1, "two", true, null]',
            "{}",
            '{"a":1}',
            '{"name": "John", "age": 30, "active": true}',
            '{"nested": {"a": [1, 2, {"b": 3}]}}',
        ],
    )
    def test_valid_json(self, parser, input_str):
        assert_parses(parser, input_str)


# ── Arithmetic ──


class TestArithmeticGrammar:
    @pytest.fixture(scope="class")
    def parser(self):
        return zgram.compile(
            "expr    = term (('+' | '-') term)*\n"
            "term    = factor (('*' | '/') factor)*\n"
            "factor  = number | '(' expr ')'\n"
            "number  = [0-9]+\n"
        )

    @pytest.mark.parametrize(
        "input_str",
        [
            "1",
            "42",
            "1+2",
            "1+2*3",
            "(1+2)*3",
            "((1))",
            "100/5-3+2*8",
        ],
    )
    def test_valid_expressions(self, parser, input_str):
        assert_parses(parser, input_str)


# ── Identifiers / Assignments ──


class TestIdentifierGrammar:
    @pytest.fixture(scope="class")
    def parser(self):
        return zgram.compile(
            "program = stmt*\n"
            "stmt    = ident '=' expr ';'\n"
            "expr    = ident | number\n"
            "ident   = [a-zA-Z_] [a-zA-Z0-9_]*\n"
            "number  = [0-9]+\n"
        )

    @pytest.mark.parametrize(
        "input_str",
        [
            "x=1;",
            "foo=bar;",
            "_a=123;",
            "x=1;y=2;",
        ],
    )
    def test_valid_assignments(self, parser, input_str):
        assert_parses(parser, input_str)


# ── Recursive nesting ──


class TestNestedParensGrammar:
    @pytest.fixture(scope="class")
    def parser(self):
        return zgram.compile("expr = '(' expr ')' | 'x'\n")

    @pytest.mark.parametrize(
        "input_str",
        [
            "x",
            "(x)",
            "((x))",
            "(((x)))",
            "((((((((x))))))))",
        ],
    )
    def test_nested_parens(self, parser, input_str):
        assert_parses(parser, input_str)


# ── String escapes ──


class TestStringEscapesGrammar:
    @pytest.fixture(scope="class")
    def parser(self):
        return zgram.compile(
            "string  = '\"' content '\"'\n"
            "content = (escape | plain)*\n"
            "escape  = '\\\\' [\"\\\\nrt]\n"
            'plain   = [^"\\\\]\n'
        )

    @pytest.mark.parametrize(
        "input_str",
        [
            '""',
            '"hello"',
            '"with space"',
            '"line\\n"',
            '"tab\\t"',
            '"quote\\""',
            '"back\\\\"',
            '"mixed\\n\\t\\""',
            '"' + "a" * 43 + '"',  # long plain — tests SIMD scan
        ],
    )
    def test_valid_strings(self, parser, input_str):
        assert_parses(parser, input_str)


# ── CSV ──


class TestCsvGrammar:
    @pytest.fixture(scope="class")
    def parser(self):
        return zgram.compile(
            "csv     = row ('\\n' row)*\n"
            "row     = field (',' field)*\n"
            "field   = quoted | plain\n"
            "quoted  = '\"' qchar* '\"'\n"
            'qchar   = \'""\' | [^"]\n'
            'plain   = [^,"\\n]*\n'
        )

    @pytest.mark.parametrize(
        "input_str",
        [
            "a,b,c",
            "1,2,3",
            '"hello","world"',
            'a,"b,c",d',
            "a,b\n1,2",
        ],
    )
    def test_valid_csv(self, parser, input_str):
        assert_parses(parser, input_str)


# ── Optional / empty ──


class TestOptionalGrammar:
    @pytest.fixture(scope="class")
    def parser(self):
        return zgram.compile("start = 'a' opt 'b'\nopt   = 'x'?\n")

    @pytest.mark.parametrize(
        "input_str",
        [
            "ab",  # opt matches empty
            "axb",  # opt matches 'x'
        ],
    )
    def test_optional_match(self, parser, input_str):
        assert_parses(parser, input_str)


# ── Long input stress test ──


class TestLongInput:
    def test_100k_chars(self):
        parser = zgram.compile("start = [a-z]+\n")
        long_input = "a" * 100_000
        result = parser.parse(long_input)
        assert result is not None
        assert len(result.text()) == 100_000


# ── Node tree structure ──


class TestNodeTreeStructure:
    def test_tree_shape(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root.rule() == "list"
        assert root.child_count() == 3
        for i, expected in enumerate(["abc", "def", "ghi"]):
            child = root.child(i)
            assert child is not None
            assert child.rule() == "item"
            assert child.text() == expected


# ── Predicates ──


class TestPredicateGrammar:
    @pytest.fixture(scope="class")
    def parser(self):
        return zgram.compile(
            "start   = !digit letter+\ndigit   = [0-9]\nletter  = [a-zA-Z]\n"
        )

    @pytest.mark.parametrize("input_str", ["hello", "ABC"])
    def test_valid_with_predicate(self, parser, input_str):
        assert_parses(parser, input_str)

    def test_digit_prefix_rejected(self, parser):
        assert_fails(parser, "123")
