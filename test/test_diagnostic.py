"""zgram.Diagnostic and ParseError.diagnostic."""

import pytest
import zgram
from zgram import Diagnostic

SRC = "fn f() {\n    while x {\n    }\n\tbreak;\n}\n"
BREAK = SRC.index("break")


def test_fields():
    d = Diagnostic("warning", "unused", "unused variable 'x'", (3, 4), 1, 4)
    assert (d.severity, d.code, d.message, d.span, d.line, d.column, d.notes) == (
        "warning",
        "unused",
        "unused variable 'x'",
        (3, 4),
        1,
        4,
        [],
    )


def test_defaults():
    d = Diagnostic("error", "syntax", "bad", (0, 0))
    assert (d.line, d.column, d.notes) == (0, 0, [])


def test_keyword_arguments():
    d = Diagnostic("error", "c", "m", (1, 2), column=4, notes=[Diagnostic("note", "", "n", (0, 0))])
    assert (d.line, d.column, len(d.notes)) == (0, 4, 1)
    assert Diagnostic(severity="warning", code="c", message="m", span=(0, 1), line=2) == Diagnostic("warning", "c", "m", (0, 1), 2)
    with pytest.raises(TypeError):
        Diagnostic("error", "c", "m", (0, 0), bogus=1)
    with pytest.raises(TypeError):
        Diagnostic("error", "c", "m", (0, 0), span=(1, 2))


def test_notes():
    note = Diagnostic("note", "", "previous definition is here", (1, 2))
    d = Diagnostic("error", "redefined", "redefined", (5, 6), 0, 0, (note,))
    assert d.notes == [note]
    assert d.notes[0].message == "previous definition is here"


def test_equality():
    make = lambda msg="m": Diagnostic("error", "c", msg, (1, 2), 3, 4, [Diagnostic("note", "", "n", (0, 0))])
    assert make() == make()
    assert make() != make("other")
    assert Diagnostic("error", "c", "m", (1, 2)) != Diagnostic("error", "c", "m", (1, 3))


def test_repr():
    d = Diagnostic("error", "syntax", "expected num", (19, 19), 2, 9)
    assert repr(d) == "Diagnostic('error', 'syntax', 'expected num', span=(19, 19), line=2, column=9)"
    # the strings are Python reprs: quotes inside them are escaped
    d = Diagnostic("error", "syntax", "expected '='", (6, 6), 1, 7)
    assert repr(d) == """Diagnostic('error', 'syntax', "expected '='", span=(6, 6), line=1, column=7)"""


@pytest.mark.parametrize(
    "args, exc",
    [
        (("fatal", "c", "m", (0, 0)), ValueError),
        (("error", "c", "m", (3, 1)), ValueError),
        (("error", "c", "m", (-1, 1)), ValueError),
        ((1, "c", "m", (0, 0)), TypeError),
        (("error", "c", "m", 5), TypeError),
        (("error", "c", "m", (0, 0), 0, 0, [1]), TypeError),
        (("error", "c", "m"), TypeError),
    ],
)
def test_invalid(args, exc):
    with pytest.raises(exc):
        Diagnostic(*args)


class TestRender:
    def test_with_filename_and_code(self):
        d = Diagnostic("error", "break-outside-loop", "'break' outside loop", (BREAK, BREAK + 5))
        assert d.render(SRC, "program.z") == (
            "program.z:4:2: error: 'break' outside loop [break-outside-loop]\n"
            "    4 | \tbreak;\n"
            "      | \t^^^^^"
        )

    def test_without_filename_or_code(self):
        d = Diagnostic("warning", "", "odd", (3, 4))
        assert d.render(SRC) == "1:4: warning: odd\n    1 | fn f() {\n      | " + "   ^"

    def test_given_line_and_column_are_used(self):
        d = Diagnostic("error", "c", "m", (3, 4), 7, 9)
        assert d.render(SRC).startswith("7:9: error: m [c]\n    7 | fn f() {")

    def test_empty_span_gets_one_caret(self):
        assert Diagnostic("error", "", "m", (3, 3)).render("abcdef").endswith("| abcdef\n      |    ^")

    def test_span_is_clipped_to_its_first_line(self):
        assert Diagnostic("error", "", "m", (2, 9)).render("abcd\nefgh\n").endswith("| abcd\n      |   ^^")

    def test_span_beyond_the_source(self):
        assert Diagnostic("error", "", "m", (50, 60)).render("ab").endswith("| ab\n      |   ^")

    def test_non_ascii_columns(self):
        src = "é = ü;"
        start = len("é = ".encode())
        out = Diagnostic("error", "", "m", (start, start + len("ü".encode()))).render(src)
        assert out == "1:6: error: m\n    1 | é = ü;\n      |     ^"

    def test_bytes_source(self):
        assert Diagnostic("error", "", "m", (1, 2)).render(b"abc").endswith("| abc\n      |  ^")

    def test_crlf(self):
        assert Diagnostic("error", "", "m", (5, 6)).render("ab\r\ncd\r\n") == "2:2: error: m\n    2 | cd\n      |  ^"

    def test_notes_follow(self):
        note = Diagnostic("note", "", "the loop ends here", (SRC.index("}"), SRC.index("}") + 1))
        d = Diagnostic("error", "c", "'break' outside loop", (BREAK, BREAK + 5), 0, 0, [note])
        assert d.render(SRC, "p.z").splitlines() == [
            "p.z:4:2: error: 'break' outside loop [c]",
            "    4 | \tbreak;",
            "      | \t^^^^^",
            "p.z:3:5: note: the loop ends here",
            "    3 |     }",
            "      |     ^",
        ]

    def test_large_line_numbers_widen_the_gutter(self):
        src = "\n" * 123455 + "oops"
        out = Diagnostic("error", "", "m", (123455, 123459)).render(src)
        assert out == "123456:1: error: m\n123456 | oops\n       | ^^^^"

    def test_source_type(self):
        with pytest.raises(TypeError):
            Diagnostic("error", "", "m", (0, 0)).render(5)


class TestParseErrorDiagnostic:
    GRAMMAR = "prog = (stmt ws)*\nstmt = 'let ' name ' = ' num ';'\nname = [a-z]+\nnum = [0-9]+\n@silent ws = [ \n]*"

    def test_diagnostic(self):
        p = zgram.compile(self.GRAMMAR)
        src = "let a = 1;\nlet b = ;"
        with pytest.raises(zgram.ParseError) as e:
            p.parse(src)
        d = e.value.diagnostic
        assert d == Diagnostic("error", "syntax", "expected num", (19, 19), 2, 9)
        assert d.render(src, "x.tiny") == "x.tiny:2:9: error: expected num [syntax]\n    2 | let b = ;\n      |         ^"

    def test_matches_the_exception_attributes(self):
        p = zgram.compile(self.GRAMMAR)
        with pytest.raises(zgram.ParseError) as e:
            p.parse("let a = 1; junk")
        err, d = e.value, e.value.diagnostic
        assert (d.message, d.line, d.column, d.span) == (err.message, err.line, err.column, (err.offset, err.offset))

    @pytest.mark.parametrize("method", ["parse", "parse_ast", "parse_tree"])
    def test_every_parse_method(self, method):
        p = zgram.compile(self.GRAMMAR)
        with pytest.raises(zgram.ParseError) as e:
            getattr(p, method)("let a = ;")
        assert e.value.diagnostic.code == "syntax"
