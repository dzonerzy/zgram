"""Syntax errors name the terminal that was expected, where it was expected."""

import pytest
import zgram

GRAMMAR = r"""
prog = ws (stmt ws)*
@silent stmt = let | call
let  = 'let' ws1 name ws '=' ws expr ws ';'
call = name '(' ws (expr (ws ',' ws expr)*)? ws ')' ws ';'
@silent expr = num | name | '(' ws expr ws ')'
name = [a-z]+
num  = [0-9]+
@silent ws  = ([ \t\n] | '#' [^\n]*)*
@silent ws1 = [ \t\n]+
"""


@pytest.fixture(scope="module", params=[False, True], ids=["plain", "memo"])
def parser(request):
    grammar = GRAMMAR
    if request.param:
        grammar = grammar.replace("\nlet ", "\n@memo let ").replace("\nname ", "\n@memo name ")
    return zgram.compile(grammar)


def error(parser, src):
    with pytest.raises(zgram.ParseError) as e:
        parser.parse(src)
    return e.value.message, e.value.line, e.value.column, e.value.offset


@pytest.mark.parametrize(
    "src, expected",
    [
        ("let a = 1", ("expected ';'", 1, 10, 9)),
        ("let a = 1;\nlet b = (2;", ("expected ')'", 2, 11, 21)),
        ("f(1, 2;", ("expected ',' or ')'", 1, 7, 6)),
        ("f(1 2);", ("expected ',' or ')'", 1, 5, 4)),
        ("let a = 1;\nlet b 2;", ("expected '='", 2, 7, 17)),
        ("f(1,\n  2\n  3);", ("expected ',' or ')'", 3, 3, 11)),
        # Rules that failed right where they started are expected by name
        ("let a = ;", ("expected num, name or '('", 1, 9, 8)),
        ("let = 1;", ("expected name", 1, 5, 4)),
        ("f(1,);", ("expected num, name or '('", 1, 5, 4)),
        ("let a = 1;\nlet", ("expected '('", 2, 4, 14)),
        # Nothing failed beyond the end of the match
        ("let a = 1; ?", ("unexpected input after match", 1, 12, 11)),
    ],
)
def test_messages(parser, src, expected):
    assert error(parser, src) == expected


def test_whitespace_and_comments_are_never_expected(parser):
    # At the furthest position ws also failed on [ \t\n] and '#': not reported
    message, *_ = error(parser, "let a = 1 # no semicolon")
    assert message == "expected ';'"


def test_matches_and_error_property(parser):
    assert not parser.matches("f(1 2);")
    assert (parser.error.message(), parser.error.offset()) == ("expected ',' or ')'", 4)
    assert parser.match("f(1 2);", start="call") is None
    assert parser.error.message() == "expected ',' or ')'"


def test_error_property_for_trailing_input():
    p = zgram.compile("a = 'x'")
    assert not p.matches("xy")
    assert p.error.message() == "unexpected input after match"
    assert str(p.error) == "line 1, col 2: unexpected input after match"


def test_diagnostic(parser):
    src = "let a = 1;\nlet b 2;"
    with pytest.raises(zgram.ParseError) as e:
        parser.parse(src)
    assert e.value.diagnostic.render(src) == "2:7: error: expected '=' [syntax]\n    2 | let b 2;\n      |       ^"


def test_character_classes_when_no_literal_is_expected():
    p = zgram.compile("pair = word '=' [0-9] [0-9_]*\nword = [a-z]+")
    assert error(p, "ab=")[0] == "expected [0-9]"
    assert error(p, "ab")[0] == "expected '='"


def test_several_classes():
    p = zgram.compile("v = 'x' ([0-9] | [a-f] 'h' | .)")
    assert error(p, "x")[0] == "expected [0-9], [a-f] or any character"


def test_a_class_of_punctuation_is_named_by_its_characters():
    # (a separator written `[,;]`: its characters, as literals, with the rest)
    p = zgram.compile("t = '{' w ([,;] w)* '}'\nw = [a-z]+")
    assert error(p, "{a b}")[0] == "expected ',', ';' or '}'"
    # (letters, or more than four characters: a class)
    q = zgram.compile("t = 'x' [,;:.!]")
    assert error(q, "xa")[0] == "expected [,;:.!]"


def test_negated_class_and_escapes():
    p = zgram.compile("s = 'a' [^\\n\\]x] '\\t' 'it\\'s'")
    assert error(p, "a\n")[0] == "expected [^\\n\\]x]"
    assert error(p, "ab")[0] == "expected '\\t'"
    assert error(p, "ab\t")[0] == "expected 'it\\'s'"


def test_failures_inside_predicates_are_not_reported():
    p = zgram.compile("s = 'a' !'b' &'c' 'cd'")
    assert error(p, "ac")[0] == "expected 'cd'"
    # The predicates themselves failing leaves only the rule-level error
    assert error(p, "ab") == ("expected s", 1, 1, 0)


def test_outermost_rule_that_failed_at_its_start_is_named():
    p = zgram.compile("stmt = 'print ' expr ';'\nexpr = term ('+' term)*\nterm = num | '(' expr ')'\nnum = [0-9]+")
    # expr, term and num all failed at offset 6: only expr is reported
    assert error(p, "print ;") == ("expected expr", 1, 7, 6)
    # here expr got further, so the detail inside it is kept
    assert error(p, "print 1+;") == ("expected term", 1, 9, 8)
    assert error(p, "print (1;") == ("expected '+' or ')'", 1, 9, 8)


def test_rule_failing_on_a_predicate_is_named():
    p = zgram.compile("stmt = 'let ' name\nname = !'let' [a-z]+")
    assert error(p, "let let") == ("expected name", 1, 5, 4)


def test_duplicates_are_listed_once():
    p = zgram.compile("s = 'a' ('b' 'c' | 'b' 'd' | 'b')")
    assert error(p, "a")[0] == "expected 'b'"


def test_start_rule(parser):
    with pytest.raises(zgram.ParseError) as e:
        parser.parse("f(1", start="call")
    assert (e.value.message, e.value.offset) == ("expected ',' or ')'", 3)


def test_long_literal_is_truncated():
    p = zgram.compile("s = 'a' '" + "x" * 100 + "'")
    message = error(p, "a")[0]
    assert message.startswith("expected 'xxxx") and message.endswith("'") and len(message) < 70


def test_deep_nesting_falls_back_to_the_rule_error():
    p = zgram.compile("v = '(' v ')' | num\nnum = [0-9]+")
    depth = 3000
    message, _, _, offset = error(p, "(" * depth + "1" + ")" * (depth - 1))
    assert message.startswith("expected ")
    assert offset <= 2 * depth


def test_large_input_error_at_the_end(parser):
    src = "let a = 1;\n" * 20000 + "let b = 2"
    assert error(parser, src) == ("expected ';'", 20001, 10, len(src))


def test_display_names():
    p = zgram.compile(
        "stmt = 'print ' expr ';'\n"
        "expr \"expression\" = term (addop term)*\n"
        "term 'expression' = num | '(' expr ')'\n"
        "addop \"operator\" = [+\\-]\n"
        "num = [0-9]+"
    )
    assert error(p, "print ;")[0] == "expected expression"
    assert error(p, "print 1+;")[0] == "expected expression"
    assert error(p, "print 1")[0] == "expected operator or ';'"
    assert error(p, "print (1;")[0] == "expected operator or ')'"
    # Display names are for messages only
    assert p.rules() == ["stmt", "expr", "term", "addop", "num"]
    assert p.parse("print 1+2;")[0].rule() == "expr"


def test_display_name_of_the_start_rule():
    p = zgram.compile("doc \"a document\" = 'x'+")
    assert error(p, "y") == ("expected a document", 1, 1, 0)


def test_display_name_too_long():
    with pytest.raises(ValueError):
        zgram.compile("r \"" + "x" * 65 + "\" = 'a'")
