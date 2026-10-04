"""Labels (label:rule): field names stored on parse tree nodes."""

import pytest
import zgram

GRAMMAR = r"""
if_stmt = 'if' ws cond:expr ws then:block (ws 'else' ws else_:block)?
block   = '{' ws (body:stmt ws)* '}'
@silent stmt = if_stmt | call
call    = name:ident '(' (args:arglist)? ')' ';'
@silent arglist = expr (',' ws expr)*
@silent expr = num | ident
num     = [0-9]+
ident   = [a-z]+
@silent ws = [ \t\n]*
"""


@pytest.fixture(scope="module")
def parser():
    return zgram.compile(GRAMMAR)


def test_fields_in_order_of_first_use(parser):
    assert parser.fields() == ["cond", "then", "else_", "body", "name", "args"]


def test_labels_per_rule(parser):
    # (label, many): many inside * / +, or on a silent rule that repeats
    # (args:arglist, each argument gets the label); else one child or none
    by_rule = dict(zip(parser.rules(), parser.labels()))
    assert by_rule["if_stmt"] == [("cond", False), ("then", False), ("else_", False)]
    assert by_rule["block"] == [("body", True)]
    assert by_rule["call"] == [("name", False), ("args", True)]
    assert by_rule["num"] == [] and len(parser.labels()) == len(parser.rules())
    # a label used twice is many too
    p = zgram.compile("pair = a:x ',' a:x\nx = [a-z]")
    assert p.labels()[0] == [("a", True)]


def test_no_labels():
    p = zgram.compile("root = item+\nitem = [a-z]")
    assert p.fields() == []
    root = p.parse("ab")
    assert root.field() is None
    assert [c.field() for c in root] == [None, None]
    assert root.get("x") is None
    assert root.get_all("x") == []


def test_field_of_each_child(parser):
    root = parser.parse("if x { f(1); } else { g(); }")
    assert root.field() is None
    assert [(c.field(), c.rule()) for c in root] == [("cond", "ident"), ("then", "block"), ("else_", "block")]


def test_get(parser):
    root = parser.parse("if x { f(1); } else { g(); }")
    assert root.get("cond").text() == "x"
    assert root.get("then").text() == "{ f(1); }"
    assert root.get("else_").text() == "{ g(); }"
    assert root.get("then") == root[1]


def test_get_absent_optional(parser):
    root = parser.parse("if x { f(1); }")
    assert root.get("else_") is None
    assert root.get("body") is None  # a label of another rule
    assert root.get("unknown") is None


def test_get_all_repeated_label(parser):
    root = parser.parse("if x { f(1); g(); h(2, y); }")
    body = root.get("then").get_all("body")
    assert [c.text() for c in body] == ["f(1);", "g();", "h(2, y);"]
    assert all(c.field() == "body" for c in body)


def test_label_on_silent_rule_tags_all_its_nodes(parser):
    root = parser.parse("if x { h(2, y, 3); }")
    call = root.get("then").get("body")
    assert [(a.field(), a.rule(), a.text()) for a in call.get_all("args")] == [
        ("args", "num", "2"),
        ("args", "ident", "y"),
        ("args", "num", "3"),
    ]
    assert call.get("name").text() == "h"


def test_get_only_searches_direct_children(parser):
    root = parser.parse("if x { f(1); }")
    assert root.get("name") is None
    assert root.get_all("args") == []


def test_field_string_is_interned(parser):
    root = parser.parse("if x { f(1); g(); }")
    a, b = root.get("then").get_all("body")
    assert a.field() is b.field()


def test_same_rule_different_labels():
    p = zgram.compile("pair = left:num ',' right:num\nnum = [0-9]+")
    root = p.parse("1,2")
    assert root.get("left").text() == "1"
    assert root.get("right").text() == "2"


def test_backtracking_discards_labelled_nodes():
    p = zgram.compile("root = a:item b:item '!' | c:item d:item\nitem = [a-z]")
    root = p.parse("xy")
    assert [c.field() for c in root] == ["c", "d"]


def test_label_with_memo():
    p = zgram.compile("root = a:item '!' | b:item '?' | c:item\n@memo item = [a-z]+")
    assert p.parse("xy!")[0].field() == "a"
    assert p.parse("xy?")[0].field() == "b"
    assert p.parse("xy")[0].field() == "c"


def test_memoized_parent_keeps_child_labels():
    p = zgram.compile("root = outer '!' | outer '?'\n@memo outer = k:item '=' v:item\nitem = [a-z]+")
    outer = p.parse("a=b?")[0]
    assert [c.field() for c in outer] == ["k", "v"]


def test_label_in_repetition_of_silent_alternative():
    # (a | cls | b)* loops are vectorized by inlining silent rules; the label must survive
    p = zgram.compile("root = (part:chunk)*\n@silent chunk = esc | [a-z]\nesc = '%' [0-9]")
    root = p.parse("ab%1c%2")
    assert [(c.field(), c.text()) for c in root] == [("part", "%1"), ("part", "%2")]


def test_label_inside_predicate_leaves_no_nodes():
    p = zgram.compile("root = &(x:item) y:item\nitem = [a-z]+")
    assert [(c.field(), c.text()) for c in p.parse("abc")] == [("y", "abc")]


def test_outer_label_wins_over_inner():
    p = zgram.compile("root = outer:wrap\n@silent wrap = inner:item\nitem = [a-z]+")
    assert p.parse("abc")[0].field() == "outer"


def test_label_with_many_children():
    p = zgram.compile("root = '[' (el:num (',' el:num)*)? ']'\nnum = [0-9]+")
    root = p.parse("[" + ",".join(["1"] * 5000) + "]")
    assert len(root.get_all("el")) == 5000
    assert root.rule() == "root"


def test_matches_ignores_labels(parser):
    assert parser.matches("if x { f(1); }")
    assert not parser.matches("if x { f(1) }")


def test_to_tuple_unchanged_by_labels():
    p = zgram.compile("pair = left:num ',' right:num\nnum = [0-9]+")
    assert p.parse("1,2").to_tuple() == ("pair", "1,2", (("num", "1", ()), ("num", "2", ())))


@pytest.mark.parametrize(
    "grammar",
    [
        "root = x:'lit'",
        "root = x:[a-z]",
        "root = x:(item)\nitem = 'a'",
        "root = x: item\nitem = 'a'",
    ],
)
def test_label_needs_a_rule_reference(grammar):
    with pytest.raises(ValueError, match="label"):
        zgram.compile(grammar)


def test_label_on_undefined_rule():
    with pytest.raises(ValueError):
        zgram.compile("root = x:missing")


def test_too_many_labels():
    ok = "root = " + " ".join(f"f{i}:item" for i in range(255)) + "\nitem = 'a'"
    assert len(zgram.compile(ok).fields()) == 255
    too_many = "root = " + " ".join(f"f{i}:item" for i in range(256)) + "\nitem = 'a'"
    with pytest.raises(ValueError, match="labels"):
        zgram.compile(too_many)
