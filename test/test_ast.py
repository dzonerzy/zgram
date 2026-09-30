"""parse_ast(): converting the tree to values with `-> name` actions."""

import json
from dataclasses import dataclass
from types import SimpleNamespace

import pytest
import zgram

JSON = r"""
value  = ws (object | array | string | number | true | false | null) ws   -> first
object = '{' ws (pair (',' pair)*)? ws '}'   -> dict
pair   = ws string ws ':' value              -> tuple
array  = '[' ws (value (',' value)*)? ws ']' -> list
string = '"' ('\\' . | [^"\\])* '"'          -> unquote
number = '-'? [0-9]+ ('.' [0-9]+)?           -> float
true   = 'true'   -> True
false  = 'false'  -> False
null   = 'null'   -> None
@silent ws = [ \t\n\r]*
"""


@pytest.fixture(scope="module")
def json_parser():
    return zgram.compile(JSON)


class TestBuiltins:
    @pytest.mark.parametrize(
        "doc",
        [
            "1",
            '"x"',
            "true",
            "false",
            "null",
            "[]",
            "{}",
            "[1, 2.5, -3]",
            '{"a": [1, true, null, {"b": "x"}], "c": false, "d": [], "e": {}}',
            '{"esc\\"aped": "tab\\t \\u00e9 \\ud83d\\ude00"}',
            ' { "k" : [ [ ] , [ [ 1 ] ] ] } ',
        ],
    )
    def test_json(self, json_parser, doc):
        assert json_parser.parse_ast(doc) == json.loads(doc)

    def test_json_large(self, json_parser):
        data = [{"id": i, "name": f"user{i}", "tags": ["a", "b"], "score": i * 1.5, "ok": i % 2 == 0} for i in range(500)]
        assert json_parser.parse_ast(json.dumps(data)) == data

    def test_str_int_float(self):
        p = zgram.compile("row = a:word ',' b:int ',' c:real -> list\nword = [a-z]+ -> str\nint = '-'? [0-9]+ -> int\nreal = [0-9]+ '.' [0-9]+ -> float")
        assert p.parse_ast("ab,-12,3.5") == ["ab", -12, 3.5]
        assert [type(v) for v in p.parse_ast("ab,7,0.5")] == [str, int, float]

    def test_big_int(self):
        p = zgram.compile("n = [0-9]+ -> int")
        assert p.parse_ast("123456789012345678901234567890") == 123456789012345678901234567890

    def test_int_conversion_error(self):
        p = zgram.compile("n = [a-z]+ -> int")
        with pytest.raises(ValueError):
            p.parse_ast("abc")

    def test_tuple(self):
        p = zgram.compile("pair = num ',' num -> tuple\nnum = [0-9]+ -> int")
        assert p.parse_ast("1,2") == (1, 2)

    def test_dict_needs_pairs(self):
        p = zgram.compile("d = num (',' num)* -> dict\nnum = [0-9]+ -> int")
        with pytest.raises(TypeError, match="rule 'd' -> dict"):
            p.parse_ast("1,2")

    def test_first(self):
        p = zgram.compile("wrap = '(' num ',' num ')' -> first\nnum = [0-9]+ -> int")
        assert p.parse_ast("(1,2)") == 1

    def test_first_without_children(self):
        p = zgram.compile("wrap = '(' num? ')' -> first\nnum = [0-9]+ -> int")
        assert p.parse_ast("()") is None

    def test_drop(self):
        p = zgram.compile("items = (num | sep)* -> list\nnum = [0-9]+ -> int\nsep = ',' -> drop")
        assert p.parse_ast("1,2,3") == [1, 2, 3]

    def test_dropped_root_is_none(self):
        assert zgram.compile("root = 'x' -> drop").parse_ast("x") is None

    def test_dropped_label_is_none(self):
        p = zgram.compile("pair = a:num ',' b:gone -> Pair\nnum = [0-9]+ -> int\ngone = [0-9]+ -> drop", ast={"Pair": lambda a, b: (a, b)})
        assert p.parse_ast("1,2") == (1, None)


class TestUnquote:
    GRAMMAR = "s = '\"' ('\\\\' . | [^\"\\\\])* '\"' -> unquote"

    @pytest.fixture
    def parser(self):
        return zgram.compile(self.GRAMMAR)

    @pytest.mark.parametrize(
        "value",
        [
            "",
            "plain",
            "héllo wörld",
            "tab\there",
            'quote " and \\ backslash',
            "line\nbreak\r\n",
            "\b\f/",
            "\u00e9\u4e2d",
            "\U0001f600 emoji",
            "nul \x00 byte",
            "x" * 2000 + "\n" + "y" * 2000,
        ],
    )
    def test_json_round_trip(self, parser, value):
        for ensure_ascii in (True, False):
            assert parser.parse_ast(json.dumps(value, ensure_ascii=ensure_ascii)) == value

    def test_escapes(self, parser):
        assert parser.parse_ast(r'"a\x41\0\/\q\'"') == "aA\x00/q'"

    def test_lone_surrogate(self, parser):
        assert parser.parse_ast(r'"\ud83d"') == "\ud83d"
        assert parser.parse_ast(r'"\ud83dx\ude00"') == "\ud83dx\ude00"

    def test_malformed_hex_escape_keeps_the_letter(self, parser):
        assert parser.parse_ast(r'"\uZZ"') == "uZZ"
        assert parser.parse_ast(r'"\x4"') == "x4"

    def test_single_quotes(self):
        p = zgram.compile("s = '\\'' [^']* '\\'' -> unquote")
        assert p.parse_ast("'it'") == "it"


class TestNoAction:
    def test_leaf_is_text(self):
        assert zgram.compile("word = [a-z]+").parse_ast("abc") == "abc"

    def test_only_child_passes_through(self):
        p = zgram.compile("wrap = '(' num ')'\nnum = [0-9]+ -> int")
        assert p.parse_ast("(5)") == 5

    def test_several_children_make_a_list(self):
        p = zgram.compile("nums = num (',' num)*\nnum = [0-9]+ -> int")
        assert p.parse_ast("1,2,3") == [1, 2, 3]
        assert p.parse_ast("1") == 1


@dataclass
class Number:
    text: str


@dataclass
class Name:
    text: str


@dataclass
class BinOp:
    left: object
    op: str
    right: object


@dataclass
class Neg:
    operand: object


@dataclass
class Call:
    func: object
    args: list


@dataclass
class Let:
    name: Name
    value: object


@dataclass
class If:
    cond: object
    then: list
    else_: object


@dataclass
class Program:
    body: list


LANG = r"""
program = ws (body:stmt ws)*                     -> Program
@silent stmt = if_stmt | let | expr_stmt
if_stmt = 'if' ws cond:expr ws then:block (ws 'else' ws else_:block)?  -> If
block   = '{' ws (stmt ws)* '}'                  -> list
let     = 'let' ws name:ident ws '=' ws value:expr ws ';'   -> Let
@silent expr_stmt = expr ws ';'
@silent expr = sum
@left sum     = left:product (ws op:addop ws right:product)*  -> BinOp
@left product = left:unary (ws op:mulop ws right:unary)*      -> BinOp
@silent unary = neg | post
neg     = '-' ws operand:unary                   -> Neg
@postfix post = func:primary (call)*
call    = '(' ws (args:expr (ws ',' ws args:expr)*)? ws ')'   -> Call
@silent primary = number | ident | '(' ws expr ws ')'
number  = [0-9]+                                 -> Number
ident   = !kw [a-z_]+                            -> Name
@silent kw = ('if' | 'else' | 'let') ![a-z_]
addop   = [+\-]                                  -> str
mulop   = [*/]                                   -> str
@silent ws = [ \t\n]*
"""


@pytest.fixture(scope="module")
def lang():
    import sys

    return zgram.compile(LANG, ast=sys.modules[__name__])


class TestClasses:
    def test_labels_become_keyword_arguments(self, lang):
        assert lang.parse_ast("let x = 1;") == Program(body=[Let(name=Name("x"), value=Number("1"))])

    def test_left_fold(self, lang):
        (stmt,) = lang.parse_ast("1 - 2 - 3;").body
        assert stmt == BinOp(BinOp(Number("1"), "-", Number("2")), "-", Number("3"))

    def test_precedence_and_unary(self, lang):
        (stmt,) = lang.parse_ast("1 + 2 * -x;").body
        assert stmt == BinOp(Number("1"), "+", BinOp(Number("2"), "*", Neg(Name("x"))))

    def test_postfix_chain(self, lang):
        (stmt,) = lang.parse_ast("f(1, y)(z)();").body
        assert stmt == Call(Call(Call(Name("f"), [Number("1"), Name("y")]), [Name("z")]), [])

    def test_absent_optional_label_is_none(self, lang):
        (stmt,) = lang.parse_ast("if x { }").body
        assert stmt == If(cond=Name("x"), then=[], else_=None)

    def test_repeated_label_is_a_list_even_when_empty(self, lang):
        assert lang.parse_ast("") == Program(body=[])
        assert lang.parse_ast("x;") == Program(body=[Name("x")])

    def test_nested(self, lang):
        tree = lang.parse_ast("if a { let b = c(1); } else { if d { e; } }")
        assert tree == Program(
            [If(Name("a"), [Let(Name("b"), Call(Name("c"), [Number("1")]))], [If(Name("d"), [Name("e")], None)])]
        )

    def test_token_rule_gets_its_text(self, lang):
        assert lang.parse_ast("abc", start="ident") == Name("abc")

    def test_start_rule(self, lang):
        assert lang.parse_ast("1+2", start="sum") == BinOp(Number("1"), "+", Number("2"))

    def test_unlabelled_children_are_positional(self):
        p = zgram.compile("pair = num ',' num -> Pair\nnum = [0-9]+ -> int", ast={"Pair": lambda *a: ("pair", a)})
        assert p.parse_ast("1,2") == ("pair", (1, 2))

    def test_unlabelled_rule_without_matches_gets_no_arguments(self):
        p = zgram.compile("items = '[' (num (',' num)*)? ']' -> Items\nnum = [0-9]+ -> int", ast={"Items": lambda *a: a})
        assert p.parse_ast("[]") == ()
        assert p.parse_ast("[1,2]") == (1, 2)

    def test_empty_parentheses_call_with_no_arguments(self):
        calls = []
        p = zgram.compile(
            "stmt = brk | pair\nbrk = 'break' ';' -> Break()\npair = a:num ',' num -> Pair()\nnum = [0-9]+ -> int",
            ast={"Break": lambda *a, **kw: calls.append((a, kw)) or "B", "Pair": lambda *a, **kw: calls.append((a, kw)) or "P"},
        )
        assert p.parse_ast("break;") == "B"
        assert p.parse_ast("1,2") == "P"
        assert calls == [((), {}), ((), {})]

    def test_unlabelled_children_are_ignored_when_there_are_labels(self):
        p = zgram.compile("pair = a:num ',' num -> Pair\nnum = [0-9]+ -> int", ast={"Pair": lambda **kw: kw})
        assert p.parse_ast("1,2") == {"a": 1}

    def test_label_on_silent_rule_is_a_list(self):
        p = zgram.compile(
            "call = name:id '(' args:arglist? ')' -> Call\n@silent arglist = id (',' id)*\nid = [a-z]+",
            ast={"Call": lambda **kw: kw},
        )
        assert p.parse_ast("f(a,b)") == {"name": "f", "args": ["a", "b"]}
        assert p.parse_ast("f()") == {"name": "f", "args": []}

    def test_labels_inside_silent_rule(self):
        p = zgram.compile(
            "call = name:id '(' arglist? ')' -> Call\n@silent arglist = args:id (',' args:id)*\nid = [a-z]+",
            ast={"Call": lambda **kw: kw},
        )
        assert p.parse_ast("f(a,b)") == {"name": "f", "args": ["a", "b"]}
        assert p.parse_ast("f(a)") == {"name": "f", "args": ["a"]}

    def test_label_in_alternatives_is_scalar(self):
        p = zgram.compile("item = v:num | v:word -> Item\nnum = [0-9]+ -> int\nword = [a-z]+", ast={"Item": lambda **kw: kw})
        assert p.parse_ast("7") == {"v": 7}
        assert p.parse_ast("ab") == {"v": "ab"}

    def test_label_twice_in_sequence_is_a_list(self):
        p = zgram.compile("pair = v:num ',' v:num -> Pair\nnum = [0-9]+ -> int", ast={"Pair": lambda **kw: kw})
        assert p.parse_ast("1,2") == {"v": [1, 2]}

    def test_constructor_exception_propagates(self):
        def boom(text):
            raise KeyError(text)

        p = zgram.compile("word = [a-z]+ -> Boom", ast={"Boom": boom})
        with pytest.raises(KeyError, match="abc"):
            p.parse_ast("abc")

    def test_parse_error(self, lang):
        with pytest.raises(zgram.ParseError) as e:
            lang.parse_ast("let x = ;")
        assert (e.value.offset, e.value.line, e.value.column) == (8, 1, 9)
        assert e.value.message.startswith("expected ")
        with pytest.raises(zgram.ParseError) as e:
            lang.parse_ast("1 + ", start="sum")
        assert e.value.offset == 4

    def test_parse_still_returns_the_tree(self, lang):
        root = lang.parse("let x = 1;")
        assert root.rule() == "program"
        assert root.get("body").rule() == "let"


class TestSpans:
    def test_zspan(self, lang):
        tree = lang.parse_ast("let x = 1 + 22;")
        let = tree.body[0]
        assert tree.__zspan__ == (0, 15)
        assert let.__zspan__ == (0, 15)
        assert let.name.__zspan__ == (4, 5)
        assert let.value.__zspan__ == (8, 14)
        assert let.value.right.__zspan__ == (12, 14)

    def test_znode_is_the_index_in_the_tree(self, lang):
        src = "let x = 1 + 22;"
        root = lang.parse(src)
        tree = root.to_ast()
        assert tree == lang.parse_ast(src)
        by_index = {n.index: n for rule in root.tree.rules for n in root.find(rule)}
        for obj in (tree, tree.body[0], tree.body[0].name, tree.body[0].value, tree.body[0].value.right):
            node = by_index[obj.__znode__]
            assert (node.start(), node.end()) == obj.__zspan__
        assert by_index[tree.body[0].value.__znode__].rule() == "sum"

    def test_to_ast_of_a_subtree(self, lang):
        let = lang.parse("let x = 1; let y = 2 * 3;").get_all("body")[1]
        assert let.to_ast() == Let(Name("y"), BinOp(Number("2"), "*", Number("3")))
        assert let.to_ast(spans=False).__dict__.keys() == {"name", "value"}

    def test_spans_false(self, lang):
        tree = lang.parse_ast("let x = 1;", spans=False)
        assert not hasattr(tree, "__zspan__")
        assert not hasattr(tree, "__znode__")
        assert not hasattr(tree.body[0], "__zspan__")

    def test_objects_that_cannot_take_attributes(self):
        p = zgram.compile("n = [0-9]+ -> Num", ast={"Num": int})
        assert p.parse_ast("42") == 42

    def test_frozen_dataclass(self):
        @dataclass(frozen=True)
        class Word:
            text: str

        p = zgram.compile("w = [a-z]+ -> Word", ast={"Word": Word})
        assert p.parse_ast("abc") == Word("abc")


class TestBinding:
    GRAMMAR = "w = [a-z]+ -> Word"

    def test_dict(self):
        assert zgram.compile(self.GRAMMAR, ast={"Word": str.upper}).parse_ast("ab") == "AB"

    def test_namespace(self):
        assert zgram.compile(self.GRAMMAR, ast=SimpleNamespace(Word=str.upper)).parse_ast("ab") == "AB"

    def test_missing_class(self):
        with pytest.raises(ValueError, match="ast has no 'Word'"):
            zgram.compile(self.GRAMMAR, ast={})
        with pytest.raises(ValueError, match="ast has no 'Word'"):
            zgram.compile(self.GRAMMAR, ast=SimpleNamespace())

    def test_unbound(self):
        p = zgram.compile(self.GRAMMAR)
        assert p.parse("ab").text() == "ab"
        with pytest.raises(ValueError, match="-> Word"):
            p.parse_ast("ab")

    def test_bind_later_and_rebind(self):
        p = zgram.compile(self.GRAMMAR)
        assert p.bind({"Word": str.upper}) is None
        assert p.parse_ast("ab") == "AB"
        p.bind({"Word": str.title})
        assert p.parse_ast("ab") == "Ab"

    def test_failed_bind_keeps_previous_binding(self):
        p = zgram.compile(self.GRAMMAR, ast={"Word": str.upper})
        with pytest.raises(ValueError):
            p.bind({})
        assert p.parse_ast("ab") == "AB"

    def test_parsers_of_one_grammar_bind_independently(self):
        a = zgram.compile(self.GRAMMAR, ast={"Word": str.upper})
        b = zgram.compile(self.GRAMMAR, ast={"Word": str.title})
        assert (a.parse_ast("ab"), b.parse_ast("ab")) == ("AB", "Ab")

    def test_ast_none(self):
        assert zgram.compile("w = [a-z]+", ast=None).parse_ast("ab") == "ab"

    def test_builtins_need_no_binding(self):
        assert zgram.compile("n = [0-9]+ -> int").parse_ast("12") == 12

    def test_no_reference_leaks(self):
        import sys

        cls = type("Word", (), {"__init__": lambda self, text: None})
        before = sys.getrefcount(cls)
        p = zgram.compile(self.GRAMMAR, ast={"Word": cls})
        for _ in range(100):
            p.parse_ast("ab")
        del p
        assert sys.getrefcount(cls) == before


class TestDeepAndWide:
    def test_wide(self):
        p = zgram.compile("items = num (',' num)* -> list\nnum = [0-9]+ -> int")
        assert p.parse_ast(",".join(map(str, range(10000)))) == list(range(10000))

    def test_deep(self):
        p = zgram.compile("v = '[' v ']' | num -> first\nnum = [0-9]+ -> int")
        assert p.parse_ast("[" * 500 + "7" + "]" * 500) == 7


def test_actions():
    p = zgram.compile("pair = k:word '=' v:num -> Pair\nword = [a-z]+\nnum = [0-9]+ -> int\n@silent ws = ' '*")
    assert p.actions() == ["Pair", None, "int", None]


def test_literals():
    # every literal once, in order of first appearance, lookaheads included
    p = zgram.compile(
        "stmt = 'let' ws name ws '=' ws num ws ';' | 'if' ws name ws '{' ws stmt* ws '}'\n"
        "name = !('let' | 'if') [a-z]+\nnum = [0-9]+ ('.' [0-9]+)?\n@silent ws = (' ' | 'é')*"
    )
    assert p.literals() == ["let", "=", ";", "if", "{", "}", ".", " ", "é"]
    assert zgram.compile("a = [a-z]+").literals() == []
