"""Tests for compile caching, async compile, start rules, prefix matching,
to_tuple(), parallel parsing and the vectorized loops in generated code."""

import asyncio
import json
import threading

import pytest
import zgram
from test.conftest import JSON_GRAMMAR, LIST_GRAMMAR


class TestCompileCache:
    def test_same_grammar_is_cached(self):
        a = zgram.compile(LIST_GRAMMAR)
        b = zgram.compile(LIST_GRAMMAR)
        assert a is not b  # separate parser objects...
        assert a.parse("[ab]").text() == b.parse("[ab]").text()  # ...sharing compiled code

    def test_clear_cache_keeps_existing_parsers_working(self):
        p = zgram.compile(LIST_GRAMMAR)
        zgram.clear_cache()
        assert p.parse("[a,b]").text() == "[a,b]"
        assert zgram.compile(LIST_GRAMMAR).parse("[c]").text() == "[c]"

    def test_errors_are_not_cached(self):
        for _ in range(2):
            with pytest.raises(ValueError):
                zgram.compile("a = b\n")


class TestCompileAsync:
    def test_compile_async(self):
        async def main():
            p = await zgram.compile_async("greeting = 'hi ' name\nname = [a-z]+\n")
            return p.parse("hi bob").to_tuple()

        assert asyncio.run(main()) == ("greeting", "hi bob", (("name", "bob", ()),))

    def test_compile_async_concurrent(self):
        async def main():
            ps = await asyncio.gather(*(zgram.compile_async(f"r{i} = 'x'+ '{i}'\n") for i in range(4)))
            return [p.parse(f"xx{i}").text() for i, p in enumerate(ps)]

        assert asyncio.run(main()) == ["xx0", "xx1", "xx2", "xx3"]

    def test_compile_async_error(self):
        async def main():
            await zgram.compile_async("a = (\n")

        with pytest.raises(ValueError):
            asyncio.run(main())


class TestStartRuleAndMatch:
    def test_rules(self, json_parser):
        names = json_parser.rules()
        assert names[0] == "value" and "number" in names and "ws" in names

    def test_start_rule(self, json_parser):
        assert json_parser.parse("42", start="number").rule() == "number"
        assert json_parser.parse("[1]", start="array").rule() == "array"

    def test_unknown_start_rule(self, json_parser):
        with pytest.raises(ValueError, match="unknown start rule"):
            json_parser.parse("1", start="nope")

    def test_silent_start_rule_without_nodes(self, json_parser):
        with pytest.raises(ValueError, match="no nodes"):
            json_parser.parse("   ", start="ws")

    def test_match_prefix(self, list_parser):
        m = list_parser.match("[a,b] and more")
        assert m.text() == "[a,b]"
        assert m.end() == 5

    def test_match_failure_returns_none(self, list_parser):
        assert list_parser.match("nope") is None
        assert list_parser.error is not None

    def test_match_with_start(self, json_parser):
        assert json_parser.match("123abc", start="number").text() == "123"

    def test_parse_rejects_trailing_input(self, list_parser):
        with pytest.raises(zgram.ParseError, match="unexpected input after match"):
            list_parser.parse("[a] x")


class TestToTuple:
    def test_shape(self, list_parser):
        assert list_parser.parse("[ab,c]").to_tuple() == (
            "list",
            "[ab,c]",
            (("item", "ab", ()), ("item", "c", ())),
        )

    def test_spans(self, list_parser):
        assert list_parser.parse("[ab]").to_tuple(spans=True) == ("list", 0, 4, "[ab]", (("item", 1, 3, "ab", ()),))

    def test_subtree(self, list_parser):
        assert list_parser.parse("[ab,c]")[1].to_tuple() == ("item", "c", ())

    def test_matches_node_api(self, json_parser):
        doc = json.dumps({"a": [1, 2.5, {"b": "x\\ny"}], "c": None})
        root = json_parser.parse(doc)

        def via_nodes(n):
            return (n.rule(), n.text(), tuple(via_nodes(c) for c in n))

        assert root.to_tuple() == via_nodes(root)

    def test_deep_nesting(self, json_parser):
        depth = 2000
        root = json_parser.parse("[" * depth + "]" * depth)
        t = root.to_tuple()
        for _ in range(depth):
            assert t[0] == "value"
            t = t[2][0]
            assert t[0] == "array"
            t = t[2][0] if t[2] else None
            if t is None:
                break


class TestParallelParsing:
    def test_threads_parse_large_inputs(self, json_parser):
        big = json.dumps([{"k": [1, 2, "abc" * 10]}] * 3000)
        assert len(big) > 16 * 1024  # parsed with the GIL released
        results = [None] * 4

        def work(i):
            results[i] = len(json_parser.parse(big)[0])

        threads = [threading.Thread(target=work, args=(i,)) for i in range(4)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        assert results == [3000] * 4


class TestVectorizedLoops:
    """Long runs exercise the 16/32-byte SIMD steps in generated code."""

    @pytest.mark.parametrize("n", [0, 1, 15, 16, 17, 31, 32, 33, 63, 64, 65, 1000])
    def test_long_json_strings(self, json_parser, n):
        s = "a" * n + '\\"' + "b" * n + "\\\\" + "c" * n
        doc = json.dumps([s, s])
        root = json_parser.parse(doc)
        strings = [x.text() for x in root.find("string")]
        assert [json.loads(x) for x in strings] == [json.loads(json.dumps(s))] * 2

    @pytest.mark.parametrize("n", [15, 16, 32, 100])
    def test_unterminated_string_error_position(self, json_parser, n):
        doc = '["' + "x" * n
        with pytest.raises(zgram.ParseError):
            json_parser.parse(doc)
        assert json_parser.error.offset() == len(doc)

    def test_class_branch_with_node_branch(self):
        p = zgram.compile("s = '\"' (plain | esc)* '\"'\nesc = '\\\\' [nt\"\\\\]\n@silent plain = [^\"\\\\]\n")
        doc = '"' + "x" * 40 + "\\n" + "y" * 40 + "\\t" + '"'
        assert [c.text() for c in p.parse(doc)] == ["\\n", "\\t"]

    def test_overlapping_branches_keep_ordered_choice(self):
        p = zgram.compile("doc = (kw | [a-z])* '.'\nkw = 'if'\n")
        assert [c.text() for c in p.parse("xxifyyif" * 5 + ".")] == ["if", "if"] * 5


class TestMemo:
    GRAMMAR = "expr = term '+' expr / term '-' expr / term\n{a}term = '(' expr ')' / 'x'\n"

    def test_memo_makes_nesting_linear(self):
        import time

        p = zgram.compile(self.GRAMMAR.format(a="@memo "))
        deep = "(" * 300 + "x" + ")" * 300  # 3^300 steps without @memo
        t0 = time.perf_counter()
        root = p.parse(deep)
        assert time.perf_counter() - t0 < 1.0
        assert root.text() == deep

    @pytest.mark.parametrize("text", ["((x))", "(x)+x-(x)", "((x)", "((x)+)", "x+", ""])
    def test_memo_matches_plain(self, text):
        plain = zgram.compile(self.GRAMMAR.format(a=""))
        memo = zgram.compile(self.GRAMMAR.format(a="@memo "))

        def result(p):
            try:
                return p.parse(text).to_tuple(spans=True)
            except zgram.ParseError as e:
                return (str(e), e.offset)

        assert result(plain) == result(memo)

    def test_annotations_combine(self):
        for g in ("a = ws 'x' ws\n@memo @silent ws = ' '*\n", "a = ws 'x' ws\n@silent @memo ws = ' '*\n"):
            assert zgram.compile(g).parse("  x ").to_tuple() == ("a", "  x ", ())

    def test_unknown_annotation(self):
        with pytest.raises(ValueError, match="unknown annotation"):
            zgram.compile("@fast a = 'x'\n")

    def test_slash_is_ordered_choice(self):
        assert zgram.compile("a = 'x' / 'y'\n").parse("y").text() == "y"


class TestMatches:
    def test_accepts_and_rejects(self, json_parser):
        assert json_parser.matches('{"a": [1, 2.5, "x", true, null]}') is True
        assert json_parser.matches('{"a": [1, 2,]}') is False
        assert json_parser.matches("") is False
        assert json_parser.matches(b"[1]") is True

    def test_error_after_rejection(self, json_parser):
        assert json_parser.matches('{"a": }') is False
        err = json_parser.error
        assert (err.line(), err.column(), err.message()) == (1, 7, "expected object")
        assert json_parser.matches("[1]") is True
        assert json_parser.error is None

    def test_start_rule(self, json_parser):
        assert json_parser.matches("-12.5e3", start="number") is True
        assert json_parser.matches("12x", start="number") is False
        with pytest.raises(ValueError, match="unknown start rule"):
            json_parser.matches("1", start="nope")

    def test_agrees_with_parse_on_long_input(self, json_parser):
        doc = json.dumps([{"k": "v" * 100, "n": [1.5] * 50}] * 400)
        assert len(doc) > 16 * 1024
        assert json_parser.matches(doc) is True
        assert json_parser.matches(doc[:-1]) is False

    def test_memo_grammar(self):
        p = zgram.compile("expr = term '+' expr / term '-' expr / term\n@memo term = '(' expr ')' / 'x'\n")
        deep = "(" * 300 + "x" + ")" * 300
        assert p.matches(deep) is True
        assert p.matches(deep + ")") is False

    def test_threads(self, json_parser):
        big = json.dumps([{"k": [1, 2, "abc" * 10]}] * 3000)
        results = [None] * 4

        def work(i):
            results[i] = json_parser.matches(big)

        threads = [threading.Thread(target=work, args=(i,)) for i in range(4)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        assert results == [True] * 4
