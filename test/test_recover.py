"""Error recovery: parse(recover=True) and parse_tree(recover=True)."""

import struct
import threading
import time

import pytest
import zgram

GRAMMAR = r"""
program = ws (stmt ws)*
@silent stmt = let_stmt | if_stmt | expr_stmt
let_stmt = 'let' kw ws name:ident ws '=' ws value:expr ws ';'
if_stmt = 'if' kw ws cond:expr ws block
block = '{' ws (stmt ws)* '}'
@silent expr_stmt = expr ws ';'
@left expr = left:atom (ws op:binop ws right:atom)*
binop = [+*]
@silent atom = num | call | ident | '(' ws expr ws ')'
call = name:ident '(' ws (args:expr (ws ',' ws args:expr)*)? ws ')'
num = [0-9]+
ident = !('let' kw | 'if' kw) [a-z]+
@silent kw = ![a-z]
@silent ws = ([ \n] | '#' [^\n]*)*
"""


@pytest.fixture(scope="module")
def parser():
    return zgram.compile(GRAMMAR)


def shape(node):
    """(rule, text, children) for a compact comparison."""
    return (node.rule(), node.text(), [shape(c) for c in node])


def top(tree):
    return [(n.rule(), n.text()) for n in tree.root]


def errors(tree):
    return [(d.line, d.column, d.message) for d in tree.errors]


class TestValidInput:
    def test_same_tree_as_without_recovery(self, parser):
        src = "let a = 1;\nif a { let b = f(a, 2) * 3; }\n"
        assert shape(parser.parse_tree(src, recover=True).root) == shape(parser.parse_tree(src).root)
        assert parser.parse_tree(src, recover=True).errors == []
        assert parser.parse_tree(src).errors == []

    def test_parse_and_node_tree(self, parser):
        root = parser.parse("let a = 1;", recover=True)
        assert root.rule() == "program" and root.tree.errors == []

    def test_without_recover_it_still_raises(self, parser):
        with pytest.raises(zgram.ParseError, match="expected expr"):
            parser.parse("let a = ;")
        with pytest.raises(zgram.ParseError):
            parser.parse_tree("let a = ;")


class TestSkipping:
    def test_broken_statement_becomes_an_error_node(self, parser):
        t = parser.parse_tree("let a = 1;\nlet b = ;\nlet c = 3;\n", recover=True)
        assert top(t) == [("let_stmt", "let a = 1;"), ("<error>", "let b = ;\n"), ("let_stmt", "let c = 3;")]
        assert errors(t) == [(2, 9, "expected expr")]

    def test_several_errors_in_one_parse(self, parser):
        src = "let a = ;\nlet b = 2;\nlet c = * ;\nlet d = 4;\nlet e = (;\nlet f = 6;\n"
        t = parser.parse_tree(src, recover=True)
        assert [r for r, _ in top(t)] == ["<error>", "let_stmt", "<error>", "let_stmt", "<error>", "let_stmt"]
        assert [(line, col) for line, col, _ in errors(t)] == [(1, 9), (3, 9), (5, 10)]
        assert all(d.code == "syntax" and d.severity == "error" for d in t.errors)

    def test_errors_inside_blocks_keep_the_block(self, parser):
        src = "if a {\n  let b = ;\n  let c = 3;\n}\nlet d = 4;\n"
        t = parser.parse_tree(src, recover=True)
        assert top(t) == [("if_stmt", "if a {\n  let b = ;\n  let c = 3;\n}"), ("let_stmt", "let d = 4;")]
        block = t.root[0].child(1)
        assert [(n.rule(), n.text()) for n in block] == [("<error>", "let b = ;\n  "), ("let_stmt", "let c = 3;")]

    def test_garbage_at_the_start(self, parser):
        t = parser.parse_tree("@@@ let a = 1;", recover=True)
        assert top(t) == [("<error>", "@@@ "), ("let_stmt", "let a = 1;")]
        assert len(t.errors) == 1

    def test_only_garbage(self, parser):
        t = parser.parse_tree("@@@", recover=True)
        assert t.root.rule() == "program"
        assert top(t) == [("<error>", "@@@")]

    def test_garbage_at_the_end(self, parser):
        t = parser.parse_tree("let a = 1;\n@@@", recover=True)
        assert top(t) == [("let_stmt", "let a = 1;"), ("<error>", "@@@")]

    def test_skipping_stays_outside_the_brackets_it_opened(self, parser):
        # `(` opens inside the broken statement: `f(x);` inside it is not
        # where to resume; `let b` after it is
        t = parser.parse_tree("let a = (1 + f(x); + ;\nlet b = 2;", recover=True)
        assert top(t)[-1] == ("let_stmt", "let b = 2;")

    def test_skipping_stops_at_a_closer_it_did_not_open(self, parser):
        # the `}` belongs to the block: the block ends there, and what follows parses
        t = parser.parse_tree("if a { let b = + }\nlet c = 3;", recover=True)
        assert top(t) == [("if_stmt", "if a { let b = + }"), ("let_stmt", "let c = 3;")]
        assert [n.rule() for n in t.root[0].child(1)] == ["<error>"]

    def test_the_error_node_is_a_leaf(self, parser):
        t = parser.parse_tree("let a = f(1, ;\nlet b = 2;", recover=True)
        err = t.root[0]
        assert err.rule() == "<error>" and len(err) == 0 and err.child_count() == 0
        # (the unclosed call doesn't swallow the next statement)
        assert top(t) == [("<error>", "let a = f(1, ;\n"), ("let_stmt", "let b = 2;")]
        assert errors(t) == [(1, 14, "expected expr")]

    def test_two_errors_inside_one_block(self, parser):
        # the block keeps its structure: the first error doesn't make the
        # whole `if` one error node, hiding the second
        src = "let a = 1;\nlet b = ;\nif a {\n  let c = f(a 2);\n  let d = 4\n}\nlet e = 5;\n"
        t = parser.parse_tree(src, recover=True)
        assert [r for r, _ in top(t)] == ["let_stmt", "<error>", "if_stmt", "let_stmt"]
        assert [n.rule() for n in t.root[2].child(1)] == ["let_stmt", "let_stmt"]
        assert errors(t) == [(2, 9, "expected expr"), (4, 15, "expected binop, ',' or ')'"), (6, 1, "expected binop or ';'")]

    def test_error_and_unclosed_block_at_the_end(self, parser):
        t = parser.parse_tree("if a {\n  let x = ;\n", recover=True)
        assert top(t) == [("if_stmt", "if a {\n  let x = ;\n")]
        assert [n.rule() for n in t.root[0].child(1)] == ["<error>"]
        assert errors(t) == [(2, 11, "expected expr"), (3, 1, "expected '}'")]

    def test_no_error_where_recovery_only_looked(self, parser):
        # looking for where to resume tries statements further on; their
        # failures are not errors of the parse
        for src in ["let a = f(1, ;\nlet b = 2;", "let a = (1 + ;\nlet b = 2;\n"]:
            assert len(parser.parse_tree(src, recover=True).errors) == 1


class TestInsertion:
    def test_missing_semicolon(self, parser):
        t = parser.parse_tree("let a = 1\nlet b = 2;", recover=True)
        # the statement keeps its structure; the error says what was missing
        assert top(t) == [("let_stmt", "let a = 1\n"), ("let_stmt", "let b = 2;")]
        assert errors(t) == [(2, 1, "expected binop or ';'")]

    def test_missing_closing_brace_at_the_end(self, parser):
        t = parser.parse_tree("let a = 1;\nif a {\n  let b = 2;\n", recover=True)
        assert [r for r, _ in top(t)] == ["let_stmt", "if_stmt"]
        assert [n.rule() for n in t.root[1].child(1)] == ["let_stmt"]
        assert len(t.errors) == 1 and "'}'" in t.errors[0].message

    def test_missing_equals(self, parser):
        t = parser.parse_tree("let a 1;", recover=True)
        assert shape(t.root) == ("program", "let a 1;", [("let_stmt", "let a 1;", [("ident", "a", []), ("num", "1", [])])])
        assert errors(t) == [(1, 7, "expected '='")]

    def test_the_first_item_of_a_sequence_is_never_made_up(self, parser):
        # `let` is what makes a let statement: a missing one is not inserted
        t = parser.parse_tree("a = 1;", recover=True)
        assert all(n.rule() != "let_stmt" for n in t.root)


class TestSyncAnnotation:
    def test_recover_resumes_after_the_sync_point(self):
        base = zgram.compile(GRAMMAR)
        synced = zgram.compile(GRAMMAR.replace("@silent stmt =", "@recover(';') @silent stmt ="))
        src = "if a { let d = + 2; x; }"
        # automatic: resumes at `2;`, a statement of its own
        assert [n.rule() for n in base.parse_tree(src, recover=True).root[0].child(1)] == ["<error>", "num", "ident"]
        # @recover(';'): the whole broken statement is skipped, up to its `;`
        block = synced.parse_tree(src, recover=True).root[0].child(1)
        assert [(n.rule(), n.text()) for n in block] == [("<error>", "let d = + 2; "), ("ident", "x")]

    def test_sync_expression_may_be_any_expression(self):
        g = zgram.compile(GRAMMAR.replace("@silent stmt =", "@recover([;\\n]) @silent stmt ="))
        t = g.parse_tree("let a = * 1\nlet b = 2;", recover=True)
        assert top(t) == [("<error>", "let a = * 1\n"), ("let_stmt", "let b = 2;")]

    def test_bad_annotations(self):
        for bad in ["@recover stmt = 'a'", "@recover( stmt = 'a'", "@recover(';') @recover(';') stmt = 'a'", "@recover(nope) stmt = 'a'"]:
            with pytest.raises(ValueError):
                zgram.compile(bad)


class TestLimits:
    def test_at_most_a_hundred_errors(self, parser):
        src = "".join(f"let a{chr(97 + i % 26)} = ;\n" for i in range(150)) + "let z = 1;"
        t = parser.parse_tree(src, recover=True)
        assert 100 <= len(t.errors) <= 101
        # the rest of the input is still in the tree, as one error node
        assert t.root.text() == src
        assert t.root.rule() == "program"
        assert t.root.children()[-1].rule() == "<error>"
        assert sum(1 for c in t.root if c.rule() == "<error>") >= 100

    def test_nested_too_deeply_still_raises(self, parser):
        with pytest.raises(zgram.ParseError, match="nested too deeply"):
            parser.parse_tree("let a = " + "(" * 2_000_000 + ";", recover=True)

    def test_large_input_with_errors_is_fast(self, parser):
        good = "let a = f(1, 2) + 3 * b;\n" * 20_000
        src = good + "let x = ;\n" + good + "let y = (;\n" + good
        start = time.perf_counter()
        t = parser.parse_tree(src, recover=True)
        elapsed = time.perf_counter() - start
        assert len(t.errors) == 2
        assert elapsed < 2.0, elapsed


class TestTreeAndAst:
    def test_rule_names(self, parser):
        t = parser.parse_tree("let a = ;", recover=True)
        assert "<error>" not in t.rules
        assert "<error>" not in parser.rules()
        assert t.root[0].rule() == "<error>"

    def test_error_node_rule_id_in_the_node_array(self, parser):
        t = parser.parse_tree("let a = ;", recover=True)
        nodes = t.nodes
        rule_ids = [(struct.unpack_from("<IIII", nodes, 16 * i)[3] >> 12) & 0xFFF for i in range(len(t))]
        # the error rule id is the grammar's rule count: one past the last rule
        assert rule_ids[1] == len(t.rules)

    def test_to_tuple_and_find(self, parser):
        t = parser.parse_tree("let a = ;\nlet b = 2;", recover=True)
        assert t.root.to_tuple()[2][0] == ("<error>", "let a = ;\n", ())
        assert [n.text() for n in t.root.find("<error>")] == ["let a = ;\n"]

    def test_parse_ast_gives_none_for_broken_text(self):
        from dataclasses import dataclass

        @dataclass
        class Let:
            name: str
            value: object

        p = zgram.compile(
            r"""
            program = ws (stmt ws)*   -> list
            @silent stmt = let_stmt
            let_stmt = 'let' kw ws name:ident ws '=' ws value:num ws ';' -> Let
            num = [0-9]+ -> int
            ident = [a-z]+ -> str
            @silent kw = ![a-z]
            @silent ws = [ \n]*
            """,
            ast={"Let": Let},
        )
        assert p.parse_ast("let a = 1; let b = ; let c = 3;", recover=True, spans=False) == [Let("a", 1), None, Let("c", 3)]

    def test_capsule_of_a_recovered_tree(self, parser):
        t = parser.parse_tree("let a = ;", recover=True)
        assert t.capsule is not None


class TestGrammarFeatures:
    def test_json_items(self):
        p = zgram.compile(
            r"""
            value = ws (object | array | num | string) ws
            object = '{' ws (pair (ws ',' ws pair)*)? ws '}'
            pair = string ws ':' value
            array = '[' ws (value (',' value)*)? ws ']'
            num = [0-9]+
            string = '"' [^"]* '"'
            @silent ws = [ \n]*
            """
        )
        t = p.parse_tree('[1, 2,, 3, {"a": 1, "b" 2, "c": 3}]', recover=True)
        assert len(t.errors) == 2
        arr = t.root.child(0)
        assert [n.rule() for n in arr][:3] == ["value", "value", "<error>"]

    def test_memo_rules(self):
        g = zgram.compile(GRAMMAR.replace("@silent atom =", "@memo atom_m = atom\n@silent atom ="))
        t = g.parse_tree("let a = ;\nlet b = 2;", recover=True)
        assert top(t) == [("<error>", "let a = ;\n"), ("let_stmt", "let b = 2;")]

    def test_folded_rules_and_labels(self, parser):
        t = parser.parse_tree("let a = 1 + 2 * 3;\nlet b = ;", recover=True)
        stmt = t.root[0]
        assert stmt.get("value").rule() == "expr"
        assert stmt.get("name").text() == "a"

    def test_start_rule_and_bytes(self, parser):
        t = parser.parse_tree(b"let a = ;\nlet b = 2;", recover=True)
        assert top(t)[1] == ("let_stmt", "let b = 2;")
        blk = parser.parse_tree("{ let a = ; let b = 2; }", start="block", recover=True)
        assert [n.rule() for n in blk.root] == ["<error>", "let_stmt"]

    def test_match_and_matches_are_unaffected(self, parser):
        assert parser.matches("let a = ;") is False
        assert parser.match("let a = 1; @@@").text() == "let a = 1; "


class TestThreads:
    def test_recovering_parser_compiled_once_from_several_threads(self):
        p = zgram.compile(GRAMMAR + "\n# threads\n")
        results = []

        def work():
            results.append(len(p.parse_tree("let a = ;\nlet b = 2;", recover=True).errors))

        threads = [threading.Thread(target=work) for _ in range(8)]
        for th in threads:
            th.start()
        for th in threads:
            th.join()
        assert results == [1] * 8
