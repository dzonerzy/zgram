"""@left / @right / @postfix: chains folded into nested nodes in the parse tree."""

import random

import pytest
import zgram


def sx(n):
    """Compact s-expression: field=rule(children), leaves as their text."""
    label = f"{n.field()}=" if n.field() else ""
    kids = [sx(c) for c in n]
    return label + (f"{n.rule()}({' '.join(kids)})" if kids else n.text())


EXPR = r"""
expr = ws sum ws
@{fold} sum     = left:product (ws op:addop ws right:product)*
@{fold} product = left:atom (ws op:mulop ws right:atom)*
@silent atom = num | '(' expr ')'
num   = [0-9]+
addop = [+\-]
mulop = [*/]
@silent ws = [ ]*
"""


@pytest.fixture(scope="module")
def left():
    return zgram.compile(EXPR.format(fold="left"))


@pytest.fixture(scope="module")
def right():
    return zgram.compile(EXPR.format(fold="right"))


class TestLeft:
    def test_single_operand_stands_in(self, left):
        root = left.parse("7")
        assert sx(root) == "expr(7)"
        assert root[0].rule() == "num"
        assert root[0].field() is None

    def test_one_operator(self, left):
        assert sx(left.parse("1+2")) == "expr(sum(left=1 op=+ right=2))"

    def test_chain_nests_to_the_left(self, left):
        assert sx(left.parse("1+2-3")) == "expr(sum(left=sum(left=1 op=+ right=2) op=- right=3))"

    def test_precedence(self, left):
        assert sx(left.parse("1+2*3-4")) == (
            "expr(sum(left=sum(left=1 op=+ right=product(left=2 op=* right=3)) op=- right=4))"
        )

    def test_parentheses(self, left):
        assert sx(left.parse("(1+2)*3")) == "expr(product(left=expr(sum(left=1 op=+ right=2)) op=* right=3))"

    def test_spans(self, left):
        outer = left.parse(" 1 + 2 - 3 ")[0]
        assert (outer.rule(), outer.text()) == ("sum", "1 + 2 - 3")
        inner = outer.get("left")
        assert (inner.rule(), inner.text()) == ("sum", "1 + 2")
        assert outer.start() == inner.start() == 1

    def test_each_node_has_three_children(self, left):
        root = left.parse("1+2+3+4+5")
        assert all(len(n) == 3 for n in root.find("sum"))
        assert len(root.find("sum")) == 4

    def test_to_tuple(self, left):
        assert left.parse("1+2").to_tuple() == (
            "expr",
            "1+2",
            (("sum", "1+2", (("num", "1", ()), ("addop", "+", ()), ("num", "2", ()))),),
        )


class TestRight:
    def test_single_operand_stands_in(self, right):
        assert sx(right.parse("7")) == "expr(7)"

    def test_chain_nests_to_the_right(self, right):
        assert sx(right.parse("1+2-3")) == "expr(sum(left=1 op=+ right=sum(left=2 op=- right=3)))"

    def test_long_chain(self, right):
        assert sx(right.parse("1*2*3*4")) == (
            "expr(product(left=1 op=* right=product(left=2 op=* right=product(left=3 op=* right=4))))"
        )

    def test_spans(self, right):
        outer = right.parse(" 1 + 2 - 3 ")[0]
        assert outer.text() == "1 + 2 - 3"
        assert outer.get("right").text() == "2 - 3"
        assert outer.get("right").rule() == "sum"


class TestPostfix:
    GRAMMAR = r"""
    @postfix post = target:prim (call | index | member)*
    call   = '(' (args:post (',' args:post)*)? ')'
    index  = '[' idx:post ']'
    member = '.' name:ident
    @silent prim = ident | num
    ident  = [a-z]+
    num    = [0-9]+
    """

    @pytest.fixture
    def parser(self):
        return zgram.compile(self.GRAMMAR)

    def test_no_suffix(self, parser):
        root = parser.parse("a")
        assert (root.rule(), root.text(), root.field(), len(root)) == ("ident", "a", None, 0)

    def test_suffix_adopts_target(self, parser):
        root = parser.parse("a.b")
        assert sx(root) == "member(target=a name=b)"
        assert root.text() == "a.b"

    def test_chain(self, parser):
        assert sx(parser.parse("a.b(c,1)[2].d")) == (
            "member(target=index(target=call(target=member(target=a name=b) args=c args=1) idx=2) name=d)"
        )

    def test_suffix_without_children(self, parser):
        root = parser.parse("f()")
        assert sx(root) == "call(target=f)"
        assert len(root) == 1

    def test_nested_chains(self, parser):
        assert sx(parser.parse("f(g(x).y)")) == "call(target=f args=member(target=call(target=g args=x) name=y))"

    def test_spans(self, parser):
        root = parser.parse("a.b(c)")
        assert root.text() == "a.b(c)"
        assert root.get("target").text() == "a.b"
        assert root.get("target").get("target").text() == "a"

    def test_repetition_of_several_nodes_gets_the_rules_own_node(self):
        p = zgram.compile("@postfix chain = head:item (sep item)*\nitem = [a-z]+\nsep = '-'")
        assert sx(p.parse("a-b-c")) == "chain(head=chain(head=a - b) - c)"


class TestShapes:
    def test_optional_group(self):
        p = zgram.compile("@left cmp = left:num (op:cmpop right:num)?\nnum = [0-9]+\ncmpop = '<' | '>'")
        assert sx(p.parse("1")) == "1"
        assert sx(p.parse("1<2")) == "cmp(left=1 op=< right=2)"
        with pytest.raises(zgram.ParseError):
            p.parse("1<2<3")

    def test_plus_group(self):
        p = zgram.compile("@left app = fn:atom (arg:atom)+\natom = [a-z]")
        assert sx(p.parse("fxy")) == "app(fn=app(fn=f arg=x) arg=y)"
        with pytest.raises(zgram.ParseError):
            p.parse("f")

    def test_unlabelled(self):
        p = zgram.compile("@left sum = num (op num)*\nnum = [0-9]+\nop = '+'")
        assert sx(p.parse("1+2+3")) == "sum(sum(1 + 2) + 3)"

    def test_silent_operator(self):
        p = zgram.compile("@left sum = num ('+' num)*\nnum = [0-9]+")
        assert sx(p.parse("1+2+3")) == "sum(sum(1 2) 3)"

    def test_head_without_nodes_is_an_ordinary_node(self):
        p = zgram.compile("@left r = 'x' (item)*\nitem = [a-z]")
        root = p.parse("x")
        assert (root.rule(), root.text(), len(root)) == ("r", "x", 0)
        assert sx(p.parse("xab")) == "r(r(a) b)"

    def test_head_with_several_nodes(self):
        p = zgram.compile("@left r = item item ('+' item)*\nitem = [a-z]")
        assert sx(p.parse("ab")) == "r(a b)"
        assert sx(p.parse("ab+c+d")) == "r(r(a b c) d)"

    def test_prefix_before_head(self):
        p = zgram.compile("@left r = '<' left:item (op:sep right:item)* \nitem = [a-z]\nsep = ','")
        root = p.parse("<a,b")
        assert sx(root) == "r(left=a op=, right=b)"
        assert root.text() == "<a,b"

    def test_fold_rule_as_start_rule(self):
        p = zgram.compile("@left sum = left:num (op:plus right:num)*\nnum = [0-9]+\nplus = '+'")
        assert p.parse("5").rule() == "num"
        assert p.parse("5+6").rule() == "sum"
        assert p.match("5+6 rest").end() == 3

    def test_label_from_caller(self):
        p = zgram.compile(
            "let = name:id '=' value:sum\n@left sum = left:num (op:plus right:num)*\nnum = [0-9]+\nplus = '+'\nid = [a-z]+"
        )
        assert sx(p.parse("x=1")) == "let(name=x value=1)"
        assert sx(p.parse("x=1+2")) == "let(name=x value=sum(left=1 op=+ right=2))"

    def test_backtracking_over_a_folded_rule(self):
        p = zgram.compile(
            "root = sum '!' | sum '?'\n@left sum = left:num (op:plus right:num)*\nnum = [0-9]+\nplus = '+'"
        )
        assert sx(p.parse("1+2+3?")) == "root(sum(left=sum(left=1 op=+ right=2) op=+ right=3))"

    def test_trailing_operator_is_not_consumed(self):
        p = zgram.compile("root = sum tail\n@left sum = left:num (op:plus right:num)*\nnum = [0-9]+\nplus = '+'\ntail = '+' '!'")
        assert sx(p.parse("1+2+!")) == "root(sum(left=1 op=+ right=2) +!)"

    @pytest.mark.parametrize("memo", ["@memo @left", "@left @memo"])
    def test_with_memo(self, memo):
        p = zgram.compile(
            f"root = sum '!' | sum '?' | sum\n{memo} sum = left:num (op:plus right:num)*\n@memo num = [0-9]+\nplus = '+'"
        )
        for suffix in ("!", "?", ""):
            assert sx(p.parse("1+2+3" + suffix)) == "root(sum(left=sum(left=1 op=+ right=2) op=+ right=3))"
            assert sx(p.parse("4" + suffix)) == "root(4)"

    def test_matches(self, left):
        assert left.matches("1+2*(3-4)")
        assert not left.matches("1+")

    def test_error_position(self, left):
        with pytest.raises(zgram.ParseError) as e:
            left.parse("1+2*")
        assert (e.value.offset, e.value.message) == (4, "expected num or '('")


class TestLongChains:
    @pytest.mark.parametrize("fold", ["left", "right"])
    @pytest.mark.parametrize("n", [2, 100, 300, 5000])
    def test_depth_and_order(self, fold, n):
        p = zgram.compile(f"@{fold} sum = left:num (op:plus right:num)*\nnum = [0-9]+\nplus = '+'")
        node = p.parse("+".join(str(i) for i in range(n)))
        inner, leaf = ("left", "right") if fold == "left" else ("right", "left")
        seen = []
        while node.rule() == "sum":
            assert len(node) == 3
            seen.append(int(node.get(leaf).text()))
            node = node.get(inner)
        seen.append(int(node.text()))
        assert seen == (list(range(n))[::-1] if fold == "left" else list(range(n)))

    def test_long_postfix_chain(self):
        p = zgram.compile("@postfix post = target:id (member)*\nmember = '.' name:id\nid = [a-z]+")
        node = p.parse(".".join(["ab"] * 3000))
        depth = 0
        while node.rule() == "member":
            assert len(node) == 2
            node = node.get("target")
            depth += 1
        assert depth == 2999


class TestAgainstPythonFold:
    """Fold the unannotated (flat) tree in Python and compare."""

    FLAT = EXPR.replace("@{fold} ", "")

    @staticmethod
    def shape(n):
        return (n.rule(), n.field(), n.start(), n.end(), [TestAgainstPythonFold.shape(c) for c in n])

    @classmethod
    def fold(cls, n, how, field=None):
        kids = list(n)
        if n.rule() not in ("sum", "product"):
            return (n.rule(), field, n.start(), n.end(), [cls.fold(c, how, c.field()) for c in kids])
        items = [cls.fold(c, how, c.field()) for c in kids]
        if len(items) == 1:
            return items[0][:1] + (field,) + items[0][2:]
        def relabel(t, f):
            return t[:1] + (f,) + t[2:]

        if how == "left":
            acc = items[0]
            for i in range(1, len(items), 2):
                last = i + 2 >= len(items)
                end = n.end() if last else items[i + 1][3]
                acc = (n.rule(), field if last else "left", n.start(), end, [relabel(acc, "left"), items[i], items[i + 1]])
            return acc
        acc = items[-1]
        for i in range(len(items) - 2, 0, -2):
            first = i == 1
            start = n.start() if first else items[i - 1][2]
            acc = (n.rule(), field if first else "right", start, n.end(), [relabel(items[i - 1], "left"), items[i], acc])
        return acc

    @staticmethod
    def random_expr(rng, depth=0):
        parts = []
        for i in range(rng.randint(1, 5)):
            if i:
                parts.append(rng.choice(["+", "-", "*", "/", " + ", " * "]))
            if depth < 3 and rng.random() < 0.25:
                parts.append("(" + TestAgainstPythonFold.random_expr(rng, depth + 1) + ")")
            else:
                parts.append(str(rng.randint(0, 99)))
        return "".join(parts)

    @pytest.mark.parametrize("how", ["left", "right"])
    def test_random_expressions(self, how):
        flat = zgram.compile(self.FLAT)
        folded = zgram.compile(EXPR.format(fold=how))
        rng = random.Random(1234)
        for _ in range(500):
            text = self.random_expr(rng)
            assert self.shape(folded.parse(text)) == self.fold(flat.parse(text), how), text


class TestErrors:
    @pytest.mark.parametrize(
        "grammar",
        [
            "@left r = item\nitem = 'a'",
            "@left r = item*\nitem = 'a'",
            "@left r = item (item)* item\nitem = 'a'",
            "@left r = item (item)* | item\nitem = 'a'",
            "@silent @left r = item (item)*\nitem = 'a'",
        ],
    )
    def test_shape(self, grammar):
        with pytest.raises(ValueError, match="head"):
            zgram.compile(grammar)

    @pytest.mark.parametrize("annotations", ["@left @right", "@left @left", "@postfix @left", "@none"])
    def test_conflicting_annotations(self, annotations):
        with pytest.raises(ValueError, match="annotation"):
            zgram.compile(f"{annotations} r = item (item)*\nitem = 'a'")
