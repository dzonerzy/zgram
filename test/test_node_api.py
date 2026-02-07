"""Tests for zgram's Python API: Node dunder methods, iteration, tree navigation, exceptions."""

import pytest
import zgram


class TestNodeStr:
    def test_str_returns_matched_text(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert str(root) == "[abc,def,ghi]"

    def test_str_on_child(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert str(root[0]) == "abc"


class TestNodeRepr:
    def test_repr_contains_rule_name(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        r = repr(root)
        assert r.startswith("Node('list'")
        assert "children)" in r

    def test_repr_on_leaf(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        leaf = root[0]
        r = repr(leaf)
        assert "Node('item'" in r


class TestNodeBool:
    def test_node_is_truthy(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert bool(root) is True

    def test_child_is_truthy(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert bool(root[0]) is True


class TestNodeLen:
    def test_len_returns_child_count(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert len(root) == 3

    def test_leaf_len_is_zero(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert len(root[0]) == 0


class TestNodeGetitem:
    def test_positive_index(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert str(root[0]) == "abc"
        assert str(root[1]) == "def"
        assert str(root[2]) == "ghi"

    def test_negative_index(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert str(root[-1]) == "ghi"
        assert str(root[-2]) == "def"
        assert str(root[-3]) == "abc"

    def test_index_out_of_bounds_raises(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        with pytest.raises(IndexError):
            root[99]

    def test_negative_index_out_of_bounds_raises(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        with pytest.raises(IndexError):
            root[-99]


class TestNodeIteration:
    def test_iter_yields_children(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        texts = [str(child) for child in root]
        assert texts == ["abc", "def", "ghi"]

    def test_iter_resets_on_second_pass(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        first = [str(c) for c in root]
        second = [str(c) for c in root]
        assert first == second

    def test_iter_on_leaf_yields_nothing(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert list(root[0]) == []

    def test_nested_iteration(self, json_parser):
        tree = json_parser.parse('{"name": "Alice", "scores": [100, 200]}')
        for child in tree:
            for grandchild in child:
                pass  # should not crash


class TestNodeEquality:
    def test_same_child_is_equal(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root[0] == root[0]

    def test_cross_parse_equality_no_crash(self, list_parser):
        root1 = list_parser.parse("[abc,def,ghi]")
        root2 = list_parser.parse("[abc,def,ghi]")
        # May or may not be equal (different pointers), but must not crash
        _ = root1[0] == root2[0]


class TestNodeMethods:
    def test_rule(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root.rule() == "list"
        assert root[0].rule() == "item"

    def test_text(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root.text() == "[abc,def,ghi]"
        assert root[0].text() == "abc"

    def test_start_end(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root.start() == 0
        assert root.end() == 13

    def test_child_count(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root.child_count() == 3

    def test_child_returns_node_or_none(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        c = root.child(0)
        assert c is not None
        assert c.rule() == "item"

    def test_children_returns_list(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        kids = root.children()
        assert isinstance(kids, list)
        assert len(kids) == 3
        assert [str(k) for k in kids] == ["abc", "def", "ghi"]

    def test_children_on_leaf(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root[0].children() == []


class TestNodeFind:
    def test_find_by_rule_name(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        items = root.find("item")
        assert isinstance(items, list)
        assert len(items) == 3
        assert [str(n) for n in items] == ["abc", "def", "ghi"]

    def test_find_nonexistent_rule(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root.find("nonexistent") == []

    def test_find_on_leaf(self, list_parser):
        root = list_parser.parse("[abc,def,ghi]")
        assert root[0].find("anything") == []

    def test_find_strings_in_json(self, json_parser):
        tree = json_parser.parse('{"name": "Alice", "scores": [100, 200]}')
        strings = tree.find("string")
        assert len(strings) >= 2

    def test_find_numbers_in_json(self, json_parser):
        tree = json_parser.parse('{"name": "Alice", "scores": [100, 200]}')
        numbers = tree.find("number")
        assert len(numbers) >= 2


class TestGrammarParserRepr:
    def test_repr_format(self, list_parser):
        r = repr(list_parser)
        assert "GrammarParser(" in r
        assert "rules)" in r


class TestParseError:
    def test_invalid_input_raises(self, list_parser):
        with pytest.raises(Exception):
            list_parser.parse("not_a_list")

    def test_error_has_details(self):
        parser = zgram.compile("start = 'hello'\n")
        try:
            parser.parse("world")
            pytest.fail("should have raised")
        except Exception as e:
            assert str(e)  # has a message
