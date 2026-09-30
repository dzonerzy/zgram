"""parse_tree(), Tree and the zgram.tree.v1 capsule."""

import ctypes
import gc
import struct

import pytest
import zgram

GRAMMAR = "pair = key:word '=' val:num\nword = [a-z]+\nnum = [0-9]+"


@pytest.fixture(scope="module")
def parser():
    return zgram.compile(GRAMMAR)


def unpack(nodes):
    """[(start, end, subtree_size, child_count, rule_id, field_id)]"""
    out = []
    for start, end, size, meta in struct.iter_unpack("<IIII", nodes):
        out.append((start, end, size, meta & 0xFFF, (meta >> 12) & 0xFFF, meta >> 24))
    return out


def test_tree_abi():
    assert zgram.TREE_ABI == 1


def test_parse_tree(parser):
    tree = parser.parse_tree("ab=12")
    assert type(tree).__name__ == "Tree"
    assert len(tree) == 3
    assert tree.root.rule() == "pair"
    assert tree.rules == ["pair", "word", "num"]
    assert tree.fields == ["key", "val"]
    assert tree.input == b"ab=12"


def test_node_array_layout(parser):
    tree = parser.parse_tree("ab=12")
    assert len(tree.nodes) == 16 * len(tree)
    assert unpack(tree.nodes) == [
        (0, 5, 2, 2, 0, 0),
        (0, 2, 0, 0, 1, 1),
        (3, 5, 0, 0, 2, 2),
    ]


def test_node_tree_and_index(parser):
    root = parser.parse("ab=12")
    assert root.index == 0
    assert [c.index for c in root] == [1, 2]
    assert root.tree.root == root
    assert root[1].tree is root.tree


def test_bytes_input_is_returned_as_is(parser):
    data = b"ab=12"
    assert parser.parse_tree(data).input is data


def test_non_ascii_input_offsets_are_utf8():
    p = zgram.compile("s = word ' ' word\nword = [^ ]+")
    tree = p.parse_tree("héllo wörld")
    assert tree.input == "héllo wörld".encode()
    (_, _, _, _, _, _), first, second = unpack(tree.nodes)
    assert tree.input[first[0] : first[1]].decode() == "héllo"
    assert tree.input[second[0] : second[1]].decode() == "wörld"


def test_parse_tree_error(parser):
    with pytest.raises(zgram.ParseError):
        parser.parse_tree("ab=")


def test_parse_tree_start_rule(parser):
    assert parser.parse_tree("12", start="num").root.rule() == "num"


class Str(ctypes.Structure):
    _fields_ = [("ptr", ctypes.POINTER(ctypes.c_char)), ("len", ctypes.c_size_t)]


class FlatNode(ctypes.Structure):
    _fields_ = [("text_start", ctypes.c_uint32), ("text_end", ctypes.c_uint32), ("subtree_size", ctypes.c_uint32), ("meta", ctypes.c_uint32)]


class TreeView(ctypes.Structure):
    _fields_ = [
        ("abi", ctypes.c_uint32),
        ("node_count", ctypes.c_uint32),
        ("nodes", ctypes.POINTER(FlatNode)),
        ("input", ctypes.POINTER(ctypes.c_char)),
        ("input_len", ctypes.c_size_t),
        ("rule_count", ctypes.c_uint32),
        ("field_count", ctypes.c_uint32),
        ("rule_names", ctypes.POINTER(Str)),
        ("field_names", ctypes.POINTER(Str)),
    ]


def view_of(capsule):
    get = ctypes.pythonapi.PyCapsule_GetPointer
    get.restype = ctypes.c_void_p
    get.argtypes = [ctypes.py_object, ctypes.c_char_p]
    ptr = get(capsule, b"zgram.tree.v1")
    assert ptr
    return ctypes.cast(ptr, ctypes.POINTER(TreeView)).contents


def names(strs, count):
    return [strs[i].ptr[: strs[i].len].decode() for i in range(count)]


def test_capsule(parser):
    tree = parser.parse_tree("ab=12")
    view = view_of(tree.capsule)
    assert view.abi == zgram.TREE_ABI
    assert view.node_count == 3
    assert view.input[: view.input_len] == b"ab=12"
    assert names(view.rule_names, view.rule_count) == ["pair", "word", "num"]
    assert names(view.field_names, view.field_count) == ["key", "val"]
    got = [(view.nodes[i].text_start, view.nodes[i].text_end, view.nodes[i].subtree_size, view.nodes[i].meta) for i in range(3)]
    assert got == list(struct.iter_unpack("<IIII", tree.nodes))


def test_capsule_keeps_the_tree_alive():
    p = zgram.compile(GRAMMAR)
    capsule = p.parse_tree("abc=" + "7" * 50).capsule
    del p
    zgram.clear_cache()
    gc.collect()
    view = view_of(capsule)
    assert view.input[: view.input_len] == b"abc=" + b"7" * 50
    assert names(view.rule_names, view.rule_count) == ["pair", "word", "num"]
    assert view.nodes[2].text_end == 54


def test_capsule_name_is_checked(parser):
    get = ctypes.pythonapi.PyCapsule_GetPointer
    get.restype = ctypes.c_void_p
    get.argtypes = [ctypes.py_object, ctypes.c_char_p]
    with pytest.raises(ValueError):
        get(parser.parse_tree("ab=12").capsule, b"zgram.tree.v2")


def test_tree_without_labels():
    tree = zgram.compile("w = [a-z]+").parse_tree("abc")
    assert tree.fields == []
    assert view_of(tree.capsule).field_count == 0


def test_node_by_index(parser):
    tree = parser.parse_tree("ab=12")
    assert [tree.node(i).rule() for i in range(len(tree))] == ["pair", "word", "num"]
    assert tree.node(2) == tree.root[1]
    assert tree.node(1).index == 1
    for bad in (-1, 3):
        with pytest.raises(IndexError):
            tree.node(bad)


def test_parent():
    p = zgram.compile("s = '(' (s | word)* ')' ' '?\nword = [a-z]+ ' '?")
    root = p.parse("(a (b (c d) e) f)")
    assert root.parent() is None

    def check(node):
        for child in node:
            assert child.parent() == node
            check(child)

    check(root)
    deepest = [n for n in root.find("word") if n.text().strip() == "c"][0]
    chain = []
    while deepest is not None:
        chain.append(deepest.rule())
        deepest = deepest.parent()
    assert chain == ["word", "s", "s", "s"]
