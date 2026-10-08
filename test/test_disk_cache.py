"""Compiled grammars kept on disk between processes (disk_cache.zig):
zgram.configure(cache=..., cache_size=...), clear_cache(disk=True)."""

import os
import subprocess
import sys

import pytest
import zgram
from test.conftest import JSON_GRAMMAR, LIST_GRAMMAR

# A fresh process: the grammar compiled with the cache at the directory
# given; what it parses, and how long compiling took
FRESH = r"""
import json, sys, time
import zgram
zgram.configure(cache=sys.argv[1])
t = time.perf_counter()
p = zgram.compile(sys.argv[2])
took = time.perf_counter() - t
print(json.dumps({"tree": p.parse(sys.argv[3]).to_tuple(), "matches": p.matches(sys.argv[3]), "took": took}))
"""


def fresh(cache, grammar, text):
    import json

    r = subprocess.run([sys.executable, "-c", FRESH, str(cache), grammar, text], capture_output=True, text=True, timeout=600, env=os.environ)
    assert r.returncode == 0, r.stderr
    return json.loads(r.stdout)


def objects(cache):
    return sorted(n for n in os.listdir(cache) if n.endswith(".o"))


def test_kept_and_loaded_by_another_process(tmp_path):
    first = fresh(tmp_path, JSON_GRAMMAR, '{"a": [1, 2]}')
    kept = objects(tmp_path)
    # (the parser and the validator matches() compiled)
    assert len(kept) == 2
    again = fresh(tmp_path, JSON_GRAMMAR, '{"a": [1, 2]}')
    assert again["tree"] == first["tree"] and again["matches"] is True
    assert objects(tmp_path) == kept
    assert again["took"] < first["took"]


def test_a_damaged_object_is_compiled_again(tmp_path):
    first = fresh(tmp_path, LIST_GRAMMAR, "[a,b]")
    for name in objects(tmp_path):
        path = tmp_path / name
        data = path.read_bytes()
        path.write_bytes(data[:40] + bytes(len(data) - 40))
    assert fresh(tmp_path, LIST_GRAMMAR, "[a,b]")["tree"] == first["tree"]
    # (replaced: the next process loads it)
    for name in objects(tmp_path):
        assert (tmp_path / name).read_bytes()[40:].strip(b"\0")


def test_cut_short(tmp_path):
    first = fresh(tmp_path, LIST_GRAMMAR, "[a]")
    for name in objects(tmp_path):
        (tmp_path / name).write_bytes(b"short")
    assert fresh(tmp_path, LIST_GRAMMAR, "[a]")["tree"] == first["tree"]


def test_off(tmp_path):
    zgram.configure(cache=False)
    try:
        zgram.clear_cache()
        assert zgram.compile(LIST_GRAMMAR).parse("[a]").text() == "[a]"
    finally:
        zgram.configure(cache=str(tmp_path))
    assert objects(tmp_path) == []


def test_parsers_share_a_loaded_object(tmp_path):
    zgram.configure(cache=str(tmp_path))
    zgram.clear_cache()
    grammar = LIST_GRAMMAR + "\n"
    a = zgram.compile(grammar)
    # (compiled again after the memory cache dropped it: the same object,
    # shared; each parser keeps it while it lives)
    zgram.clear_cache()
    b = zgram.compile(grammar)
    assert a.parse("[x,y]").to_tuple() == b.parse("[x,y]").to_tuple()
    del a
    zgram.clear_cache()
    assert b.parse("[z]").text() == "[z]"
    c = zgram.compile(grammar)
    del b
    zgram.clear_cache()
    assert c.parse("[w]").text() == "[w]"


def test_the_recovering_parser_kept(tmp_path):
    zgram.configure(cache=str(tmp_path))
    zgram.clear_cache()
    p = zgram.compile(LIST_GRAMMAR + "  \n")
    p.parse_tree("[a,,b]", recover=True)
    assert len(objects(tmp_path)) == 2


def test_clear_cache_disk(tmp_path):
    zgram.configure(cache=str(tmp_path))
    zgram.clear_cache()
    zgram.compile(LIST_GRAMMAR + "   \n")
    assert objects(tmp_path)
    zgram.clear_cache(disk=True)
    assert objects(tmp_path) == []


def test_size_limited(tmp_path):
    zgram.configure(cache=str(tmp_path), cache_size=1)
    try:
        zgram.clear_cache()
        for i in range(3):
            zgram.compile(LIST_GRAMMAR + " " * (10 + i) + "\n")
        # (looked over as each is written past the limit: the older ones go)
        assert len(objects(tmp_path)) <= 1
    finally:
        zgram.configure(cache_size=256 << 20)


def test_settings_checked():
    with pytest.raises(TypeError):
        zgram.configure(cache=3)
    with pytest.raises(TypeError):
        zgram.configure(cache_size="big")
    with pytest.raises(ValueError):
        zgram.configure(cache_size=-1)
