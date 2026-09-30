"""examples/tiny: a small language built on labels, folding, AST actions and Diagnostic."""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "examples", "tiny"))
import tiny  # noqa: E402

FIB = open(os.path.join(os.path.dirname(tiny.__file__), "fib.tiny"), encoding="utf-8").read()


def run(source):
    out, errors = [], []
    ok = tiny.run(source, output=lambda *a: out.append(" ".join(a)), errors=errors.append)
    return ok, out, errors


def test_fibonacci():
    assert run(FIB) == (True, ["0", "1", "1", "2", "3", "5", "8", "13", "21", "34"], [])


def test_ast():
    program = tiny.parse("fn add(a, b) { return a + b * 2; }\nlet x = add(1, -2);")
    assert program == tiny.Program(
        [
            tiny.FuncDef(
                tiny.Name("add"),
                [tiny.Name("a"), tiny.Name("b")],
                [tiny.Return(tiny.BinOp(tiny.Name("a"), "+", tiny.BinOp(tiny.Name("b"), "*", 2.0)))],
            ),
            tiny.Let(tiny.Name("x"), tiny.Call(tiny.Name("add"), [1.0, tiny.Neg(2.0)])),
        ]
    )


@pytest.mark.parametrize(
    "source, output",
    [
        ('print("a\\tb", 1.5, 4 / 2, 1 == 1, 2 < 1);', ["a\tb 1.5 2 true false"]),
        ("let i = 0; while 1 < 2 { i = i + 1; if i >= 3 { break; } } print(i);", ["3"]),
        ("fn f() { return; } print(f());", ["nothing"]),
        ("fn even(n) { if n == 0 { return 1 == 1; } return odd(n - 1); }\nfn odd(n) { if n == 0 { return 1 == 2; } return even(n - 1); }\nprint(even(10));", ["true"]),
        ("let letter = 7; let iffy = letter % 4; print(iffy); # keywords are whole words", ["3"]),
        ('if 2 > 1 { print("yes"); } else { print("no"); }', ["yes"]),
        ("print(1 - 2 - 3, 2 * 3 + 4, 2 * (3 + 4), -(1 + 1));", ["-4 10 14 -2"]),
    ],
)
def test_programs(source, output):
    assert run(source) == (True, output, [])


@pytest.mark.parametrize(
    "source, rendered",
    [
        ("let x = 1 + ;", "<tiny>:1:13: error: expected expression [syntax]\n    1 | let x = 1 + ;\n      |             ^"),
        ("let x = ;", "<tiny>:1:9: error: expected expression [syntax]\n    1 | let x = ;\n      |         ^"),
        ("fn (a) {}", "<tiny>:1:4: error: expected name [syntax]\n    1 | fn (a) {}\n      |    ^"),
        ("print(1;", "<tiny>:1:8: error: expected operator, ',' or ')' [syntax]\n    1 | print(1;\n      |        ^"),
        ("print(y);", "<tiny>:1:7: error: undefined name 'y' [undefined-name]\n    1 | print(y);\n      |       ^"),
        ('print(1 + "a");', "<tiny>:1:7: error: cannot apply '+' to number and string [type-mismatch]\n    1 | print(1 + \"a\");\n      |       ^^^^^^^"),
        ("let a = 1;\nbreak;", "<tiny>:2:1: error: 'break' outside loop [break-outside-loop]\n    2 | break;\n      | ^^^^^^"),
        ("fn f(a) { return a; }\nprint(f(1, 2));", "<tiny>:2:7: error: f() takes 1 arguments, got 2 [arity]\n    2 | print(f(1, 2));\n      |       ^^^^^^^"),
        ("nope();", "<tiny>:1:1: error: undefined function 'nope' [undefined-function]\n    1 | nope();\n      | ^^^^"),
        ("print(7 / 0);", "<tiny>:1:7: error: division by zero [division-by-zero]\n    1 | print(7 / 0);\n      |       ^^^^^"),
    ],
)
def test_errors(source, rendered):
    ok, _, errors = run(source)
    assert (ok, errors) == (False, [rendered])
