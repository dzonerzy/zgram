#!/usr/bin/env python3
"""Generate JSON test data for benchmarks."""

import json
import os

DATA_DIR = os.path.join(os.path.dirname(__file__), "data")


def make_small_json():
    return json.dumps({"name": "John", "age": 30, "active": True})


def make_medium_json():
    return json.dumps(
        {
            "users": [
                {"id": i, "name": f"user{i}", "email": f"user{i}@example.com"}
                for i in range(20)
            ]
        }
    )


def make_large_json():
    return json.dumps(
        {
            "users": [
                {
                    "id": i,
                    "name": f"user{i}",
                    "scores": [j * 1.1 for j in range(10)],
                    "active": i % 2 == 0,
                }
                for i in range(100)
            ]
        }
    )


def make_strings_json():
    """Log-like records with long string values (deterministic)."""
    words = "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor".split()
    return json.dumps(
        [
            {
                "id": i,
                "message": " ".join(words[(i * 7 + k) % len(words)] for k in range(40)),
                "path": "/var/log/app/" + "segment-" * 8 + str(i),
            }
            for i in range(200)
        ]
    )


def make_expr(size, seed):
    """Arithmetic expression of about `size` bytes: numbers, identifiers,
    function calls, parentheses, unary minus, + - * / % ^ (deterministic)."""
    import random

    rnd = random.Random(seed)
    funcs = ["sin", "cos", "max", "min", "sqrt", "clamp", "avg"]
    names = ["x", "y", "rate", "total", "count", "alpha", "beta_2", "_tmp"]

    def number():
        r = rnd.random()
        if r < 0.5:
            return str(rnd.randint(0, 999))
        if r < 0.85:
            return f"{rnd.randint(0, 99)}.{rnd.randint(0, 99)}"
        return f"{rnd.randint(1, 9)}.{rnd.randint(0, 9)}e{rnd.choice(['', '-', '+'])}{rnd.randint(1, 12)}"

    def expr(depth):
        if depth <= 0 or rnd.random() < 0.25:
            return number() if rnd.random() < 0.6 else rnd.choice(names)
        r = rnd.random()
        if r < 0.55:
            op = rnd.choice(["+", "-", "*", "/", "%", "^"])
            sp = rnd.choice(["", " "])
            return f"{expr(depth - 1)}{sp}{op}{sp}{expr(depth - 1)}"
        if r < 0.7:
            return f"({expr(depth - 1)})"
        if r < 0.8:
            return f"-{expr(depth - 1)}"
        args = ", ".join(expr(depth - 2) for _ in range(rnd.randint(0, 3)))
        return f"{rnd.choice(funcs)}({args})"

    parts = []
    total = 0
    while total < size:
        e = expr(4)
        parts.append(e)
        total += len(e) + 3
    return " + ".join(parts)


def make_expr_deep():
    """Deeply nested parentheses and calls (backtracking-heavy)."""
    e = "x"
    for i in range(150):
        e = f"f{i % 3}({e} + {i})" if i % 2 else f"({e} * y{i % 5})"
    return e


if __name__ == "__main__":
    os.makedirs(DATA_DIR, exist_ok=True)

    for name, gen in [
        ("small", make_small_json),
        ("medium", make_medium_json),
        ("large", make_large_json),
        ("strings", make_strings_json),
    ]:
        data = gen()
        path = os.path.join(DATA_DIR, f"{name}.json")
        with open(path, "w") as f:
            f.write(data)
        print(f"  {name}.json: {len(data)} bytes")

    for name, data in [
        ("expr_small", make_expr(40, 1)),
        ("expr_medium", make_expr(1200, 2)),
        ("expr_large", make_expr(15000, 3)),
        ("expr_deep", make_expr_deep()),
    ]:
        path = os.path.join(DATA_DIR, f"{name}.txt")
        with open(path, "w") as f:
            f.write(data)
        print(f"  {name}.txt: {len(data)} bytes")
