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


if __name__ == "__main__":
    os.makedirs(DATA_DIR, exist_ok=True)

    for name, gen in [
        ("small", make_small_json),
        ("medium", make_medium_json),
        ("large", make_large_json),
    ]:
        data = gen()
        path = os.path.join(DATA_DIR, f"{name}.json")
        with open(path, "w") as f:
            f.write(data)
        print(f"  {name}.json: {len(data)} bytes")
