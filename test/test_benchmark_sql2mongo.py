"""
Benchmark: SQL-to-MongoDB conversion latency to assess real-time viability.

Tests parse-only (zgram native) vs full convert (parse + tree walk + MongoDB output)
across queries of increasing complexity.

Run with:  pytest test/test_benchmark_sql2mongo.py -v -s
"""

import os
import sys
import time

import pytest
import zgram

# Add examples to path so we can import sql2mongo
sys.path.insert(
    0, os.path.join(os.path.dirname(__file__), "..", "examples", "sql2mongo")
)
from sql2mongo import SQLToMongo

# ── Test queries of increasing complexity ──

QUERIES = [
    ("Simple SELECT *", "SELECT * FROM users"),
    ("WHERE filter", "SELECT name, email FROM users WHERE age > 25"),
    (
        "AND + comparisons",
        "SELECT * FROM orders WHERE status = 'active' AND total >= 100",
    ),
    ("BETWEEN range", "SELECT * FROM users WHERE age BETWEEN 18 AND 65"),
    ("IN list", "SELECT * FROM products WHERE category IN ('electronics', 'books')"),
    ("LIKE pattern", "SELECT * FROM users WHERE name LIKE 'John%'"),
    ("IS NOT NULL", "SELECT * FROM users WHERE email IS NOT NULL"),
    (
        "ORDER + LIMIT",
        "SELECT * FROM logs WHERE level = 'error' ORDER BY timestamp DESC LIMIT 10",
    ),
    ("DISTINCT", "SELECT DISTINCT country FROM users"),
    ("COUNT aggregate", "SELECT COUNT(*) FROM orders WHERE status = 'shipped'"),
    (
        "Nested boolean",
        "SELECT * FROM users WHERE (age > 18 AND active = true) OR role = 'admin'",
    ),
    (
        "Pagination",
        "SELECT name FROM users ORDER BY created_at DESC LIMIT 20 OFFSET 40",
    ),
]

ITERATIONS = 5000


def bench(func, iterations):
    """Warmup + timed run. Returns (total_s, us_per_call, ops_per_sec)."""
    for _ in range(min(10, iterations)):
        func()
    t0 = time.perf_counter()
    for _ in range(iterations):
        func()
    elapsed = time.perf_counter() - t0
    us_per = elapsed / iterations * 1e6
    ops = iterations / elapsed
    return elapsed, us_per, ops


class TestSql2MongoCorrectness:
    """Verify all queries convert without error."""

    @pytest.fixture(scope="class")
    def converter(self):
        return SQLToMongo()

    @pytest.mark.parametrize("label,sql", QUERIES)
    def test_converts_without_error(self, converter, label, sql):
        result = converter.convert(sql)
        assert result is not None
        assert len(result) > 0


class TestBenchmarkSql2Mongo:
    """Benchmark parse + convert latency for real-time viability assessment."""

    @pytest.fixture(scope="class")
    def converter(self):
        return SQLToMongo()

    def test_benchmark_all(self, converter):
        """Run the full SQL-to-MongoDB benchmark suite."""
        print()
        print("=" * 90)
        print(f"{'SQL-to-MongoDB Benchmark':^90}")
        print("=" * 90)
        print(
            f"  {'Query':<22} {'Parse (us)':>12} {'Convert (us)':>14} {'Overhead':>10} {'Ops/sec':>12}"
        )
        print(f"  {'-' * 22} {'-' * 12} {'-' * 14} {'-' * 10} {'-' * 12}")

        for label, sql in QUERIES:
            _, parse_us, _ = bench(lambda: converter.parser.parse(sql), ITERATIONS)
            _, convert_us, ops = bench(lambda: converter.convert(sql), ITERATIONS)
            overhead = convert_us - parse_us

            print(
                f"  {label:<22} {parse_us:>10.1f}us {convert_us:>12.1f}us "
                f"{overhead:>8.1f}us {ops:>11,.0f}"
            )

        print()
        print("  Parse = zgram native parse only")
        print("  Convert = parse + tree walk + MongoDB output")
        print("  Overhead = Python tree-walking cost")
