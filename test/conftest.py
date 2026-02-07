"""Shared fixtures for zgram tests."""

import json

import pytest
import zgram

# ── Common grammars ──

JSON_GRAMMAR = (
    "value     = ws (object | array | string | number | true_lit | false_lit | null_lit) ws\n"
    "object    = '{' ws (pair (',' ws pair)*)? ws '}'\n"
    "pair      = ws string ws ':' value\n"
    "array     = '[' ws (value (',' ws value)*)? ws ']'\n"
    "string    = '\"' chars '\"'\n"
    "chars     = char*\n"
    "char      = escape | plain\n"
    "escape    = '\\\\' [\"\\\\bfnrt/]\n"
    'plain     = [^"\\\\]\n'
    "number    = int frac? exp?\n"
    "int       = '-'? ('0' | [1-9] [0-9]*)\n"
    "frac      = '.' [0-9]+\n"
    "exp       = [eE] [+\\-]? [0-9]+\n"
    "true_lit  = 'true'\n"
    "false_lit = 'false'\n"
    "null_lit  = 'null'\n"
    "ws        = [ \\t\\n\\r]*\n"
)

LIST_GRAMMAR = "list   = '[' item (',' item)* ']'\nitem   = [a-z]+\n"


# ── Cache cleanup ──


def pytest_sessionstart(session):
    """Clear zgram .so cache before test session starts."""
    zgram.clear_cache()


def pytest_sessionfinish(session, exitstatus):
    """Clear zgram .so cache after test session ends."""
    zgram.clear_cache()


# ── Parsers (session-scoped so grammars compile only once) ──


@pytest.fixture(scope="session")
def json_parser():
    """Compile the JSON grammar once per session."""
    return zgram.compile(JSON_GRAMMAR)


@pytest.fixture(scope="session")
def list_parser():
    """Compile the list grammar once per session."""
    return zgram.compile(LIST_GRAMMAR)


# ── Test data generators ──


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
