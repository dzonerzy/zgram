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
    "@silent chars  = char*\n"
    "@silent char   = escape | plain\n"
    "@silent escape = '\\\\' [\"\\\\bfnrt/]\n"
    '@silent plain  = [^"\\\\]\n'
    "number    = int frac? exp?\n"
    "@silent int  = '-'? ('0' | [1-9] [0-9]*)\n"
    "@silent frac = '.' [0-9]+\n"
    "@silent exp  = [eE] [+\\-]? [0-9]+\n"
    "@silent true_lit  = 'true'\n"
    "@silent false_lit = 'false'\n"
    "@silent null_lit  = 'null'\n"
    "@silent ws        = [ \\t\\n\\r]*\n"
)

LIST_GRAMMAR = "list   = '[' item (',' item)* ']'\nitem   = [a-z]+\n"


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
