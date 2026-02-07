"""Tests for JSON parsing: value types, complex structures, and error reporting."""

import json

import pytest
import zgram


class TestJsonValues:
    @pytest.mark.parametrize(
        "input_str,desc",
        [
            ("true", "boolean true"),
            ("false", "boolean false"),
            ("null", "null"),
            ("42", "integer"),
            ("-3.14", "negative float"),
            ("1e10", "scientific notation"),
            ("0", "zero"),
            ('"hello"', "simple string"),
            ('"with \\"escape\\""', "escaped quotes"),
            ('"\\n\\t"', "escape sequences"),
        ],
    )
    def test_primitive_values(self, json_parser, input_str, desc):
        result = json_parser.parse(input_str)
        assert result is not None, f"failed to parse {desc}"
        assert result.text() == input_str

    @pytest.mark.parametrize(
        "input_str,desc",
        [
            ("[]", "empty array"),
            ("[1, 2, 3]", "integer array"),
            ('[1, "two", true, null]', "mixed array"),
        ],
    )
    def test_arrays(self, json_parser, input_str, desc):
        result = json_parser.parse(input_str)
        assert result is not None, f"failed to parse {desc}"
        assert result.text() == input_str

    @pytest.mark.parametrize(
        "input_str,desc",
        [
            ("{}", "empty object"),
            ('{"a": 1}', "simple object"),
            ('{"name": "John", "age": 30}', "multi-key object"),
        ],
    )
    def test_objects(self, json_parser, input_str, desc):
        result = json_parser.parse(input_str)
        assert result is not None, f"failed to parse {desc}"
        assert result.text() == input_str


class TestJsonComplex:
    def test_nested_structure(self, json_parser):
        data = {
            "users": [
                {
                    "id": i,
                    "name": f"user{i}",
                    "email": f"user{i}@example.com",
                    "active": i % 2 == 0,
                    "scores": [i * 10, i * 20, i * 30],
                    "meta": {"created": "2024-01-01", "version": 1},
                }
                for i in range(50)
            ],
            "total": 50,
            "page": 1,
            "nested": {"a": {"b": {"c": {"d": "deep"}}}},
        }
        input_str = json.dumps(data)
        result = json_parser.parse(input_str)
        assert result is not None
        assert len(result.text()) == len(input_str)


class TestJsonErrorReporting:
    def test_get_error_on_invalid_input(self, json_parser):
        try:
            json_parser.parse("{invalid}")
        except Exception:
            pass
        err = json_parser.get_error()
        assert err is not None
        assert err.message()
        assert err.offset() >= 0
        assert err.line() >= 1
        assert err.column() >= 1
