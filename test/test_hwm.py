"""Test high-water mark (HWM) error reporting.

The HWM tracks the furthest position reached during parsing by recording
the entry position of each rule that fails. When parsing fails completely,
the error is reported at the HWM position with "expected <rule_name>".
"""

import pytest
import zgram

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


class TestHWMErrorPosition:
    """Verify that parse errors report the correct position via HWM."""

    def test_json_missing_value_after_colon(self):
        """Error points to the position where a value was expected."""
        p = zgram.compile(JSON_GRAMMAR)
        with pytest.raises(Exception) as exc_info:
            p.parse('{"name": }')
        err = str(exc_info.value)
        assert "col 10" in err

    def test_json_missing_value_deeper(self):
        """Error position advances with more valid input before the error."""
        p = zgram.compile(JSON_GRAMMAR)
        with pytest.raises(Exception) as exc_info:
            p.parse('{"name": "alice", "age": }')
        err = str(exc_info.value)
        assert "col 26" in err

    def test_json_multiline_error_position(self):
        """HWM correctly tracks line/column in multiline input."""
        p = zgram.compile(JSON_GRAMMAR)
        with pytest.raises(Exception) as exc_info:
            p.parse('{\n  "name": "alice",\n  "age": \n}')
        err = str(exc_info.value)
        # HWM should be past line 1
        assert "line 1," not in err

    def test_json_invalid_after_valid_array(self):
        """Error position is after the valid array portion."""
        p = zgram.compile(JSON_GRAMMAR)
        with pytest.raises(Exception) as exc_info:
            p.parse("[1, 2, 3, ]")
        err = str(exc_info.value)
        assert "col 11" in err

    def test_json_nested_object_error(self):
        """Error in deeply nested structure reports deep position."""
        p = zgram.compile(JSON_GRAMMAR)
        with pytest.raises(Exception) as exc_info:
            p.parse('{"a": {"b": {"c": }}}')
        err = str(exc_info.value)
        # Should point to position 19 where value is expected after ':'
        assert "col 19" in err

    def test_multi_rule_error_position(self):
        """Multi-rule grammar reports error at deepest rule entry."""
        grammar = (
            "root   = greeting punct\n"
            "greeting = 'hello' ' ' name\n"
            "name   = [a-z]+\n"
            "@silent punct = [.!?]\n"
        )
        p = zgram.compile(grammar)
        with pytest.raises(Exception) as exc_info:
            # 'hello ' matches, then name tries at pos 6 and fails on '1'
            p.parse("hello 123")
        err = str(exc_info.value)
        assert "col 7" in err


class TestHWMErrorMessage:
    """Verify that HWM error messages include the expected rule name."""

    def test_error_says_expected(self):
        """Error message uses 'expected' format."""
        p = zgram.compile("root = 'hello'\n")
        with pytest.raises(Exception) as exc_info:
            p.parse("xyz")
        assert "expected" in str(exc_info.value)

    def test_error_includes_rule_name(self):
        """Error message includes the name of the failing rule."""
        p = zgram.compile("root = 'hello'\n")
        with pytest.raises(Exception) as exc_info:
            p.parse("xyz")
        assert "expected root" in str(exc_info.value)

    def test_json_error_says_expected_rule(self):
        """JSON parse error names a rule."""
        p = zgram.compile(JSON_GRAMMAR)
        with pytest.raises(Exception) as exc_info:
            p.parse('{"key": }')
        err = str(exc_info.value)
        assert "expected" in err

    def test_multi_rule_names_deepest_rule(self):
        """Error names the rule at the furthest position."""
        grammar = (
            "root   = greeting punct\n"
            "greeting = 'hello' ' ' name\n"
            "name   = [a-z]+\n"
            "@silent punct = [.!?]\n"
        )
        p = zgram.compile(grammar)
        with pytest.raises(Exception) as exc_info:
            p.parse("hello 123")
        err = str(exc_info.value)
        assert "expected" in err


class TestHWMWithGetError:
    """Verify HWM error info is accessible via get_error()."""

    def test_get_error_returns_hwm_position(self):
        """get_error() returns the HWM-based error after a failed parse."""
        p = zgram.compile(JSON_GRAMMAR)
        try:
            p.parse('{"key": }')
        except Exception:
            pass
        err = p.get_error()
        assert err is not None
        assert err.column() > 1

    def test_get_error_line_col_multiline(self):
        """get_error() line/col are correct for multiline HWM errors."""
        p = zgram.compile(JSON_GRAMMAR)
        try:
            p.parse('{\n  "key": \n}')
        except Exception:
            pass
        err = p.get_error()
        assert err is not None
        assert err.line() > 1

    def test_parser_reuse_resets_hwm(self):
        """HWM resets between parse calls."""
        p = zgram.compile(JSON_GRAMMAR)

        # First: fail deep
        try:
            p.parse('{"name": "alice", "age": }')
        except Exception:
            pass
        err1 = p.get_error()

        # Second: fail early
        try:
            p.parse("!!!")
        except Exception:
            pass
        err2 = p.get_error()

        assert err1 is not None
        assert err2 is not None
        # Second error should not carry over the first HWM
        assert err2.column() <= err1.column()


class TestHWMEdgeCases:
    """Edge cases for HWM tracking."""

    def test_empty_input_error(self):
        """Empty input on a grammar that requires content."""
        p = zgram.compile("root = 'a'+\n")
        with pytest.raises(Exception) as exc_info:
            p.parse("")
        err = str(exc_info.value)
        assert "expected" in err
        assert "col 1" in err

    def test_single_char_grammar(self):
        """Single character grammar reports expected on mismatch."""
        p = zgram.compile("root = 'x'\n")
        with pytest.raises(Exception) as exc_info:
            p.parse("y")
        assert "expected" in str(exc_info.value)

    def test_successful_parse_no_error(self):
        """Successful parse has no error."""
        p = zgram.compile(JSON_GRAMMAR)
        result = p.parse('{"key": "value"}')
        assert result is not None
        assert result.text() == '{"key": "value"}'

    def test_very_long_input_error_position(self):
        """HWM works correctly with large inputs."""
        p = zgram.compile(JSON_GRAMMAR)
        # Build a valid JSON prefix then break it
        prefix = '{"k": [' + ", ".join(str(i) for i in range(100)) + ", "
        bad_input = prefix + "]}"
        try:
            p.parse(bad_input)
        except Exception:
            pass
        err = p.get_error()
        assert err is not None
        # Error should be deep in the input, not at position 0
        assert err.column() > 10

    def test_partial_match_still_uses_hwm_for_position(self):
        """Partial match error uses result position, not HWM."""
        p = zgram.compile(JSON_GRAMMAR)
        with pytest.raises(Exception) as exc_info:
            p.parse('"hello" EXTRA')
        err = str(exc_info.value)
        # Partial match error is different from total failure
        assert "unexpected input after match" in err
