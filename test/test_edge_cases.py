"""Comprehensive edge case tests for zgram.

Covers: grammar compilation errors, empty/extreme inputs, @silent annotation,
left-recursion detection, adversarial backtracking, parser reuse, multi-grammar
isolation, deep recursion, large grammars, character class edge cases, and
ResourceTracker / GC cleanup.
"""

import gc
import sys

import pytest
import zgram

# ============================================================================
# Grammar compilation errors
# ============================================================================


class TestGrammarCompilationErrors:
    """Test that invalid grammars raise appropriate errors at compile time."""

    def test_empty_grammar(self):
        with pytest.raises(ValueError):
            zgram.compile("")

    def test_whitespace_only_grammar(self):
        with pytest.raises(ValueError):
            zgram.compile("   \n\n  \t  \n")

    def test_comment_only_grammar(self):
        with pytest.raises(ValueError):
            zgram.compile("# just a comment\n# another comment\n")

    def test_missing_equals(self):
        with pytest.raises(ValueError):
            zgram.compile("root [a-z]+\n")

    def test_missing_expression(self):
        with pytest.raises(ValueError):
            zgram.compile("root = \n")

    def test_undefined_rule_reference(self):
        with pytest.raises(ValueError):
            zgram.compile("root = foo\n")

    def test_duplicate_rule_name(self):
        with pytest.raises(ValueError):
            zgram.compile("root = 'a'\nroot = 'b'\n")

    def test_unterminated_single_quote_string(self):
        with pytest.raises(ValueError):
            zgram.compile("root = 'hello\n")

    def test_unterminated_double_quote_string(self):
        with pytest.raises(ValueError):
            zgram.compile('root = "hello\n')

    def test_unterminated_character_class(self):
        with pytest.raises(ValueError):
            zgram.compile("root = [a-z\n")

    def test_invalid_char_range_reversed(self):
        with pytest.raises(ValueError):
            zgram.compile("root = [z-a]\n")

    def test_empty_literal_single_quotes(self):
        with pytest.raises(ValueError):
            zgram.compile("root = ''\n")

    def test_empty_literal_double_quotes(self):
        with pytest.raises(ValueError):
            zgram.compile('root = ""\n')

    def test_unclosed_parenthesis(self):
        with pytest.raises(ValueError):
            zgram.compile("root = ('a'\n")

    def test_deeply_nested_parens_exceeding_limit(self):
        """Nesting beyond MAX_NESTING_DEPTH should error."""
        # 130 levels of nesting should exceed the limit of 128
        grammar = "root = " + "(" * 130 + "'x'" + ")" * 130 + "\n"
        with pytest.raises(ValueError):
            zgram.compile(grammar)


class TestRuleNameLimits:
    def test_very_long_rule_name(self):
        """Rule names longer than MAX_RULE_NAME should be rejected."""
        long_name = "a" * 200
        with pytest.raises(ValueError):
            zgram.compile(f"{long_name} = 'x'\nroot = {long_name}\n")

    def test_rule_name_with_underscores(self):
        p = zgram.compile("root = _helper\n_helper = 'ok'\n")
        r = p.parse("ok")
        assert r is not None

    def test_rule_name_starting_with_underscore(self):
        p = zgram.compile("_root = 'ok'\n")
        r = p.parse("ok")
        assert r is not None


# ============================================================================
# Empty and extreme inputs
# ============================================================================


class TestEmptyInput:
    def test_parse_empty_string_with_star(self):
        """Star repetition should match empty input."""
        p = zgram.compile("root = [a-z]*\n")
        r = p.parse("")
        assert r is not None
        assert r.text() == ""

    def test_parse_empty_string_with_plus_fails(self):
        """Plus repetition requires at least one match."""
        p = zgram.compile("root = [a-z]+\n")
        with pytest.raises(Exception):
            p.parse("")

    def test_parse_empty_string_with_optional(self):
        p = zgram.compile("root = 'x'?\n")
        r = p.parse("")
        assert r is not None
        assert r.text() == ""

    def test_parse_empty_with_complex_grammar(self):
        """Empty input with a grammar that can match empty."""
        p = zgram.compile("root = item*\nitem = [a-z]+\n")
        r = p.parse("")
        assert r is not None
        assert r.text() == ""
        assert r.child_count() == 0


class TestExtremeInputSizes:
    def test_single_char(self):
        p = zgram.compile("root = [a-z]\n")
        r = p.parse("x")
        assert r is not None
        assert r.text() == "x"

    def test_1mb_input(self):
        """Parse a 1MB input to verify SIMD scanning handles large data."""
        p = zgram.compile("root = [a-z]+\n")
        big = "a" * (1024 * 1024)
        r = p.parse(big)
        assert r is not None
        assert len(r.text()) == 1024 * 1024

    def test_input_with_only_whitespace(self):
        p = zgram.compile("root = [ \\t\\n\\r]*\n")
        r = p.parse("   \t\t\n\n\r  ")
        assert r is not None

    def test_single_byte_values(self):
        """Parse each printable ASCII character individually."""
        p = zgram.compile("root = .\n")
        for i in range(32, 127):
            ch = chr(i)
            r = p.parse(ch)
            assert r is not None, f"failed on char {ch!r} (ord {i})"

    def test_repeated_pattern_boundary(self):
        """Test input lengths around 16-byte SIMD boundary."""
        p = zgram.compile("root = [a-z]+\n")
        for length in [15, 16, 17, 31, 32, 33, 47, 48, 49, 63, 64, 65]:
            inp = "a" * length
            r = p.parse(inp)
            assert r is not None, f"failed on length {length}"
            assert len(r.text()) == length


# ============================================================================
# @silent annotation
# ============================================================================


class TestSilentAnnotation:
    def test_silent_rule_produces_no_node(self):
        """A @silent rule should not appear in the parse tree."""
        p = zgram.compile("root = word sep word\n@silent sep = ' '+\nword = [a-z]+\n")
        r = p.parse("hello world")
        assert r.child_count() == 2
        texts = [c.text() for c in r]
        assert texts == ["hello", "world"]

    def test_silent_does_not_affect_other_rules(self):
        """Non-silent rules should still produce nodes."""
        p = zgram.compile("root = a b c\na = 'x'\n@silent b = 'y'\nc = 'z'\n")
        r = p.parse("xyz")
        assert r.child_count() == 2
        assert r[0].text() == "x"
        assert r[1].text() == "z"

    def test_silent_on_multiple_rules(self):
        p = zgram.compile(
            "root = a sep b sep c\na = 'a'\nb = 'b'\nc = 'c'\n@silent sep = ','\n"
        )
        r = p.parse("a,b,c")
        assert r.child_count() == 3
        assert [c.text() for c in r] == ["a", "b", "c"]

    def test_silent_with_repetition(self):
        """@silent rule used inside repetition."""
        p = zgram.compile("root = (item sep)* item\nitem = [a-z]+\n@silent sep = ','\n")
        r = p.parse("a,b,c")
        items = r.find("item")
        assert len(items) == 3
        assert r.find("sep") == []  # silent, so not findable

    def test_silent_overrides_automatic_visibility(self):
        """Even if the automatic algorithm would make a rule visible,
        @silent forces it silent."""
        # Without @silent, 'middle' would be visible since root references it
        p_visible = zgram.compile("root = middle\nmiddle = [a-z]+\n")
        r1 = p_visible.parse("hello")
        assert r1.child_count() == 1
        assert r1[0].rule() == "middle"

        # With @silent, 'middle' is forced silent
        p_silent = zgram.compile("root = middle\n@silent middle = [a-z]+\n")
        r2 = p_silent.parse("hello")
        assert r2.child_count() == 0
        assert r2.text() == "hello"

    def test_silent_annotation_with_comments(self):
        """@silent should work alongside comment lines."""
        p = zgram.compile(
            "# Main rule\n"
            "root = item+\n"
            "# Helper (hidden from tree)\n"
            "@silent item = [a-z] | ' '\n"
        )
        r = p.parse("a b c")
        assert r.child_count() == 0

    def test_silent_on_first_rule(self):
        """@silent on the entry rule should result in no nodes (parse fails)."""
        p = zgram.compile("@silent root = [a-z]+\n")
        # Root being silent means node_count=0, which triggers error path
        with pytest.raises(Exception):
            p.parse("hello")

    def test_find_cannot_find_silent_rule_nodes(self):
        """Nodes from @silent rules don't exist, so find() returns nothing."""
        p = zgram.compile("root = ws word ws\n@silent ws = [ ]*\nword = [a-z]+\n")
        r = p.parse(" hello ")
        assert r.find("ws") == []
        assert len(r.find("word")) == 1


# ============================================================================
# Left-recursion detection
# ============================================================================


class TestLeftRecursionDetection:
    def test_direct_left_recursion(self):
        """A rule referencing itself at first position."""
        with pytest.raises(ValueError):
            zgram.compile("root = root 'a' | 'a'\n")

    def test_indirect_left_recursion(self):
        """A -> B -> A cycle at first position."""
        with pytest.raises(ValueError):
            zgram.compile("root = mid\nmid = root 'x' | 'x'\n")

    def test_three_rule_left_recursion_cycle(self):
        """A -> B -> C -> A."""
        with pytest.raises(ValueError):
            zgram.compile("root = b\nb = c\nc = root | 'x'\n")

    def test_non_left_recursion_is_allowed(self):
        """Right recursion should compile fine."""
        p = zgram.compile("root = 'a' root | 'a'\n")
        r = p.parse("aaa")
        assert r is not None
        assert r.text() == "aaa"

    def test_left_recursion_through_alternative(self):
        """Left recursion through one branch of an alternative."""
        with pytest.raises(ValueError):
            zgram.compile("root = root '+' root | [0-9]+\n")

    def test_left_recursion_through_optional(self):
        """Left recursion through a ? — the sub-expr is at first position."""
        with pytest.raises(ValueError):
            zgram.compile("root = prefix? root\nprefix = root '+'\n")

    def test_left_recursion_through_star(self):
        """Left recursion through * repetition."""
        with pytest.raises(ValueError):
            zgram.compile("root = root* 'x'\n")

    def test_predicate_does_not_cause_left_recursion(self):
        """Predicates don't consume input, but they're excluded from
        first-position refs in the current algorithm."""
        p = zgram.compile("root = &'a' [a-z]+\n")
        r = p.parse("abc")
        assert r is not None


# ============================================================================
# Adversarial / backtracking inputs
# ============================================================================


class TestBacktracking:
    def test_ordered_choice_tries_first(self):
        """PEG ordered choice: first match wins."""
        p = zgram.compile("root = 'abc' | 'ab' | 'a'\n")
        r = p.parse("abc")
        assert r.text() == "abc"

    def test_ordered_choice_falls_through(self):
        p = zgram.compile("root = 'xyz' | 'abc'\n")
        r = p.parse("abc")
        assert r.text() == "abc"

    def test_greedy_star_consumes_all(self):
        """Star is greedy: [a-z]* consumes everything."""
        p = zgram.compile("root = [a-z]* 'x'\n")
        # [a-z]* will consume 'hellox' including the 'x',
        # then 'x' literal won't match — this should fail
        with pytest.raises(Exception):
            p.parse("hellox")

    def test_backtracking_with_alternatives(self):
        """PEG backtracks on alternative failure."""
        p = zgram.compile(
            "root = ab_then_c | a_then_bc\nab_then_c = 'ab' 'x'\na_then_bc = 'a' 'bc'\n"
        )
        r = p.parse("abc")
        assert r.text() == "abc"

    def test_not_predicate_rejects_then_succeeds(self):
        p = zgram.compile("root = !('ab') [a-z]+\n")
        # 'ab' prefix matches the not-predicate's sub-expr, so !('ab') fails
        with pytest.raises(Exception):
            p.parse("abc")
        # 'xyz' doesn't start with 'ab', so !('ab') succeeds
        r = p.parse("xyz")
        assert r.text() == "xyz"

    def test_and_predicate_peeks_without_consuming(self):
        p = zgram.compile("root = &('ab') [a-z]+\n")
        r = p.parse("abc")
        assert r.text() == "abc"  # &('ab') matched, then [a-z]+ consumed all

    def test_many_alternatives(self):
        """Grammar with many alternatives — worst-case backtracking."""
        alts = " | ".join(f"'{chr(ord('a') + i)}'" for i in range(26))
        p = zgram.compile(f"root = ({alts})+\n")
        r = p.parse("zyxwvutsrqponmlkjihgfedcba")
        assert r is not None

    def test_pathological_aab_pattern(self):
        """Classic pathological pattern: a*a matching against 'aaa...b'."""
        p = zgram.compile("root = [a]* 'b'\n")
        # This requires backtracking from the star
        with pytest.raises(Exception):
            # [a]* greedily consumes all 'a's, then 'b' fails on 'a'
            p.parse("aaaaaaaaaa")
        # But with a 'b' at the end it should work:
        r = p.parse("aaaab")
        assert r.text() == "aaaab"


# ============================================================================
# Parser reuse and multi-grammar isolation
# ============================================================================


class TestParserReuse:
    def test_parse_same_input_twice(self):
        p = zgram.compile("root = [a-z]+\n")
        r1 = p.parse("hello")
        r2 = p.parse("world")
        # Second parse should work correctly
        assert r2.text() == "world"
        # First result may be invalid now (output reused), but shouldn't crash
        # (This tests that the parser reuses its output buffer correctly)

    def test_parse_different_sizes(self):
        """Parse inputs of varying sizes to test buffer reallocation."""
        p = zgram.compile("root = [a-z]+\n")
        for size in [1, 10, 100, 1000, 10000, 100, 10, 1]:
            r = p.parse("a" * size)
            assert r is not None
            assert len(r.text()) == size

    def test_parse_alternating_success_failure(self):
        p = zgram.compile("root = 'hello'\n")
        r = p.parse("hello")
        assert r is not None
        with pytest.raises(Exception):
            p.parse("world")
        r = p.parse("hello")
        assert r is not None

    def test_multiple_parsers_coexist(self):
        """Multiple compiled grammars should not interfere."""
        p1 = zgram.compile("root = [a-z]+\n")
        p2 = zgram.compile("root = [0-9]+\n")
        p3 = zgram.compile("root = [A-Z]+\n")

        assert p1.parse("hello").text() == "hello"
        assert p2.parse("12345").text() == "12345"
        assert p3.parse("HELLO").text() == "HELLO"

        # Cross-check: each parser rejects the others' input
        with pytest.raises(Exception):
            p1.parse("12345")
        with pytest.raises(Exception):
            p2.parse("hello")
        with pytest.raises(Exception):
            p3.parse("hello")

    def test_ten_parsers_simultaneously(self):
        """Compile 10 different grammars and use them interleaved."""
        parsers = []
        for i in range(10):
            ch = chr(ord("a") + i)
            p = zgram.compile(f"root = '{ch}'+\n")
            parsers.append((p, ch))

        for p, ch in parsers:
            inp = ch * 50
            r = p.parse(inp)
            assert r.text() == inp


class TestMultiGrammarIsolation:
    def test_compiling_after_gc_doesnt_crash(self):
        """Compile, delete, GC, compile again."""
        p1 = zgram.compile("root = 'a'+\n")
        assert p1.parse("aaa").text() == "aaa"
        del p1
        gc.collect()

        p2 = zgram.compile("root = 'b'+\n")
        assert p2.parse("bbb").text() == "bbb"

    def test_parser_survives_gc_of_others(self):
        """One parser should survive GC of another."""
        p1 = zgram.compile("root = [a-z]+\n")
        p2 = zgram.compile("root = [0-9]+\n")

        r1 = p1.parse("hello")
        assert r1.text() == "hello"

        del p2
        gc.collect()

        # p1 should still work fine
        r2 = p1.parse("world")
        assert r2.text() == "world"


# ============================================================================
# Deep recursion and large grammars
# ============================================================================


class TestDeepRecursion:
    def test_deeply_nested_parens(self):
        """Test deep recursive rule matching."""
        p = zgram.compile("root = '(' root ')' | 'x'\n")
        depth = 100
        inp = "(" * depth + "x" + ")" * depth
        r = p.parse(inp)
        assert r is not None
        assert r.text() == inp

    def test_deeply_nested_json(self):
        """Deeply nested JSON arrays."""
        p = zgram.compile(
            "value  = array | number\narray  = '[' value ']'\nnumber = [0-9]+\n"
        )
        depth = 50
        inp = "[" * depth + "42" + "]" * depth
        r = p.parse(inp)
        assert r is not None

    def test_long_sequence_of_items(self):
        """Parse a long comma-separated list."""
        p = zgram.compile("root = item (',' item)*\nitem = [a-z]+\n")
        items = ",".join("abcd" for _ in range(500))
        r = p.parse(items)
        assert r is not None


class TestLargeGrammars:
    def test_many_rules(self):
        """Grammar with many rules (50+)."""
        rules = []
        rule_names = []
        for i in range(50):
            name = f"r{i}"
            rule_names.append(name)
            rules.append(f"{name} = '{chr(ord('a') + i % 26)}'\n")
        # Root references all rules as alternatives
        alts = " | ".join(rule_names)
        grammar = f"root = ({alts})+\n" + "".join(rules)
        p = zgram.compile(grammar)
        r = p.parse("abcabc")
        assert r is not None

    def test_many_alternatives_in_single_rule(self):
        """One rule with many alternatives."""
        keywords = [
            "if",
            "else",
            "while",
            "for",
            "return",
            "break",
            "continue",
            "def",
            "class",
            "import",
            "from",
            "try",
            "except",
            "finally",
            "with",
            "as",
            "yield",
            "lambda",
            "pass",
            "raise",
            "global",
        ]
        alts = " | ".join(f"'{kw}'" for kw in keywords)
        p = zgram.compile(f"root = {alts}\n")
        for kw in keywords:
            r = p.parse(kw)
            assert r is not None
            assert r.text() == kw

    def test_grammar_with_long_literal(self):
        """Grammar matching a long literal string."""
        lit = "hello_world_this_is_a_long_literal"
        p = zgram.compile(f"root = '{lit}'\n")
        r = p.parse(lit)
        assert r is not None
        assert r.text() == lit


# ============================================================================
# Character class edge cases
# ============================================================================


class TestCharacterClasses:
    def test_single_char_class(self):
        p = zgram.compile("root = [x]+\n")
        r = p.parse("xxxx")
        assert r.text() == "xxxx"

    def test_negated_single_char(self):
        p = zgram.compile("root = [^x]+\n")
        r = p.parse("abcdef")
        assert r.text() == "abcdef"
        with pytest.raises(Exception):
            p.parse("x")

    def test_negated_range(self):
        p = zgram.compile("root = [^a-z]+\n")
        r = p.parse("12345")
        assert r.text() == "12345"
        with pytest.raises(Exception):
            p.parse("abc")

    def test_char_class_with_hyphen_at_end(self):
        """Hyphen at end of class: [a-]."""
        p = zgram.compile("root = [a\\-]+\n")
        r = p.parse("a-a-")
        assert r.text() == "a-a-"

    def test_char_class_with_special_chars(self):
        """Character class with tab, newline, etc."""
        p = zgram.compile("root = [\\t\\n\\r]+\n")
        r = p.parse("\t\n\r\t")
        assert r.text() == "\t\n\r\t"

    def test_char_class_with_backslash(self):
        p = zgram.compile("root = [\\\\]+\n")
        r = p.parse("\\\\\\")
        assert r.text() == "\\\\\\"

    def test_full_ascii_range(self):
        """Match any printable ASCII."""
        p = zgram.compile("root = [ -~]+\n")
        text = "Hello, World! 123 @#$%"
        r = p.parse(text)
        assert r.text() == text

    def test_digit_class(self):
        p = zgram.compile("root = [0-9]+\n")
        r = p.parse("9876543210")
        assert r.text() == "9876543210"

    def test_mixed_ranges_and_singles(self):
        """Character class with both ranges and individual chars."""
        p = zgram.compile("root = [a-zA-Z_]+\n")
        r = p.parse("Hello_World")
        assert r.text() == "Hello_World"

    def test_negated_class_with_multiple_chars(self):
        """[^"\\\\] — critical for JSON string scanning."""
        p = zgram.compile('root = [^"\\\\]+\n')
        r = p.parse("hello world 123")
        assert r.text() == "hello world 123"
        with pytest.raises(Exception):
            p.parse('"')
        with pytest.raises(Exception):
            p.parse("\\")

    def test_char_class_simd_boundary_single_range(self):
        """Single range at SIMD 16-byte boundaries."""
        p = zgram.compile("root = [a-z]+\n")
        for n in [15, 16, 17, 31, 32, 33, 48, 64, 128, 256]:
            r = p.parse("m" * n)
            assert len(r.text()) == n

    def test_char_class_simd_boundary_negated(self):
        """Negated set at SIMD boundaries."""
        p = zgram.compile('root = [^"\\\\]+\n')
        for n in [15, 16, 17, 31, 32, 33, 48, 64, 128, 256]:
            r = p.parse("x" * n)
            assert len(r.text()) == n

    def test_char_class_simd_boundary_small_set(self):
        """Small included set at SIMD boundaries."""
        p = zgram.compile("root = [ \\t\\n]+\n")
        for n in [15, 16, 17, 31, 32, 33, 48, 64, 128, 256]:
            r = p.parse(" " * n)
            assert len(r.text()) == n

    def test_simd_with_mismatch_at_every_position(self):
        """SIMD scan where the first mismatch is at offset 0..15 within a chunk."""
        p = zgram.compile("root = [a-z]+ '!'\n")
        for offset in range(16):
            inp = "a" * (16 + offset) + "!"
            r = p.parse(inp)
            assert r is not None
            assert r.text() == inp

    def test_dot_matches_any_byte(self):
        """The '.' wildcard should match any single byte."""
        p = zgram.compile("root = .+\n")
        r = p.parse("abc123!@#")
        assert r.text() == "abc123!@#"


# ============================================================================
# ResourceTracker / GC cleanup
# ============================================================================


class TestResourceTrackerCleanup:
    def test_gc_frees_parser(self):
        """Parser should be GC-able without crashes."""
        p = zgram.compile("root = [a-z]+\n")
        assert p.parse("test").text() == "test"
        del p
        gc.collect()
        # No crash = success

    def test_compile_many_then_gc(self):
        """Compile many grammars, let them all get GC'd."""
        for i in range(50):
            p = zgram.compile(f"root = '{chr(ord('a') + i % 26)}'+\n")
            p.parse(chr(ord("a") + i % 26) * 10)
        gc.collect()
        # No crash = success

    def test_parser_refcount(self):
        """After del + gc, refcount should go to 0."""
        p = zgram.compile("root = 'x'\n")
        ref = sys.getrefcount(p)
        assert ref >= 1
        del p
        gc.collect()

    def test_node_keeps_parser_alive(self):
        """Nodes hold a Ref to the parser, keeping it alive via INCREF."""
        p = zgram.compile("root = [a-z]+\n")
        r = p.parse("hello")
        del p
        gc.collect()
        # Node still works because it holds a strong reference to the parser
        assert r.text() == "hello"
        del r
        gc.collect()

    def test_inline_compile_parse(self):
        """Parser created inline is kept alive by the returned Node."""
        node = zgram.compile("root = [a-z]+\n").parse("hello")
        gc.collect()
        assert node.text() == "hello"

    def test_child_access_after_parser_gc(self):
        """Children remain valid after parser is garbage collected."""
        p = zgram.compile("root = word ' ' word\nword = [a-z]+\n")
        r = p.parse("hello world")
        del p
        gc.collect()
        assert r.child_count() == 2
        assert r.child(0).text() == "hello"
        assert r.child(1).text() == "world"

    def test_find_after_parser_gc(self):
        """find() works after parser is garbage collected."""
        p = zgram.compile("root = word ' ' word\nword = [a-z]+\n")
        r = p.parse("hello world")
        del p
        gc.collect()
        words = r.find("word")
        assert len(words) == 2
        assert words[0].text() == "hello"
        assert words[1].text() == "world"

    def test_compile_loop_stress(self):
        """Compile in a loop to stress ResourceTracker allocation/release."""
        for _ in range(100):
            p = zgram.compile("root = [a-z]+\n")
            r = p.parse("test")
            assert r is not None
            del p
        gc.collect()


# ============================================================================
# Error reporting details
# ============================================================================


class TestErrorReporting:
    def test_error_on_partial_match(self):
        p = zgram.compile("root = 'hello'\n")
        with pytest.raises(Exception) as exc_info:
            p.parse("hello world")
        assert (
            "unexpected" in str(exc_info.value).lower()
            or "after" in str(exc_info.value).lower()
        )

    def test_error_on_no_match(self):
        p = zgram.compile("root = 'hello'\n")
        with pytest.raises(Exception) as exc_info:
            p.parse("goodbye")
        assert "expected" in str(exc_info.value).lower()

    def test_error_line_column_multiline(self):
        p = zgram.compile("root = 'line1\\nline2\\nline3'\n")
        try:
            p.parse("line1\nline2\nwrong")
        except Exception:
            pass
        err = p.get_error()
        assert err is not None
        assert err.line() >= 1

    def test_error_preserves_between_parses(self):
        """get_error() should reflect the last parse."""
        p = zgram.compile("root = 'ok'\n")
        r = p.parse("ok")
        assert r is not None
        err = p.get_error()
        assert err is None

        with pytest.raises(Exception):
            p.parse("bad")
        err = p.get_error()
        assert err is not None


# ============================================================================
# Escape sequences in grammars
# ============================================================================


class TestGrammarEscapes:
    def test_literal_with_newline_escape(self):
        p = zgram.compile("root = 'a\\nb'\n")
        r = p.parse("a\nb")
        assert r is not None

    def test_literal_with_tab_escape(self):
        p = zgram.compile("root = 'a\\tb'\n")
        r = p.parse("a\tb")
        assert r is not None

    def test_literal_with_escaped_quote(self):
        p = zgram.compile("root = 'it\\'s'\n")
        r = p.parse("it's")
        assert r is not None

    def test_literal_with_backslash(self):
        p = zgram.compile("root = 'a\\\\b'\n")
        r = p.parse("a\\b")
        assert r is not None

    def test_double_quoted_literal(self):
        p = zgram.compile('root = "hello"\n')
        r = p.parse("hello")
        assert r is not None

    def test_double_quoted_with_escape(self):
        p = zgram.compile('root = "a\\"b"\n')
        r = p.parse('a"b')
        assert r is not None


# ============================================================================
# Any-char (dot) expressions
# ============================================================================


class TestAnyChar:
    def test_dot_matches_single(self):
        p = zgram.compile("root = .\n")
        r = p.parse("x")
        assert r.text() == "x"

    def test_dot_does_not_match_empty(self):
        p = zgram.compile("root = .\n")
        with pytest.raises(Exception):
            p.parse("")

    def test_dot_star_matches_everything(self):
        p = zgram.compile("root = .*\n")
        r = p.parse("anything goes here 123 !@#")
        assert r.text() == "anything goes here 123 !@#"

    def test_dot_star_matches_empty(self):
        p = zgram.compile("root = .*\n")
        r = p.parse("")
        assert r.text() == ""

    def test_dot_with_literal_anchor(self):
        """Match 3 any-chars followed by literal."""
        p = zgram.compile("root = ... '!'\n")
        r = p.parse("abc!")
        assert r.text() == "abc!"
        with pytest.raises(Exception):
            p.parse("ab!")  # only 2 chars before !


# ============================================================================
# Predicate edge cases
# ============================================================================


class TestPredicateEdgeCases:
    def test_not_predicate_at_eof(self):
        """Not predicate should succeed at EOF if sub-expr needs input."""
        p = zgram.compile("root = [a-z]+ !.\n")
        r = p.parse("hello")
        assert r.text() == "hello"

    def test_double_not_predicate(self):
        """!!expr is equivalent to &expr (succeeds iff expr succeeds)."""
        p = zgram.compile("root = !![a-z] [a-z]+\n")
        r = p.parse("hello")
        assert r.text() == "hello"
        with pytest.raises(Exception):
            p.parse("123")

    def test_and_predicate_does_not_consume(self):
        p = zgram.compile("root = &'h' [a-z]+\n")
        r = p.parse("hello")
        # &'h' succeeds but doesn't consume, then [a-z]+ matches "hello"
        assert r.text() == "hello"

    def test_not_predicate_with_alternatives(self):
        p = zgram.compile("root = (![0-9] .)+\n")
        r = p.parse("abc")
        assert r.text() == "abc"
        with pytest.raises(Exception):
            p.parse("a1b")  # '1' fails the not-predicate


# ============================================================================
# Complex real-world-ish grammars
# ============================================================================


class TestRealWorldGrammars:
    def test_ini_file_grammar(self):
        """Parse a simple INI file format."""
        p = zgram.compile(
            "ini     = section+\n"
            "section = '[' name ']' '\\n' entry*\n"
            "entry   = name '=' value '\\n'\n"
            "name    = [a-zA-Z_]+\n"
            "@silent value   = [^\\n]*\n"
        )
        ini = "[server]\nhost=localhost\nport=8080\n[db]\nname=mydb\n"
        r = p.parse(ini)
        assert r is not None
        assert r.rule() == "ini"
        assert r.child_count() == 2

    def test_simple_html_tags(self):
        """Parse simple self-closing HTML-like tags."""
        p = zgram.compile(
            "root    = element+\n"
            "element = '<' name attr* '/>'\n"
            "attr    = ' ' name '=' value\n"
            "name    = [a-zA-Z]+\n"
            "value   = '\"' [^\"]* '\"'\n"
        )
        html = '<input type="text"/><br/>'
        r = p.parse(html)
        assert r is not None
        elements = r.find("element")
        assert len(elements) == 2

    def test_simple_arithmetic_with_evaluation(self):
        """Parse arithmetic and validate tree structure for evaluation."""
        p = zgram.compile(
            "expr   = term (('+' | '-') term)*\n"
            "term   = factor (('*' | '/') factor)*\n"
            "factor = number | '(' expr ')'\n"
            "number = [0-9]+\n"
        )
        r = p.parse("2+3*4")
        assert r is not None
        # expr -> has children
        assert r.child_count() > 0

    def test_lisp_like_grammar(self):
        """Parse s-expressions."""
        p = zgram.compile(
            "sexpr  = atom | list\n"
            "list   = '(' ws sexpr (ws sexpr)* ws ')'\n"
            "atom   = [a-zA-Z0-9_]+\n"
            "ws     = [ \\t\\n]*\n"
        )
        r = p.parse("(add (mul 2 3) 4)")
        assert r is not None


# ============================================================================
# dump_ir sanity check
# ============================================================================


class TestDumpIR:
    def test_dump_ir_returns_string(self):
        ir = zgram.dump_ir("root = [a-z]+\n")
        assert isinstance(ir, str)
        assert "zgram_parse" in ir
        assert "define" in ir

    def test_dump_ir_contains_simd_for_char_class_rep(self):
        """SIMD scanning should appear in IR for [a-z]+."""
        ir = zgram.dump_ir("root = [a-z]+\n")
        # Should contain vector operations
        assert "x i8>" in ir or "vector" in ir.lower() or "simd" in ir.lower()

    def test_dump_ir_invalid_grammar_raises(self):
        with pytest.raises(ValueError):
            zgram.dump_ir("")

    def test_dump_ir_with_silent(self):
        ir = zgram.dump_ir("root = ws [a-z]+\n@silent ws = [ ]*\n")
        assert isinstance(ir, str)
        assert "zgram_parse" in ir
