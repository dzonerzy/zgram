"""A small reference PEG interpreter with zgram's semantics, for differential tests.

It works on grammar ASTs (not grammar text) and produces the same shapes as
zgram: `to_tuple(spans=True)` trees on success, and the high-water-mark error
(offset, rule name) on failure.

Semantics mirrored from the generated code:
- A non-silent rule makes one node whose children are the nodes produced by
  its expression; a @silent rule makes no node and passes its children up.
- Ordered choice, greedy repetition. A repetition stops when its body fails
  or matches without consuming input; a zero-length iteration's nodes are kept.
- Predicates never consume input or keep nodes.
- High-water mark: whenever a rule fails at a position further than any
  earlier failure, that (position, rule) becomes the error location.
- The start rule must consume the whole input.
- Expected set (src/diagnose.zig): the furthest literal, character class or
  `.` that failed, outside predicates and outside @silent rules that can match
  nothing; a node-making rule that fails where it started replaces what was
  expected inside it by its name, and a token (a node-making rule without
  child nodes) that matches forgets what was expected at its end. This is the
  error unless the rule-level
  error is further, or is trailing input at the same position.
"""

# AST nodes: ("lit", s) ("cls", set_of_chars) ("ncls", set_of_chars) ("any",) ("ref", name)
#            ("seq", [..]) ("alt", [..]) ("rep", kind, e) ("not", e) ("and", e)

FAIL = None


class TooSlow(Exception):
    """The input needs more backtracking than the step budget allows."""


class Grammar:
    def __init__(self, rules):
        # rules: list of (name, expr, silent, memo)
        self.rules = rules
        self.by_name = {r[0]: r for r in rules}
        # rule name -> what error messages call it (name "display" = ...)
        self.display = {}

    def text(self, memo_all=False):
        """Render as zgram grammar source."""
        out = []
        for name, expr, silent, memo in self.rules:
            ann = ("@silent " if silent else "") + ("@memo " if (memo or memo_all) else "")
            shown = f' "{self.display[name]}"' if name in self.display else ""
            out.append(f"{ann}{name}{shown} = {render(expr)}")
        return "\n".join(out) + "\n"


def render(e):
    k = e[0]
    if k == "lit":
        return "'" + e[1].replace("\\", "\\\\").replace("'", "\\'") + "'"
    if k == "cls":
        return "[" + "".join(sorted(e[1])) + "]"
    if k == "ncls":
        return "[^" + "".join(sorted(e[1])) + "]"
    if k == "any":
        return "."
    if k == "ref":
        return e[1]
    if k == "seq":
        return " ".join(render_atom(x) for x in e[1])
    if k == "alt":
        return " | ".join(render_atom(x) if x[0] == "alt" else render(x) for x in e[1])
    if k == "rep":
        return render_atom(e[2]) + e[1]
    if k == "not":
        return "!" + render_atom(e[1])
    if k == "and":
        return "&" + render_atom(e[1])
    raise ValueError(k)


def render_atom(e):
    # Parenthesize anything compound, so e.g. rep(not(x)) renders as (!x)?
    return render(e) if e[0] in ("lit", "cls", "ncls", "any", "ref") else "(" + render(e) + ")"


class Parser:
    def __init__(self, grammar, text, budget):
        self.g = grammar
        self.s = text
        self.steps = budget
        self.max_pos = 0
        self.max_rule = 0
        self.rule_ids = {r[0]: i for i, r in enumerate(grammar.rules)}
        # Terminal failures: furthest position and what was expected there
        self.quiet = 0
        self.term_pos = 0
        self.terms = []
        state = {}
        self.skip = {name for name, _, silent, _ in grammar.rules if silent and self.nullable(("ref", name), state)}
        self.token = {name for name, expr, silent, _ in grammar.rules if not silent and not self.has_children(expr, 0)}

    def has_children(self, e, depth):
        """Can matching `e` add nodes? (src/grammar_parser.zig nodeCount != 0)"""
        k = e[0]
        if k in ("lit", "cls", "ncls", "any", "not", "and"):
            return False
        if k == "ref":
            _, expr, silent, _ = self.g.by_name[e[1]]
            return not silent or depth >= 16 or self.has_children(expr, depth + 1)
        if k in ("seq", "alt"):
            return any(self.has_children(x, depth) for x in e[1])
        return self.has_children(e[2], depth)

    def nullable(self, e, state):
        k = e[0]
        if k in ("lit", "cls", "ncls", "any"):
            return False
        if k in ("not", "and"):
            return True
        if k == "ref":
            if e[1] not in state:
                state[e[1]] = False  # while being computed
                state[e[1]] = self.nullable(self.g.by_name[e[1]][1], state)
            return state[e[1]]
        if k == "seq":
            return all(self.nullable(x, state) for x in e[1])
        if k == "alt":
            return any(self.nullable(x, state) for x in e[1])
        return e[1] != "+" or self.nullable(e[2], state)

    def fail(self, e, pos):
        if self.quiet or pos < self.term_pos:
            return FAIL
        if pos > self.term_pos:
            self.term_pos, self.terms = pos, []
        text = render(e) if e[0] != "any" else "any character"
        if (text, e[0] == "lit") not in self.terms and len(self.terms) < 16:
            self.terms.append((text, e[0] == "lit"))
        return FAIL

    def term_message(self):
        shown = [t for t, lit in self.terms if lit] or [t for t, _ in self.terms]
        if len(shown) == 1:
            return "expected " + shown[0]
        return "expected " + ", ".join(shown[:-1]) + " or " + shown[-1]

    def rule(self, name, pos):
        _, expr, silent, _ = self.g.by_name[name]
        kept = len(self.terms) if self.term_pos == pos else 0
        before = (self.term_pos, len(self.terms))
        quiet = name in self.skip
        self.quiet += quiet
        r = self.expr(expr, pos)
        self.quiet -= quiet
        if r is not FAIL and name in self.token and not self.quiet and self.term_pos == r[0]:
            del self.terms[before[1] if before[0] == r[0] else 0 :]
        if r is FAIL and not silent and not self.quiet and self.term_pos <= pos:
            if self.term_pos < pos:
                self.term_pos, self.terms = pos, []
            else:
                del self.terms[kept:]
            shown = self.g.display.get(name, name)
            if (shown, True) not in self.terms and len(self.terms) < 16:
                self.terms.append((shown, True))
        if r is FAIL:
            if pos > self.max_pos:
                self.max_pos, self.max_rule = pos, self.rule_ids[name]
            return FAIL
        end, kids = r
        if silent:
            return end, kids
        return end, [(name, pos, end, self.s[pos:end], tuple(kids))]

    def expr(self, e, pos):
        self.steps -= 1
        if self.steps < 0:
            raise TooSlow
        k = e[0]
        s = self.s
        if k == "lit":
            return (pos + len(e[1]), []) if s.startswith(e[1], pos) else self.fail(e, pos)
        if k == "cls":
            return (pos + 1, []) if pos < len(s) and s[pos] in e[1] else self.fail(e, pos)
        if k == "ncls":
            return (pos + 1, []) if pos < len(s) and s[pos] not in e[1] else self.fail(e, pos)
        if k == "any":
            return (pos + 1, []) if pos < len(s) else self.fail(e, pos)
        if k == "ref":
            return self.rule(e[1], pos)
        if k == "seq":
            kids = []
            for x in e[1]:
                r = self.expr(x, pos)
                if r is FAIL:
                    return FAIL
                pos, more = r
                kids += more
            return pos, kids
        if k == "alt":
            for x in e[1]:
                r = self.expr(x, pos)
                if r is not FAIL:
                    return r
            return FAIL
        if k == "rep":
            kind, sub = e[1], e[2]
            if kind == "?":
                r = self.expr(sub, pos)
                return (pos, []) if r is FAIL else r
            kids = []
            first = True
            while True:
                r = self.expr(sub, pos)
                if r is FAIL:
                    if first and kind == "+":
                        return FAIL
                    return pos, kids
                end, more = r
                kids += more
                first = False
                if end == pos:
                    return pos, kids
                pos = end
        if k in ("not", "and"):
            self.quiet += 1
            matched = self.expr(e[1], pos) is not FAIL
            self.quiet -= 1
            return (pos, []) if matched == (k == "and") else FAIL
        raise ValueError(k)


def run(grammar, text, budget=20_000):
    """("ok", tree) | ("trailing", offset) | ("terminal", offset, message).

    Raises TooSlow if parsing takes more than `budget` expression steps
    (exponential backtracking); a non-memoized parser would be as slow.
    """
    p = Parser(grammar, text, budget)
    start = grammar.rules[0][0]
    r = p.rule(start, 0)
    if r is FAIL:
        error = ("expected", p.max_pos, grammar.display.get(grammar.rules[p.max_rule][0], grammar.rules[p.max_rule][0]))
    else:
        end, nodes = r
        if end == len(text):
            return ("ok", nodes[0])
        # A failure beyond the end of the match explains why it stopped
        if p.max_pos > end:
            error = ("expected", p.max_pos, grammar.display.get(grammar.rules[p.max_rule][0], grammar.rules[p.max_rule][0]))
        else:
            error = ("trailing", end)
    # ... and the expected set at the furthest failure is more precise
    if p.terms and (p.term_pos > error[1] or (p.term_pos == error[1] and error[0] == "expected")):
        return ("terminal", p.term_pos, p.term_message())
    if error[0] == "expected":
        return ("terminal", error[1], "expected " + error[2])
    return error
