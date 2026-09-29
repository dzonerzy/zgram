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

    def text(self, memo_all=False):
        """Render as zgram grammar source."""
        out = []
        for name, expr, silent, memo in self.rules:
            ann = ("@silent " if silent else "") + ("@memo " if (memo or memo_all) else "")
            out.append(f"{ann}{name} = {render(expr)}")
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

    def rule(self, name, pos):
        _, expr, silent, _ = self.g.by_name[name]
        r = self.expr(expr, pos)
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
            return (pos + len(e[1]), []) if s.startswith(e[1], pos) else FAIL
        if k == "cls":
            return (pos + 1, []) if pos < len(s) and s[pos] in e[1] else FAIL
        if k == "ncls":
            return (pos + 1, []) if pos < len(s) and s[pos] not in e[1] else FAIL
        if k == "any":
            return (pos + 1, []) if pos < len(s) else FAIL
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
        if k == "not":
            return FAIL if self.expr(e[1], pos) is not FAIL else (pos, [])
        if k == "and":
            return FAIL if self.expr(e[1], pos) is FAIL else (pos, [])
        raise ValueError(k)


def run(grammar, text, budget=20_000):
    """("ok", tree) | ("expected", offset, rule_name) | ("trailing", offset).

    Raises TooSlow if parsing takes more than `budget` expression steps
    (exponential backtracking); a non-memoized parser would be as slow.
    """
    p = Parser(grammar, text, budget)
    start = grammar.rules[0][0]
    r = p.rule(start, 0)
    if r is FAIL:
        return ("expected", p.max_pos, grammar.rules[p.max_rule][0])
    end, nodes = r
    if end != len(text):
        return ("trailing", end)
    return ("ok", nodes[0])
