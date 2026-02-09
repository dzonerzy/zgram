"""
SQL to MongoDB Query Converter — powered by zgram.

Parses a SQL SELECT statement and converts it to a MongoDB query.

Usage:
    python sql2mongo.py "SELECT name, age FROM users WHERE age > 25"
    python sql2mongo.py   # interactive mode

Supported SQL:
    SELECT [DISTINCT] fields|* FROM collection
      [WHERE conditions]
      [ORDER BY field [ASC|DESC] [, ...]]
      [LIMIT n]
      [OFFSET n]

    WHERE operators:  =  !=  <>  <  >  <=  >=
                      AND  OR  NOT  (...)
                      IN (val, ...)
                      LIKE 'pattern'
                      BETWEEN x AND y
                      IS NULL  /  IS NOT NULL

    Aggregates:       COUNT(*)  SUM(f)  AVG(f)  MIN(f)  MAX(f)
"""

import json
import sys

import zgram

# ============================================================================
# PEG Grammar
# ============================================================================

SQL_GRAMMAR = r"""
query       = ws select_clause from_clause where_clause? order_clause? limit_clause? offset_clause? ws semicolon? ws
@silent select_clause = select_kw ws distinct_kw? ws field_list ws
from_clause   = from_kw ws identifier ws

where_clause  = where_kw ws expression ws
order_clause  = order_kw ws by_kw ws order_list ws
limit_clause  = limit_kw ws integer ws
offset_clause = offset_kw ws integer ws

field_list  = star | field_item (ws comma ws field_item)*
field_item  = aggregate | identifier
aggregate   = agg_func ws lparen ws agg_arg ws rparen
agg_func    = count_kw | sum_kw | avg_kw | min_kw | max_kw
agg_arg     = star | identifier

@silent order_list  = order_item (ws comma ws order_item)*
order_item  = identifier ws order_dir?
order_dir   = asc_kw | desc_kw

expression  = or_expr
or_expr     = and_expr (ws or_kw ws and_expr)*
and_expr    = not_expr (ws and_kw ws not_expr)*
not_expr    = not_kw ws not_expr | primary_expr
primary_expr = lparen ws expression ws rparen | condition

condition   = between_cond | in_cond | like_cond | null_cond | comparison
between_cond = identifier ws between_kw ws value ws and_kw ws value
in_cond     = identifier ws in_kw ws lparen ws value_list ws rparen
like_cond   = identifier ws like_kw ws string
null_cond   = identifier ws is_kw ws not_null | identifier ws is_kw ws null_kw
not_null    = not_kw ws null_kw
comparison  = identifier ws comp_op ws value

@silent value_list  = value (ws comma ws value)*
value       = number | string | true_kw | false_kw | null_kw | identifier
number      = minus? digits dot_digits? exp_part?
@silent digits      = [0-9]+
@silent dot_digits  = '.' [0-9]+
@silent exp_part    = [eE] [+\-]? [0-9]+
@silent minus       = '-'
string      = squo squo_chars squo | dquo dquo_chars dquo
@silent squo_chars  = squo_char*
@silent squo_char   = squo_esc | squo_plain
@silent squo_esc    = '\\' ['\\/bfnrt]
@silent squo_plain  = [^'\\]
@silent dquo_chars  = dquo_char*
@silent dquo_char   = dquo_esc | dquo_plain
@silent dquo_esc    = '\\' ["\\/bfnrt]
@silent dquo_plain  = [^"\\]
integer     = [0-9]+

identifier  = [a-zA-Z_] [a-zA-Z0-9_.]*

comp_op     = lte | gte | ne1 | ne2 | lt | gt | eq
eq          = '='
ne1         = '!='
ne2         = '<>'
lt          = '<'
gt          = '>'
lte         = '<='
gte         = '>='

@silent select_kw   = [sS] [eE] [lL] [eE] [cC] [tT]
@silent from_kw     = [fF] [rR] [oO] [mM]
@silent where_kw    = [wW] [hH] [eE] [rR] [eE]
@silent order_kw    = [oO] [rR] [dD] [eE] [rR]
@silent by_kw       = [bB] [yY]
@silent limit_kw    = [lL] [iI] [mM] [iI] [tT]
@silent offset_kw   = [oO] [fF] [fF] [sS] [eE] [tT]
@silent and_kw      = [aA] [nN] [dD]
@silent or_kw       = [oO] [rR]
not_kw      = [nN] [oO] [tT]
@silent in_kw       = [iI] [nN]
@silent like_kw     = [lL] [iI] [kK] [eE]
@silent between_kw  = [bB] [eE] [tT] [wW] [eE] [eE] [nN]
@silent is_kw       = [iI] [sS]
null_kw     = [nN] [uU] [lL] [lL]
true_kw     = [tT] [rR] [uU] [eE]
false_kw    = [fF] [aA] [lL] [sS] [eE]
distinct_kw = [dD] [iI] [sS] [tT] [iI] [nN] [cC] [tT]
@silent asc_kw      = [aA] [sS] [cC]
desc_kw     = [dD] [eE] [sS] [cC]
@silent count_kw    = [cC] [oO] [uU] [nN] [tT]
@silent sum_kw      = [sS] [uU] [mM]
@silent avg_kw      = [aA] [vV] [gG]
@silent min_kw      = [mM] [iI] [nN]
@silent max_kw      = [mM] [aA] [xX]

star        = '*'
@silent comma       = ','
@silent lparen      = '('
@silent rparen      = ')'
@silent semicolon   = ';'
@silent squo        = "'"
@silent dquo        = '"'
@silent ws          = [ \t\n\r]*
"""

# ============================================================================
# Tree Walker — extract structured data from parse tree
# ============================================================================


def text(node):
    """Get trimmed text of a node."""
    return node.text().strip()


def extract_string(node):
    """Extract string content without quotes."""
    t = text(node)
    if (t.startswith("'") and t.endswith("'")) or (
        t.startswith('"') and t.endswith('"')
    ):
        return t[1:-1]
    return t


def extract_value(node):
    """Convert a value node to a Python value."""
    children = node.children()
    if not children:
        return text(node)

    for child in children:
        rule = child.rule()
        if rule == "number":
            t = text(child)
            return float(t) if "." in t or "e" in t.lower() else int(t)
        if rule == "string":
            return extract_string(child)
        if rule == "true_kw":
            return True
        if rule == "false_kw":
            return False
        if rule == "null_kw":
            return None
        if rule == "identifier":
            return {"$field": text(child)}
    return text(node)


def extract_comp_op(node):
    """Map SQL comparison operator to MongoDB operator."""
    children = node.children()
    if children:
        rule = children[0].rule()
        op_map = {
            "eq": "$eq",
            "ne1": "$ne",
            "ne2": "$ne",
            "lt": "$lt",
            "gt": "$gt",
            "lte": "$lte",
            "gte": "$gte",
        }
        return op_map.get(rule, "$eq")
    return "$eq"


def like_to_regex(pattern):
    """Convert SQL LIKE pattern to MongoDB regex."""
    regex = "^"
    i = 0
    while i < len(pattern):
        c = pattern[i]
        if c == "%":
            regex += ".*"
        elif c == "_":
            regex += "."
        elif c in r"\.+*?[](){}|^$":
            regex += "\\" + c
        else:
            regex += c
        i += 1
    regex += "$"
    return regex


def extract_condition(node):
    """Convert a condition node to a MongoDB filter dict."""
    children = node.children()
    if not children:
        return {}

    first_rule = children[0].rule()

    if first_rule == "comparison":
        comp = children[0]
        identifiers = comp.find("identifier")
        comp_ops = comp.find("comp_op")
        values = comp.find("value")
        if identifiers and comp_ops and values:
            field = text(identifiers[0])
            op = extract_comp_op(comp_ops[0])
            val = extract_value(values[0])
            if op == "$eq":
                return {field: val}
            return {field: {op: val}}

    if first_rule == "between_cond":
        bc = children[0]
        identifiers = bc.find("identifier")
        values = bc.find("value")
        if identifiers and len(values) >= 2:
            field = text(identifiers[0])
            low = extract_value(values[0])
            high = extract_value(values[1])
            return {field: {"$gte": low, "$lte": high}}

    if first_rule == "in_cond":
        ic = children[0]
        identifiers = ic.find("identifier")
        values = ic.find("value")
        if identifiers and values:
            field = text(identifiers[0])
            vals = [extract_value(v) for v in values]
            return {field: {"$in": vals}}

    if first_rule == "like_cond":
        lc = children[0]
        identifiers = lc.find("identifier")
        strings = lc.find("string")
        if identifiers and strings:
            field = text(identifiers[0])
            pattern = extract_string(strings[0])
            return {field: {"$regex": like_to_regex(pattern)}}

    if first_rule == "null_cond":
        nc = children[0]
        identifiers = nc.find("identifier")
        field = text(identifiers[0])
        not_nulls = nc.find("not_null")
        if not_nulls:
            return {field: {"$ne": None}}
        return {field: None}

    return {}


def children_by_rule(node, rule_name):
    """Get direct children matching a rule name (not recursive like find())."""
    return [c for c in node.children() if c.rule() == rule_name]


def extract_expression(node):
    """Recursively convert an expression tree to a MongoDB filter."""
    rule = node.rule()

    if rule == "expression":
        children = node.children()
        if children:
            return extract_expression(children[0])
        return {}

    if rule == "or_expr":
        and_exprs = children_by_rule(node, "and_expr")
        if len(and_exprs) == 1:
            return extract_expression(and_exprs[0])
        parts = [extract_expression(a) for a in and_exprs]
        return {"$or": parts}

    if rule == "and_expr":
        not_exprs = children_by_rule(node, "not_expr")
        if len(not_exprs) == 1:
            return extract_expression(not_exprs[0])
        parts = [extract_expression(n) for n in not_exprs]
        return {"$and": parts}

    if rule == "not_expr":
        children = node.children()
        has_not = any(c.rule() == "not_kw" for c in children)
        inner_not = [c for c in children if c.rule() == "not_expr"]
        inner_primary = [c for c in children if c.rule() == "primary_expr"]
        if has_not:
            target = (
                inner_not[0]
                if inner_not
                else inner_primary[0]
                if inner_primary
                else None
            )
            if target:
                return {"$not": extract_expression(target)}
            return {}
        if inner_primary:
            return extract_expression(inner_primary[0])
        if inner_not:
            return extract_expression(inner_not[0])
        return {}

    if rule == "primary_expr":
        children = node.children()
        for child in children:
            if child.rule() == "expression":
                return extract_expression(child)
            if child.rule() == "condition":
                return extract_condition(child)
        return {}

    if rule == "condition":
        return extract_condition(node)

    return {}


# ============================================================================
# SQL → MongoDB Converter
# ============================================================================


class SQLToMongo:
    def __init__(self):
        print("Compiling SQL grammar...", end=" ", flush=True)
        self.parser = zgram.compile(SQL_GRAMMAR)
        print("done.")

    def convert(self, sql):
        """Convert a SQL SELECT query to a MongoDB query."""
        tree = self.parser.parse(sql)  # raises ParseError on failure
        assert tree is not None

        query = tree.find("query")[0] if tree.find("query") else tree

        # Extract collection name
        from_clauses = query.find("from_clause")
        identifiers = from_clauses[0].find("identifier") if from_clauses else []
        collection = text(identifiers[0]) if identifiers else "collection"

        # Extract DISTINCT
        distinct = bool(query.find("distinct_kw"))

        # Extract fields and aggregates
        field_lists = query.find("field_list")
        fields = []
        aggregates = []
        has_star = False

        if field_lists:
            fl = field_lists[0]
            if fl.find("star"):
                has_star = True
            for fi in fl.find("field_item"):
                aggs = fi.find("aggregate")
                if aggs:
                    agg = aggs[0]
                    func = text(agg.find("agg_func")[0]).upper()
                    arg_nodes = agg.find("agg_arg")
                    arg = text(arg_nodes[0]) if arg_nodes else "*"
                    aggregates.append((func, arg))
                else:
                    ids = fi.find("identifier")
                    if ids:
                        fields.append(text(ids[0]))

        # Extract WHERE filter
        where_filter = {}
        where_clauses = query.find("where_clause")
        if where_clauses:
            exprs = where_clauses[0].find("expression")
            if exprs:
                where_filter = extract_expression(exprs[0])

        # Extract ORDER BY
        sort_spec = {}
        order_clauses = query.find("order_clause")
        if order_clauses:
            for item in order_clauses[0].find("order_item"):
                ids = item.find("identifier")
                if ids:
                    field = text(ids[0])
                    desc_nodes = item.find("desc_kw")
                    sort_spec[field] = -1 if desc_nodes else 1

        # Extract LIMIT
        limit = None
        limit_clauses = query.find("limit_clause")
        if limit_clauses:
            ints = limit_clauses[0].find("integer")
            if ints:
                limit = int(text(ints[0]))

        # Extract OFFSET
        skip = None
        offset_clauses = query.find("offset_clause")
        if offset_clauses:
            ints = offset_clauses[0].find("integer")
            if ints:
                skip = int(text(ints[0]))

        # Build MongoDB query
        if aggregates:
            return self._build_aggregate(
                collection, aggregates, fields, where_filter, sort_spec, limit, skip
            )

        if distinct and fields:
            return self._build_distinct(collection, fields[0], where_filter)

        return self._build_find(
            collection, fields, has_star, where_filter, sort_spec, limit, skip
        )

    def _build_find(
        self, collection, fields, has_star, where_filter, sort_spec, limit, skip
    ):
        """Build a db.collection.find() query."""
        projection = {}
        if not has_star and fields:
            for f in fields:
                projection[f] = 1

        parts = [f"db.{collection}.find("]
        parts.append(f"  {self._fmt(where_filter)},")
        parts.append(f"  {self._fmt(projection)}")
        parts.append(")")

        if sort_spec:
            parts.append(f".sort({self._fmt(sort_spec)})")
        if skip:
            parts.append(f".skip({skip})")
        if limit:
            parts.append(f".limit({limit})")

        return "".join(parts)

    def _build_aggregate(
        self, collection, aggregates, group_fields, where_filter, sort_spec, limit, skip
    ):
        """Build a db.collection.aggregate() pipeline."""
        pipeline = []

        if where_filter:
            pipeline.append({"$match": where_filter})

        group_id = None
        if group_fields:
            if len(group_fields) == 1:
                group_id = f"${group_fields[0]}"
            else:
                group_id = {f: f"${f}" for f in group_fields}

        group_stage: dict = {"_id": group_id}
        for func, arg in aggregates:
            if func == "COUNT":
                group_stage["count"] = {"$sum": 1}
            elif func in ("SUM", "AVG", "MIN", "MAX"):
                op = f"${func.lower()}"
                field_ref = f"${arg}" if arg != "*" else 1
                group_stage[f"{func.lower()}_{arg}"] = {op: field_ref}

        pipeline.append({"$group": group_stage})

        if sort_spec:
            pipeline.append({"$sort": sort_spec})
        if skip:
            pipeline.append({"$skip": skip})
        if limit:
            pipeline.append({"$limit": limit})

        return f"db.{collection}.aggregate({self._fmt(pipeline)})"

    def _build_distinct(self, collection, field, where_filter):
        """Build a db.collection.distinct() query."""
        if where_filter:
            return f'db.{collection}.distinct("{field}", {self._fmt(where_filter)})'
        return f'db.{collection}.distinct("{field}")'

    def _fmt(self, obj):
        """Format a Python object as MongoDB shell syntax."""
        return json.dumps(obj, indent=2, default=str)


# ============================================================================
# CLI
# ============================================================================

EXAMPLES = [
    "SELECT * FROM users",
    "SELECT name, email FROM users WHERE age > 25",
    "SELECT * FROM orders WHERE status = 'active' AND total >= 100",
    "SELECT * FROM users WHERE age BETWEEN 18 AND 65",
    "SELECT * FROM products WHERE category IN ('electronics', 'books')",
    "SELECT * FROM users WHERE name LIKE 'John%'",
    "SELECT * FROM users WHERE email IS NOT NULL",
    "SELECT * FROM logs WHERE level = 'error' ORDER BY timestamp DESC LIMIT 10",
    "SELECT DISTINCT country FROM users",
    "SELECT COUNT(*) FROM orders WHERE status = 'shipped'",
    "SELECT category, AVG(price) FROM products",
    "SELECT * FROM users WHERE (age > 18 AND active = true) OR role = 'admin'",
    "SELECT name FROM users ORDER BY created_at DESC LIMIT 20 OFFSET 40",
    "SELECT * FROM events WHERE NOT archived = true",
]


def main():
    converter = SQLToMongo()

    if len(sys.argv) > 1:
        sql = " ".join(sys.argv[1:])
        try:
            result = converter.convert(sql)
            print(result)
        except zgram.ParseError as e:
            print(f"Parse error: {e}", file=sys.stderr)
            sys.exit(1)
        return

    print("\n--- SQL to MongoDB Converter ---")
    print("Type a SQL SELECT query, or 'examples' to see demos, or 'quit' to exit.\n")

    while True:
        try:
            sql = input("sql> ").strip()
        except (EOFError, KeyboardInterrupt):
            print()
            break

        if not sql:
            continue
        if sql.lower() == "quit":
            break
        if sql.lower() == "examples":
            for ex in EXAMPLES:
                print(f"\n  SQL:   {ex}")
                try:
                    result = converter.convert(ex)
                    print(f"  Mongo: {result}")
                except zgram.ParseError as e:
                    print(f"  ERROR: {e}")
            print()
            continue

        try:
            result = converter.convert(sql)
            print(result)
        except zgram.ParseError as e:
            print(f"Parse error: {e}")
        print()


if __name__ == "__main__":
    main()
