"""tiny: a small language built on zgram.

Functions, if/while/break/return, variables and expressions. The grammar
builds the AST directly (labels, `@left` folding, `-> Class` actions); a
tree-walking interpreter runs it. Syntax and runtime errors are both
`zgram.Diagnostic`s.

    python tiny.py program.tiny
"""

import sys
from dataclasses import dataclass

import zgram

GRAMMAR = r"""
program     = ws (body:stmt ws)*                                      -> Program
@silent stmt = funcdef | while_stmt | if_stmt | return_stmt | break_stmt
             | let_stmt | assign | expr_stmt
funcdef     = 'fn' kw ws name:ident ws '(' ws (params:ident (ws ',' ws params:ident)*)? ws ')' ws body:block  -> FuncDef
block       = '{' ws (stmt ws)* '}'                                   -> list
while_stmt  = 'while' kw ws cond:expr ws body:block                   -> While
if_stmt     = 'if' kw ws cond:expr ws then:block (ws 'else' kw ws else_:block)?  -> If
return_stmt = 'return' kw (ws value:expr)? ws ';'                     -> Return
break_stmt  = 'break' kw ws ';'                                       -> Break()
let_stmt    = 'let' kw ws name:ident ws '=' ws value:expr ws ';'      -> Let
assign      = name:ident ws '=' !'=' ws value:expr ws ';'             -> Assign
@silent expr_stmt = expr ws ';'

# Errors name the outermost rule that failed where it started, by its display
# name if it has one: "expected expression", "expected operator or ';'".
@left expr "expression" = left:sum (ws op:cmpop ws right:sum)?        -> BinOp
@left sum  "expression" = left:term (ws op:addop ws right:term)*      -> BinOp
@left term "expression" = left:operand (ws op:mulop ws right:operand)*  -> BinOp
@silent operand = neg | primary
neg         = '-' ws operand:operand                                  -> Neg
@silent primary = number | string | call | ident | '(' ws expr ws ')'
call        = name:ident ws '(' ws (args:expr (ws ',' ws args:expr)*)? ws ')'  -> Call

number      = [0-9]+ ('.' [0-9]+)?                                    -> float
string      = '"' ('\\' . | [^"\\])* '"'                              -> unquote
ident "name"       = !keyword [a-zA-Z_] [a-zA-Z0-9_]*                 -> Name
cmpop "operator"   = '==' | '!=' | '<=' | '>=' | '<' | '>'            -> str
addop "operator"   = [+\-]                                            -> str
mulop "operator"   = [*/%]                                            -> str

@silent keyword = ('fn' | 'while' | 'if' | 'else' | 'return' | 'break' | 'let') kw
@silent kw      = ![a-zA-Z0-9_]
@silent ws      = ([ \t\n\r] | '#' [^\n]*)*
"""


# ── AST ──


@dataclass
class Program:
    body: list


@dataclass
class FuncDef:
    name: "Name"
    params: list
    body: list


@dataclass
class While:
    cond: object
    body: list


@dataclass
class If:
    cond: object
    then: list
    else_: list | None


@dataclass
class Return:
    value: object


@dataclass
class Break:
    pass


@dataclass
class Let:
    name: "Name"
    value: object


@dataclass
class Assign:
    name: "Name"
    value: object


@dataclass
class BinOp:
    left: object
    op: str
    right: object


@dataclass
class Neg:
    operand: object


@dataclass
class Call:
    name: "Name"
    args: list


@dataclass
class Name:
    text: str


PARSER = zgram.compile(GRAMMAR, ast=sys.modules[__name__])


def parse(source):
    """Source text -> Program. Raises zgram.ParseError."""
    return PARSER.parse_ast(source)


# ── Interpreter ──


class TinyError(Exception):
    """A runtime error; `.diagnostic` says where."""

    def __init__(self, node, code, message):
        span = getattr(node, "__zspan__", (0, 0))
        self.diagnostic = zgram.Diagnostic("error", code, message, span)
        super().__init__(message)


class _Break(Exception):
    pass


class _Return(Exception):
    def __init__(self, value):
        self.value = value


OPERATORS = {
    "+": lambda a, b: a + b,
    "-": lambda a, b: a - b,
    "*": lambda a, b: a * b,
    "/": lambda a, b: a / b,
    "%": lambda a, b: a % b,
    "==": lambda a, b: a == b,
    "!=": lambda a, b: a != b,
    "<": lambda a, b: a < b,
    "<=": lambda a, b: a <= b,
    ">": lambda a, b: a > b,
    ">=": lambda a, b: a >= b,
}


class Interpreter:
    def __init__(self, output=print):
        self.functions = {}
        self.builtins = {"print": lambda *args: output(*[format_value(a) for a in args])}
        self.scopes = [{}]

    def run(self, source):
        program = parse(source)
        # Functions are visible before their definition
        for stmt in program.body:
            if isinstance(stmt, FuncDef):
                self.functions[stmt.name.text] = stmt
        try:
            self.block(program.body)
        except _Break as e:
            raise TinyError(e.args[0], "break-outside-loop", "'break' outside loop") from None
        except _Return as e:
            raise TinyError(e.args[1], "return-outside-function", "'return' outside function") from None

    def block(self, stmts):
        for stmt in stmts:
            self.exec(stmt)

    def lookup(self, name):
        for scope in reversed(self.scopes):
            if name.text in scope:
                return scope
        raise TinyError(name, "undefined-name", f"undefined name '{name.text}'")

    def exec(self, node):
        match node:
            case FuncDef():
                pass
            case Let(name, value):
                self.scopes[-1][name.text] = self.eval(value)
            case Assign(name, value):
                self.lookup(name)[name.text] = self.eval(value)
            case If(cond, then, else_):
                if self.eval(cond):
                    self.block(then)
                elif else_ is not None:
                    self.block(else_)
            case While(cond, body):
                try:
                    while self.eval(cond):
                        self.block(body)
                except _Break:
                    pass
            case Break():
                raise _Break(node)
            case Return(value):
                e = _Return(None if value is None else self.eval(value))
                e.args = (e.value, node)
                raise e
            case _:
                self.eval(node)

    def eval(self, node):
        match node:
            case float() | str():  # literals: built by `-> float` and `-> unquote`
                return node
            case Name():
                return self.lookup(node)[node.text]
            case Neg(operand):
                return -self.number(operand)
            case BinOp(left, op, right):
                a, b = self.eval(left), self.eval(right)
                try:
                    return OPERATORS[op](a, b)
                except ZeroDivisionError:
                    raise TinyError(node, "division-by-zero", "division by zero") from None
                except TypeError:
                    raise TinyError(node, "type-mismatch", f"cannot apply '{op}' to {type_name(a)} and {type_name(b)}") from None
            case Call(name, args):
                return self.call(node, name, [self.eval(a) for a in args])
        raise TinyError(node, "internal", f"cannot evaluate {type(node).__name__}")

    def number(self, node):
        value = self.eval(node)
        if not isinstance(value, float) or isinstance(value, bool):
            raise TinyError(node, "type-mismatch", f"expected a number, got {type_name(value)}")
        return value

    def call(self, node, name, args):
        if name.text in self.builtins:
            return self.builtins[name.text](*args)
        func = self.functions.get(name.text)
        if func is None:
            raise TinyError(name, "undefined-function", f"undefined function '{name.text}'")
        if len(args) != len(func.params):
            raise TinyError(node, "arity", f"{name.text}() takes {len(func.params)} arguments, got {len(args)}")
        saved, self.scopes = self.scopes, [self.scopes[0], {p.text: a for p, a in zip(func.params, args)}]
        try:
            self.block(func.body)
        except _Return as e:
            return e.value
        except _Break as e:
            raise TinyError(e.args[0], "break-outside-loop", "'break' outside loop") from None
        finally:
            self.scopes = saved
        return None


def type_name(value):
    return {float: "number", str: "string", bool: "boolean", type(None): "nothing"}.get(type(value), type(value).__name__)


def format_value(value):
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, float) and value.is_integer():
        return str(int(value))
    return "nothing" if value is None else str(value)


def run(source, filename="<tiny>", output=print, errors=None):
    """Run a program. Returns True, or False after printing a diagnostic."""
    try:
        Interpreter(output).run(source)
        return True
    except (zgram.ParseError, TinyError) as e:
        (errors or (lambda text: print(text, file=sys.stderr)))(e.diagnostic.render(source, filename))
        return False


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit("usage: python tiny.py program.tiny")
    with open(sys.argv[1], encoding="utf-8") as f:
        sys.exit(0 if run(f.read(), sys.argv[1]) else 1)
