from dataclasses import dataclass
from enum import Enum


@dataclass
class Identifier:
    value: str


@dataclass
class Literal:
    value: int


class Keyword(Enum):
    CLASS = "class"
    THIS = "this"

    INT = "int"
    INST = "inst"

    FUNC = "function"
    METHOD = "method"
    RETURN = "return"

    IF = "if"
    THEN = "then"
    ELSE = "else"
    FOR = "for"
    IN = "in"
    WHILE = "while"
    DO = "do"

    PRINT = "print"
    INPUT = "input"


KEYWORDS = [kw.value for kw in Keyword]


class Separator(Enum):
    DOT = "."
    COMMA = ","
    SEMICOLON = ";"
    L_ROUND = "("
    R_ROUND = ")"
    L_CURLY = "{"
    R_CURLY = "}"
    L_SQUARE = "["
    R_SQUARE = "]"


SEPARATORS = [s.value for s in Separator]


class Operator(Enum):
    EQUAL = "="
    PLUS = "+"
    MINUS = "-"
    MULTIPLY = "*"
    DIVIDE = "/"

    EQUALS = "=="
    SMALLER = "<"
    SMALLER_EQ = "<="
    GREATER = ">"
    GREATER_EQ = ">="
    NOT_EQUAL = "!="
    NOT = "!"


OPERATORS = [o.value[0] for o in Operator]

Token = Identifier | Literal | Keyword | Separator | Operator
