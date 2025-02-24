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
    VAR = "var"
    FUNC = "func"
    IF = "if"
    THEN = "then"
    ELSE = "else"
    WHILE = "while"
    DO = "do"
    PRINT = "print"
    INPUT = "input"

    THIS = "this"

    RETURN = "return"


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


class Lexer:
    _tokens = list([])

    def __init__(self, file_path: str):
        try:
            with open(file_path) as file:
                lines = file.readlines()
        except OSError:
            print(f"Error occurred during reading file at location: {file_path}")
            exit(-1)

        self._tokens = []

        self._curr_token = ""
        for row, line in enumerate(lines):
            line = line.strip()
            if line == "":
                continue

            # unhandled token in previous line
            if self._curr_token != "":
                self.handle_token()

            # start new line
            self._tokens.append([])

            self._curr_column = 0
            while self._curr_column < len(line):
                char = line[self._curr_column]

                # spaces
                if char == " ":
                    self.handle_token()
                    self._curr_column += 1
                    continue

                # end of line (;) / bracket
                if char in SEPARATORS:
                    self.handle_token()
                    self._tokens[-1].append(Separator(char))
                    self._curr_column += 1
                    continue

                # assignment / comparison
                if char == "=":
                    self.handle_token()

                    if line[self._curr_column + 1] == "=":
                        self._tokens[-1].append(Operator(char + "="))
                        self._curr_column += 1
                    else:
                        self._tokens[-1].append(Operator(char))

                    self._curr_column += 1
                    continue

                # math/bool operators (+ assignment)
                if char in OPERATORS:
                    self.handle_token()

                    if line[self._curr_column + 1] == "=":
                        self._tokens[-1].append(Operator(char + "="))
                        self._curr_column += 1
                    else:
                        self._tokens[-1].append(Operator(char))

                    self._curr_column += 1
                    continue

                self._curr_token += char
                self._curr_column += 1

        # unhandled token in previous line
        if self._curr_token != "":
            self.handle_token()

    def handle_token(self):
        if self._curr_token == "":
            return

        if self._curr_token in KEYWORDS:
            self._tokens[-1].append(Keyword(self._curr_token))
        elif self._curr_token.isnumeric():
            self._tokens[-1].append(Literal(int(self._curr_token)))
        else:
            self._tokens[-1].append(Identifier(self._curr_token))

        self._curr_token = ""

    def get_tokens(self) -> list:
        return [token for tokens in self._tokens for token in tokens]


if __name__ == "__main__":
    lexer = Lexer("token_test.lmu")
    tokens = lexer.get_tokens()
    tokens
