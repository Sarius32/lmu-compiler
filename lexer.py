from copy import deepcopy

from tokens import Token, Identifier, Literal, Keyword, Separator, Operator, KEYWORDS, SEPARATORS, OPERATORS


class Lexer:
    _tokens: list[Token]

    def __init__(self, file_path: str):
        try:
            with open(file_path) as file:
                lines = file.readlines()
        except OSError:
            print(f"Error occurred during reading file at location: {file_path}")
            exit(-1)

        self._tokens = []

        self._curr_symbol = ""
        for row, line in enumerate(lines):
            line = line.strip()
            if line == "":
                continue

            # unhandled symbol in previous line
            if self._curr_symbol != "":
                self._handle_symbol()

            self._curr_column = 0
            while self._curr_column < len(line):
                char = line[self._curr_column]

                if char == "#":
                    self._handle_symbol()
                    break

                # spaces
                if char == " ":
                    self._handle_symbol()
                    self._curr_column += 1
                    continue

                # end of line (;) / bracket
                if char in SEPARATORS:
                    self._handle_symbol()
                    self._tokens.append(Separator(char))
                    self._curr_column += 1
                    continue

                # assignment / comparison
                if char == "=":
                    self._handle_symbol()

                    if line[self._curr_column + 1] == "=":
                        self._tokens.append(Operator(char + "="))
                        self._curr_column += 1
                    else:
                        self._tokens.append(Operator(char))

                    self._curr_column += 1
                    continue

                # math/bool operators (+ assignment)
                if char in OPERATORS:
                    self._handle_symbol()

                    if self._curr_column + 1 < len(line) and line[self._curr_column + 1] == "=":
                        self._tokens.append(Operator(char + "="))
                        self._curr_column += 1
                    else:
                        self._tokens.append(Operator(char))

                    self._curr_column += 1
                    continue

                self._curr_symbol += char
                self._curr_column += 1

        # unhandled token in previous line
        if self._curr_symbol != "":
            self._handle_symbol()

    def _handle_symbol(self):
        if self._curr_symbol == "":
            return

        if self._curr_symbol in KEYWORDS:
            self._tokens.append(Keyword(self._curr_symbol))
        elif self._curr_symbol.isnumeric():
            self._tokens.append(Literal(int(self._curr_symbol)))
        else:
            self._tokens.append(Identifier(self._curr_symbol))

        self._curr_symbol = ""

    def get_tokens(self) -> list:
        return deepcopy(self._tokens)


if __name__ == "__main__":
    lexer = Lexer("token_test.lmu")
    tokens = lexer.get_tokens()
    tokens
