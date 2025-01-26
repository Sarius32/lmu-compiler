from tokens import KEYWORDS, Keyword, LITERALS, Literal, Separator, SEPARATORS, OPERATORS, Operator, Identifier


def is_keyword(s: str):
    return s in KEYWORDS


def is_literal(c):
    return c in LITERALS


def is_separator(c):
    return c in SEPARATORS


def is_operator(c):
    return c in OPERATORS


class Lexer:
    def __init__(self, file_text: str):
        self.tokens = []
        text = file_text.replace("\n", " ")

        symbol = ""
        for c in text:  # go through text character by character
            # separator
            if is_separator(c):
                symbol = self._handle_separator(c, symbol)

            # operator
            elif is_operator(c):
                symbol = self._handle_separator(c, symbol)

            # literal
            elif is_literal(c):
                symbol = self._handle_literal(c, symbol)

            # character has to be analysed in context
            else:
                symbol += c

        self.tokens = [t for t in self.tokens if not (t.is_separator() and t.name == " ")]

    def _handle_separator(self, c, symbol):
        if symbol != "":  # not yet analysed symbol collected before separator
            if is_keyword(symbol):
                self.tokens.append(Keyword(symbol))
            else:
                self.tokens.append(Identifier(symbol))

        self.tokens.append(Separator(c))

        return ""

    def _handle_operator(self, c, symbol):
        if symbol != "":  # not yet analysed symbol collected before separator
            if is_keyword(symbol):
                self.tokens.append(Keyword(symbol))
            else:
                self.tokens.append(Identifier(symbol))

        self.tokens.append(Operator(c))

        return ""

    def _handle_literal(self, c, symbol):
        if symbol != "":
            return symbol + c

        if self.tokens[-1].is_literal():
            self.tokens[-1].name += c
            return ""

        self.tokens.append(Literal(c))
        return ""


if __name__ == "__main__":
    with open("test.lmu", "r") as file:
        lexer = Lexer(file.read())
        print(lexer.tokens)
