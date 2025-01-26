from dataclasses import dataclass


@dataclass
class Token:
    name: str

    @staticmethod
    def is_identifier():
        return False

    @staticmethod
    def is_keyword():
        return False

    @staticmethod
    def is_literal():
        return False

    @staticmethod
    def is_separator():
        return False

    @staticmethod
    def is_operator():
        return False


@dataclass
class Identifier(Token):
    @staticmethod
    def is_identifier():
        return True


@dataclass
class Keyword(Token):
    @staticmethod
    def is_keyword():
        return True


@dataclass
class Literal(Token):
    @staticmethod
    def is_literal():
        return True


@dataclass
class Separator(Token):
    @staticmethod
    def is_separator():
        return True


@dataclass
class Operator(Token):
    @staticmethod
    def is_operator():
        return True


KEYWORDS = ["class", "function", "int"]
LITERALS = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."]
SEPARATORS = [" ", ";", "{", "}", "(", ")"]
OPERATORS = ["+", "-", "/", "*", "%", "="]
