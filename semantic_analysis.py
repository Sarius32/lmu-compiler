from lexer import Lexer
from nodes import ProgramNode
from parser import Parser


class SemanticAnalysis:
    def __init__(self, program: ProgramNode):
        self._program = program


if __name__ == "__main__":
    lexer = Lexer("token_test.lmu")
    parser = Parser(lexer.get_tokens())
    semantic = SemanticAnalysis(parser.get_program_ast())
    semantic
