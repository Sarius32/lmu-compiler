from lexer_new import Lexer, Identifier, Literal, Keyword, Separator, Operator
from nodes import ClassNode, VariableDef, VariableNode, FunctionDef, OperationNode, AssignmentNode, FunctionCallNode, \
    IfThenElseNode, WhileNode, InvertNode, ComparisonNode


def interrupt_on_error(error_msg):
    print("Error: " + error_msg)
    exit(-1)


class Parser:
    def __init__(self, tokens: list):
        self._tokens = tokens[::-1]
        self._curr_token = self._tokens.pop()

        self._nodes = []
        self._nodes.append(self._class())

    def _next_token(self):
        if len(self._tokens):
            self._curr_token = self._tokens.pop()
            return self._curr_token
        return None

    def _check_curr_token(self, match, error_msg):
        if match in [Identifier, Literal]:
            if type(self._curr_token) == match:
                return
        else:
            if self._curr_token == match:
                return
        interrupt_on_error(error_msg)

    def _skip_current_token_conditionally(self, match, error_msg):
        self._check_curr_token(match, error_msg)  # check current token => interrupts incorrect match
        return self._next_token()  # return next token

    def _skip_next_token_conditionally(self, match, error_msg):
        self._next_token()  # goto next token
        self._check_curr_token(match, error_msg)  # check next token => interrupts incorrect match
        return self._next_token()  # return token after next token

    def _class(self):
        """ Expected Tokens: "class" name "{" { ( variable_def | function_def ) } "}" """
        name_token = self._next_token()
        self._check_curr_token(Identifier, "Class identifier must be of type Identifier")
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after class identifier")
        self._next_token()  # skip "{"

        vars_, funcs = [], []
        while self._curr_token != Separator.R_CURLY:
            if self._curr_token == Keyword.VAR:
                vars_.append(self._variable_def())
            elif self._curr_token == Keyword.FUNC:
                funcs.append(self._function_def(True))
            else:
                interrupt_on_error(f"Unknown token ({self._curr_token}) found in class body")

        self._next_token()  # skip "}"
        return ClassNode(name_token.value, vars_, funcs)

    def _function_def(self, in_class=False):
        """ Expected Tokens: "func" name "(" [ arg1 { "," argx } [ "," ] ] ")" "{" [ { variable_def } ] { statement } "}" """
        name_token = self._next_token()
        self._check_curr_token(Identifier, "Function identifier must be of type Identifier")
        self._next_token()

        self._check_curr_token(Separator.L_ROUND, "( expected after function identifier")
        self._next_token()  # skip "("

        args = []
        while True:
            if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                self._next_token()  # skip ")"
                break

            if type(self._curr_token) == Identifier:
                args.append(VariableNode(self._curr_token.value))
                self._next_token()
            elif type(self._curr_token) == Literal:
                interrupt_on_error("Function argument must be of type Identifier")

            if self._curr_token == Separator.COMMA:
                self._next_token()  # skip ","
            elif self._curr_token == Separator.R_ROUND:
                self._next_token()  # skip ")"
                break
            else:
                interrupt_on_error(", or ) expected after argument in function definition")

        self._check_curr_token(Separator.L_CURLY, "{ expected after function identifier")
        self._next_token()  # skip "{"

        vars_ = []
        while self._curr_token == Keyword.VAR:
            vars_.append(self._variable_def())

        stmts = []
        while self._curr_token != Separator.R_CURLY:
            stmts.append(self._statement())

        self._next_token()  # skip "}"
        return FunctionDef(name_token.value, args, vars_, stmts)

    def _variable_def(self):
        """ Expected Tokens: "var" name [ "=" expression ] ";" """
        name_token = self._next_token()
        self._check_curr_token(Identifier, "Variable identifier must be of type Identifier")
        self._next_token()

        value = None
        if self._curr_token == Operator.EQUAL:
            self._next_token()  # skip "="
            value = self._expression()
            self._check_curr_token(Separator.SEMICOLON, "; expected after expression assigned to variable")
        else:
            self._check_curr_token(Separator.SEMICOLON, "; expected after variable definition")

        self._next_token()
        return VariableDef(name_token.value, value)

    def _statement(self):
        """ Expected Tokens: ( name ( "=" expression | "(" [ args ] ")" ) | if_statement | while_loop ) """
        if type(self._curr_token) == Identifier:
            name_token = self._curr_token
            self._next_token()

            if self._curr_token == Operator.EQUAL:  # Assignment
                self._next_token()  # skip "="

                if type(self._curr_token) == Identifier:  # could be expression => term => factor or start of function call
                    if self._tokens[-1] == Separator.L_ROUND:
                        func_name_token = self._curr_token
                        self._next_token()  # skip function name
                        self._next_token()  # skip "("

                        args = []
                        while True:
                            if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                                self._next_token()  # skip ")"
                                break

                            if type(self._curr_token) == Identifier:
                                args.append(VariableNode(self._curr_token.value))
                            elif type(self._curr_token) == Literal:
                                args.append(self._curr_token.value)
                            else:
                                interrupt_on_error("Argument in function call must be of type Identifier or Literal")
                            self._next_token()  # skip analysed argument

                            if self._curr_token == Separator.COMMA:
                                self._next_token()  # skip ","
                            elif self._curr_token == Separator.R_ROUND:
                                self._next_token()  # skip ")"
                                break
                            else:
                                interrupt_on_error(", or ) expected after argument in function call")

                        value = FunctionCallNode(func_name_token.value, args)
                    else:
                        value = self._expression()
                else:
                    value = self._expression()

                self._check_curr_token(Separator.SEMICOLON, "; expected after variable assignment")
                self._next_token()  # skip ";"
                return AssignmentNode(VariableNode(name_token.value), value)

            if self._curr_token == Separator.L_ROUND:  # Function Call
                self._next_token()  # skip "("

                args = []
                while True:
                    if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                        self._next_token()  # skip ")"
                        break

                    if type(self._curr_token) == Identifier:
                        args.append(VariableNode(self._curr_token.value))
                    elif type(self._curr_token) == Literal:
                        args.append(self._curr_token.value)
                    else:
                        interrupt_on_error("Argument in function call must be of type Identifier or Literal")
                    self._next_token()  # skip analysed argument

                    if self._curr_token == Separator.COMMA:
                        self._next_token()  # skip ","
                    elif self._curr_token == Separator.R_ROUND:
                        self._next_token()  # skip ")"
                        break
                    else:
                        interrupt_on_error(", or ) expected after argument in function call")

                self._check_curr_token(Separator.SEMICOLON, "; expected after function call")
                self._next_token()  # skip ";"
                return FunctionCallNode(name_token.value, args)

            interrupt_on_error("Identifier found without assignment or use as function call")

        if self._curr_token == Keyword.IF:
            return self._if_stmt()

        if self._curr_token == Keyword.WHILE:
            return self._while_stmt()

        interrupt_on_error("Unknown token; statement expected")

    def _if_stmt(self):
        """ Expected Tokens: "if" "(" condition ")" "then" "{" { statement } "}" [ "else" "{" { statement } "}" ] """
        self._next_token()  # skip "if"
        self._check_curr_token(Separator.L_ROUND, "( expected before if condition")
        self._next_token()

        condition = self._condition()

        self._check_curr_token(Separator.R_ROUND, ") expected after if condition")
        self._next_token()

        self._check_curr_token(Keyword.THEN, "then expected after closed if condition")
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after then")
        self._next_token()

        then = []
        while self._curr_token != Separator.R_CURLY:
            then.append(self._statement())
        self._next_token()  # skip "}"

        alternative = []
        if self._curr_token == Keyword.ELSE:
            self._next_token()

            self._check_curr_token(Separator.L_CURLY, "{ expected after else")
            self._next_token()

            while self._curr_token != Separator.R_CURLY:
                alternative.append(self._statement())
            self._next_token()  # skip "}"

        return IfThenElseNode(condition, then, alternative)

    def _while_stmt(self):
        """ Expected Tokens: "while" "(" condition ")" "do" "{" { statement } "}" """
        self._next_token()  # skip "while"
        self._check_curr_token(Separator.L_ROUND, "( expected before while condition")
        self._next_token()

        condition = self._condition()

        self._check_curr_token(Separator.R_ROUND, ") expected after while condition")
        self._next_token()

        self._check_curr_token(Keyword.DO, "do expected after closed while condition")
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after do")
        self._next_token()

        stmts = []
        while self._curr_token != Separator.R_CURLY:
            stmts.append(self._statement())
        self._next_token()  # skip "}"

        return WhileNode(condition, stmts)

    def _condition(self):
        """ Expected Tokens: ( "!" "(" condition ")" | expression ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) expression ) """
        if self._curr_token == Operator.NOT:  # inverted condition
            self._next_token()  # skip "!"

            self._check_curr_token(Separator.L_ROUND, "( expected at beginning of inverted condition")
            self._next_token()

            cond = self._condition()

            self._check_curr_token(Separator.R_ROUND, ") expected after inverted condition")
            self._next_token()
            return InvertNode(cond)

        left = self._expression()

        operation = self._curr_token
        if operation not in [Operator.EQUALS, Operator.NOT_EQUAL, Operator.SMALLER, Operator.SMALLER_EQ,
                             Operator.GREATER, Operator.GREATER_EQ]:
            interrupt_on_error("Invalid operation found in condition")
        self._next_token()

        right = self._expression()
        return ComparisonNode(left, operation, right)

    def _expression(self):
        """ Expected Tokens: [ "+" | "-" ] term { ( "+" | "-" ) term } """
        if self._curr_token in [Operator.PLUS, Operator.MINUS]:
            operation = self._curr_token
            self._next_token()  # skip "+" / "-"
            right = self._term()
            left = OperationNode(0, operation, right)
        else:
            left = self._term()

        while self._curr_token in [Operator.PLUS, Operator.MINUS]:
            operation = self._curr_token
            self._next_token()  # skip "+" / "-"
            right = self._term()
            left = OperationNode(left, operation, right)  # chain after previous operation

        return left

    def _term(self):
        """ Expected Tokens: factor { ( "*" | "/" ) factor } """
        left = self._factor()

        while self._curr_token in [Operator.MULTIPLY, Operator.DIVIDE]:
            operation = self._curr_token
            self._next_token()  # skip "*" / "/"
            right = self._factor()
            left = OperationNode(left, operation, right)  # chain after previous operation

        return left

    def _factor(self):
        """ Expected Tokens: IDENTIFIER | NUMBER | "(" expression ")" """
        if type(self._curr_token) == Identifier:
            factor = self._curr_token
            self._next_token()
            return VariableNode(factor.value)

        if type(self._curr_token) == Literal:
            factor = self._curr_token
            self._next_token()
            return factor.value

        self._check_curr_token(Separator.L_ROUND, "( expected at beginning of factor")
        self._next_token()

        expr = self._expression()

        self._check_curr_token(Separator.L_ROUND, ") expected after expression as a factor")
        self._next_token()

        return expr


if __name__ == "__main__":
    lexer = Lexer("token_test.lmu")
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    parser
