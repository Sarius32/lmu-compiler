from lexer_new import Lexer, Identifier, Literal, Keyword, Separator, Operator
from nodes import ClassNode, VariableDefNode, VariableNode, FunctionDefNode, OperationNode, AssignmentNode, \
    FunctionCallNode, IfThenElseNode, WhileNode, InvertNode, ComparisonNode, InputNode, WriteNode


def interrupt_on_error(error_msg):
    print("Error: " + error_msg)
    exit(-1)


class Parser:
    _in_class = None
    _def_funcs = set()

    def __init__(self, tokens: list):
        self._tokens = tokens[::-1]
        self._curr_token = self._tokens.pop()

        self._nodes = []
        while self._curr_token:
            if self._curr_token == Keyword.CLASS:
                self._nodes.append(self._class())
            elif self._curr_token == Keyword.VAR:
                self._nodes.append(self._variable_def())
            elif self._curr_token == Keyword.FUNC:
                self._nodes.append(self._function_def())
            else:
                self._nodes.append(self._statement())

        self._optimize()

    def _next_token(self):
        self._curr_token = self._tokens.pop() if len(self._tokens) else None
        return self._curr_token

    def _check_curr_token(self, match, error_msg):
        if match in [Identifier, Literal]:
            if type(self._curr_token) == match:
                return
        else:
            if self._curr_token == match:
                return
        interrupt_on_error(error_msg)

    def _check_new_func_def(self, name):
        prefix = (self._in_class + "::") if self._in_class else ""
        if prefix + name in self._def_funcs:
            error_msg = (f"Function '{name}' " + (f"for class '{self._in_class}' " if self._in_class else "")
                         + "was already defined previously")
            interrupt_on_error(error_msg)

    def _add_def_func(self, name):
        prefix = (self._in_class + "::") if self._in_class else ""
        self._def_funcs.add(prefix + name)

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
        name = name_token.value
        self._in_class = name
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after class identifier")
        self._next_token()  # skip "{"

        vars_, funcs = [], []
        while self._curr_token != Separator.R_CURLY:
            if self._curr_token == Keyword.VAR:
                vars_.append(self._variable_def())
            elif self._curr_token == Keyword.FUNC:
                funcs.append(self._function_def())
            else:
                interrupt_on_error(f"Unknown token ({self._curr_token}) found in class body")

        self._next_token()  # skip "}"
        self._in_class = None
        return ClassNode(name, vars_, funcs)

    def _function_def(self):
        """ Expected Tokens: "func" name "(" [ arg1 { "," argx } [ "," ] ] ")" "{" [ { variable_def } ] { statement } [ "return" expression ] "}" """
        name_token = self._next_token()
        self._check_curr_token(Identifier, "Function identifier must be of type Identifier")
        name = name_token.value
        self._check_new_func_def(name)
        self._next_token()

        self._check_curr_token(Separator.L_ROUND, "( expected after function identifier")
        self._next_token()  # skip "("

        args = self._arg_def()

        self._check_curr_token(Separator.L_CURLY, "{ expected after function identifier")
        self._next_token()  # skip "{"

        vars_ = []
        while self._curr_token == Keyword.VAR:
            new_var = self._variable_def()
            vars_.append(new_var)

        stmts = []
        while self._curr_token not in [Separator.R_CURLY, Keyword.RETURN]:
            stmts.append(self._statement())

        ret = None
        if self._curr_token == Keyword.RETURN:
            self._next_token()
            ret = self._expression()

            self._check_curr_token(Separator.SEMICOLON, "; expected at end of function return")
            self._next_token()  # skip ";"

        self._check_curr_token(Separator.R_CURLY, "} expected at end of function")
        self._next_token()  # skip "}"

        self._add_def_func(name)
        return FunctionDefNode(name, args, vars_, stmts, ret)

    def _arg_def(self):
        """ Expected Tokens: [ [ var1 ] { "," varx } [ "," ] ] ")" """
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

        return args

    def _variable_def(self):
        """ Expected Tokens: "var" name [ "=" expression ] ";" """
        name_token = self._next_token()
        self._check_curr_token(Identifier, "Variable identifier must be of type Identifier")
        name = name_token.value
        self._next_token()

        value = None
        if self._curr_token == Operator.EQUAL:
            self._next_token()  # skip "="

            value = self._assignment_value()

            self._check_curr_token(Separator.SEMICOLON, "; expected after expression assigned to variable")
        else:
            self._check_curr_token(Separator.SEMICOLON, "; expected after variable definition")

        self._next_token()
        return VariableDefNode(name, value)

    def _statement(self):
        """ Expected Tokens: "print" "(" [ expression ] ")" | if_statement | while_loop | name [ "." name ] ( "=" expression | "(" [ args ] ")" ) """
        if self._curr_token == Keyword.PRINT:  # print statement
            self._next_token()

            self._check_curr_token(Separator.L_ROUND, "( expected after print")
            self._next_token()

            expr = None
            if self._curr_token != Separator.R_ROUND:
                expr = self._expression()
                self._check_curr_token(Separator.R_ROUND, ") expected after print argument")

            self._next_token()  # skip ")"

            self._check_curr_token(Separator.SEMICOLON, "; expected after print")
            self._next_token()  # skip ";"

            return WriteNode(expr)

        if self._curr_token == Keyword.IF:  # if statement
            return self._if_stmt()

        if self._curr_token == Keyword.WHILE:  # while statement
            return self._while_stmt()

        if type(self._curr_token) == Identifier:
            name_token = self._curr_token
            self._next_token()

            if self._curr_token == Operator.EQUAL:  # Assignment
                self._next_token()  # skip "="

                value = self._assignment_value()

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

        interrupt_on_error("Unknown token, statement expected")

    def _assignment_value(self):
        """ Expected Tokens: "input" "(" ")" | name "(" [ arg_list ] ")" | expression """
        if self._curr_token == Keyword.INPUT:  # input value assigned to variable
            self._next_token()

            self._check_curr_token(Separator.L_ROUND, "( expected after input call")
            self._next_token()

            self._check_curr_token(Separator.R_ROUND, ") expected after input call")
            self._next_token()
            return InputNode()

        if type(self._curr_token) == Identifier:  # could be expression => term => factor or start of function call
            if self._tokens[-1] == Separator.L_ROUND:
                func_name_token = self._curr_token
                self._next_token()  # skip function name
                self._next_token()  # skip "("

                args = self._arg_list()

                value = FunctionCallNode(func_name_token.value, args)
            else:
                value = self._expression()
        else:
            value = self._expression()

        return value

    def _arg_list(self):
        """ Expected Tokens: [ [ ( var1 | number ) ] { "," ( varx | number ) } [ "," ] ] ")" """
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

        return args

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

        self._check_curr_token(Separator.R_ROUND, ") expected after expression as a factor")
        self._next_token()

        return expr

    def _optimize(self):
        nodes = []
        for node in self._nodes:
            new_node = node.optimized()
            nodes.append(new_node) if new_node else None  # don't append if stmt collapsed into no statements
        self._nodes = nodes


if __name__ == "__main__":
    lexer = Lexer("token_test.lmu")
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    parser
