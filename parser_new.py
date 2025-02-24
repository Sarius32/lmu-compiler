from enum import Enum

from lexer_new import Lexer, Identifier, Literal, Keyword, Separator, Operator
from nodes import ClassNode, VariableDefNode, VariableNode, FunctionDefNode, OperationNode, AssignmentNode, \
    FunctionCallNode, IfThenElseNode, WhileNode, InvertNode, ComparisonNode, InputNode, WriteNode


def interrupt_on_error(error_msg):
    print("Error: " + error_msg)
    exit(-1)


class IdType(Enum):
    Class = "class"
    Attr = "attribute"
    Method = "method"
    Variable = "variable"
    Function = "function"
    FuncArg = "argument"


class Parser:
    _in_class = None
    _use_class = False
    _in_func = None

    _def_ids = dict()

    def __init__(self, tokens: list):
        """ Expected Tokens: { class } { function_def } { var_def } { statement } """
        self._tokens = tokens[::-1]
        self._curr_token = self._tokens.pop()

        self._nodes = []
        while self._curr_token == Keyword.CLASS:
            self._nodes.append(self._class())

        while self._curr_token == Keyword.FUNC:
            self._nodes.append(self._function_def())
        if self._curr_token == Keyword.CLASS:
            interrupt_on_error("Can't define classes after function definitions")

        while self._curr_token == Keyword.VAR:
            self._nodes.append(self._variable_def())
        if self._curr_token == Keyword.CLASS:
            interrupt_on_error("Can't define classes after variable definitions")
        if self._curr_token == Keyword.FUNC:
            interrupt_on_error("Can't define functions after variable definitions")

        while self._curr_token:
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

    def _enter_class(self, name_token):
        name = name_token.value
        if name in self._def_ids.keys():
            interrupt_on_error(f"Class identifier '{name}' already defined as {self._def_ids[name].value}")

        self._in_class = name
        return name

    def _exit_class(self):
        self._def_ids[self._in_class] = IdType.Class
        self._in_class = None

    def _enter_function(self, name_token):
        name = name_token.value
        if name in self._def_ids.keys():
            interrupt_on_error(
                f"Function identifier '{name_token.value}' already defined as {self._def_ids[name].value}")

        self._in_func = name
        return name

    def _exit_function(self):
        pre = (self._in_class + "::") if self._in_class else ""
        name = pre + self._in_func
        self._def_ids[name] = IdType.Method if self._in_class else IdType.Function
        self._in_func = None

    def _check_add_arg(self, name_token):
        pre = (self._in_class + "::") if self._in_class else ""
        pre += (self._in_func + "::") if self._in_func else ""
        name = pre + name_token.value
        if name in self._def_ids.keys():
            interrupt_on_error(
                f"Argument identifier '{name_token.value}' already defined as {self._def_ids[name].value}")
        self._def_ids[name] = IdType.FuncArg
        return name

    def _use_arg(self, name_token):
        pre = (self._in_class + "::") if self._in_class and self._use_class else ""
        pre += (self._in_func + "::") if self._in_func else ""
        name = pre + name_token.value
        type_ = self._def_ids.get(name, None)
        if type_ is None:
            interrupt_on_error(f"Variable '{name_token.value}' not defined previously")

        if type_ == IdType.Class:
            interrupt_on_error(f"Class '{name_token.value}' can't be used as a argument")

        return name

    def _check_add_var(self, name_token):
        pre = (self._in_class + "::") if self._in_class else ""
        pre += (self._in_func + "::") if self._in_func else ""
        name = pre + name_token.value
        if name in self._def_ids.keys():
            interrupt_on_error(
                f"Variable identifier '{name_token.value}' already defined as {self._def_ids[name].value}")
        self._def_ids[name] = IdType.Attr if self._in_class and not self._in_func else IdType.Variable
        return name

    def _use_var_attr(self, name_token, to_assign_to=False):
        pre = (self._in_class + "::") if self._in_class and (self._use_class or self._in_func) else ""
        pre += (self._in_func + "::") if self._in_func else ""
        name = pre + name_token.value
        type_ = self._def_ids.get(name, None)
        if type_ is None:
            interrupt_on_error(f"Variable '{name_token.value}' not defined previously")

        if to_assign_to:
            if type_ == IdType.FuncArg:
                interrupt_on_error(f"Argument '{name_token.value}' can't be assigned a value")
            elif type_ not in [IdType.Attr, IdType.Variable]:
                interrupt_on_error(f"{type_.value.title()} '{name_token.value}' can't be assigned a value")
        else:
            if type_ == IdType.Class:
                interrupt_on_error(f"Class '{name_token.value}' can't be used in a expression")

        return name

    def _use_func(self, name_token):
        pre = (self._in_class + "::") if self._in_class and self._use_class else ""
        name = pre + name_token.value
        type_ = self._def_ids.get(name, None)
        if type_ is None:
            interrupt_on_error(f"Function '{name_token.value}' not defined previously")

        if type_ not in [IdType.Function, IdType.Method]:
            interrupt_on_error(f"{type_.value.title()} '{name_token.value}' can't be called")

        return name

    def _class(self):
        """ Expected Tokens: "class" name "{" { variable_def } { function_def } "}" """
        name_token = self._next_token()
        self._check_curr_token(Identifier, "Class identifier must be of type Identifier")
        name = self._enter_class(name_token)
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after class identifier")
        self._next_token()  # skip "{"

        vars_ = []
        while self._curr_token == Keyword.VAR:
            vars_.append(self._variable_def())

        funcs = []
        while self._curr_token == Keyword.FUNC:
            funcs.append(self._function_def())

        if self._curr_token == Keyword.VAR:
            interrupt_on_error("Can't define class attributes after definition of class methods")

        self._check_curr_token(Separator.R_CURLY, "} expected at the end of class definition")
        self._next_token()  # skip "}"

        self._exit_class()
        return ClassNode(name, vars_, funcs)

    def _function_def(self):
        """ Expected Tokens: "func" name "(" [ arg1 { "," argx } [ "," ] ] ")" "{" { variable_def } { statement } [ "return" expression ] "}" """
        name_token = self._next_token()
        self._check_curr_token(Identifier, "Function identifier must be of type Identifier")
        name = self._enter_function(name_token)
        self._next_token()

        self._check_curr_token(Separator.L_ROUND, "( expected after function identifier")
        self._next_token()  # skip "("

        args = self._arg_def()

        self._check_curr_token(Separator.L_CURLY, "{ expected after function identifier")
        self._next_token()  # skip "{"

        vars_ = []
        while self._curr_token == Keyword.VAR:
            vars_.append(self._variable_def())

        stmts = []
        while self._curr_token not in [Separator.R_CURLY, Keyword.RETURN]:
            stmts.append(self._statement())

        ret = 0
        if self._curr_token == Keyword.RETURN:
            self._next_token()
            ret = self._expression()

            self._check_curr_token(Separator.SEMICOLON, "; expected at end of function return")
            self._next_token()  # skip ";"

        self._check_curr_token(Separator.R_CURLY, "} expected at end of function")
        self._next_token()  # skip "}"

        self._exit_function()
        return FunctionDefNode(name, args, vars_, stmts, ret)

    def _arg_def(self):
        """ Expected Tokens: [ [ var1 ] { "," varx } [ "," ] ] ")" """
        args = []
        while True:
            if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                self._next_token()  # skip ")"
                break

            if type(self._curr_token) == Identifier:
                name = self._check_add_arg(self._curr_token)
                args.append(VariableNode(name))
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
        """ Expected Tokens: "var" name "=" expression ";" """
        name_token = self._next_token()
        self._check_curr_token(Identifier, "Variable identifier must be of type Identifier")
        name = self._check_add_var(name_token)
        self._next_token()

        self._check_curr_token(Operator.EQUAL,
                               "Variable needs to have a value assigned ('=' needs to follow variable identifier)")
        self._next_token()  # skip "="

        value = self._assignment_value()

        self._check_curr_token(Separator.SEMICOLON, "; expected after expression assigned to variable")
        self._next_token()  # skip ";"
        return VariableDefNode(name, value)

    def _statement(self):
        """ Expected Tokens: "print" "(" [ expression ] ")" | if_statement | while_loop | ["this" "." ] name [ "." name ] ( "=" expression | "(" [ args ] ")" ) """
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

        self._use_class = False
        if self._curr_token == Keyword.THIS:
            if self._in_class is None:
                interrupt_on_error("Can't use keyword 'this' outside of class methods")

            self._use_class = True
            self._next_token()  # skip 'this'

            self._check_curr_token(Separator.DOT, ". expected after 'this' keyword")
            self._next_token()  # skip '.'

        if type(self._curr_token) == Identifier:
            name_token = self._curr_token
            self._next_token()

            if self._curr_token == Operator.EQUAL:  # Assignment
                name = self._use_var_attr(name_token, True)
                self._next_token()  # skip "="

                value = self._assignment_value()

                self._check_curr_token(Separator.SEMICOLON, "; expected after variable assignment")
                self._next_token()  # skip ";"
                return AssignmentNode(VariableNode(name), value)

            if self._curr_token == Separator.L_ROUND:  # Function Call
                name = self._use_func(name_token)
                self._next_token()  # skip "("

                args = self._arg_list()

                self._check_curr_token(Separator.SEMICOLON, "; expected after function call")
                self._next_token()  # skip ";"
                return FunctionCallNode(name, args)

            interrupt_on_error("Identifier found without assignment or use as function call")

        if self._use_attr:
            interrupt_on_error("Class method or attribute identifier needs to follow 'this' keyword")

        interrupt_on_error("Unknown token, statement expected")

    def _assignment_value(self):
        """ Expected Tokens: "input" "(" ")" | [ "this" "." ] name "(" [ arg_list ] ")" | expression """
        if self._curr_token == Keyword.INPUT:  # input value assigned to variable
            self._next_token()

            self._check_curr_token(Separator.L_ROUND, "( expected after input call")
            self._next_token()

            self._check_curr_token(Separator.R_ROUND, ") expected after input call")
            self._next_token()
            return InputNode()

        self._use_attr = False
        if self._curr_token == Keyword.THIS:
            if self._in_class is None:
                interrupt_on_error("Can't use keyword 'this' outside of class methods")

            self._use_attr = True
            self._next_token()  # skip 'this'

            self._check_curr_token(Separator.DOT, ". expected after 'this' keyword")
            self._next_token()  # skip '.'

        if type(self._curr_token) == Identifier:  # could be expression => term => factor or start of function call
            if self._tokens[-1] == Separator.L_ROUND:
                func_name_token = self._curr_token
                name = self._use_func(func_name_token)
                self._next_token()  # skip function name
                self._next_token()  # skip "("

                args = self._arg_list()

                value = FunctionCallNode(name, args)
            else:
                value = self._expression()

            if self._use_attr:
                interrupt_on_error("Class method or attribute identifier needs to follow 'this' keyword")

        else:
            value = self._expression()

        return value

    def _arg_list(self):
        """ Expected Tokens: [ [ var1 | number ] { "," ( varx | number ) } [ "," ] ] ")" """
        args = []
        while True:
            if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                self._next_token()  # skip ")"
                break

            if type(self._curr_token) == Identifier:
                name = self._use_arg(self._curr_token)
                args.append(VariableNode(name))
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
            name = self._use_var_attr(factor)
            self._next_token()
            return VariableNode(name)

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
