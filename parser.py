from copy import deepcopy

from lexer import Lexer
from nodes import ArgDefNode, AttributeDefNode, InstanceNode, VariableDefNode, ArgUseNode, AttrUseNode, VarUseNode, \
    InstAttrUseNode, MethodDefNode, FunctionDefNode, FuncCallNode, MethodCallNode, InstMethodCallNode, WriteNode, \
    IfThenElseNode, WhileNode, InputNode, AssignmentNode, ClassNode, ProgramNode, OperationNode, InvertNode, \
    ComparisonNode
from tokens import Token, Identifier, Literal, Keyword, Separator, Operator
from utils import interrupt_on_error


class Parser:
    _curr_args: set[str] = set()

    def __init__(self, token_list: list[Token]):
        """ Expected Tokens: { class } { function_def } { prog_variable_def } { statement } """
        self._tokens = token_list
        self._tokens.reverse()
        self._curr_token = self._tokens.pop()

        classes = []
        while self._curr_token == Keyword.CLASS:
            classes.append(self._class())

        funcs = []
        while self._curr_token == Keyword.FUNC:
            funcs.append(self._function_def())
        if self._curr_token == Keyword.CLASS:
            interrupt_on_error("Can't define classes after function definitions")

        vars_ = []
        while self._curr_token == Keyword.VAR:
            vars_.append(self._variable_def(program_var=True))
        if self._curr_token == Keyword.CLASS:
            interrupt_on_error("Can't define classes after variable definitions")
        if self._curr_token == Keyword.FUNC:
            interrupt_on_error("Can't define functions after variable definitions")

        stmts = []
        while self._curr_token:
            stmts.append(self._statement())

        self._program = ProgramNode(classes, funcs, vars_, stmts)

    def get_program_ast(self):
        return deepcopy(self._program)

    def _next_token(self):
        """ Jumps to next token and returns it if available, else return None """
        self._curr_token = self._tokens.pop() if len(self._tokens) else None
        return self._curr_token

    def _check_curr_token(self, match: Token, error_msg: str):
        """ Checks the current token against a Type or defined Token, interrupts with error_msg if not matched. """
        if match in [Identifier, Literal]:
            if type(self._curr_token) == match:
                return
        else:
            if self._curr_token == match:
                return
        interrupt_on_error(error_msg)

    def _set_args(self, args: list[ArgDefNode]):
        """ Saves all argument names of the current function, used to decide if name is of variable or argument. """
        self._curr_args = {arg.name for arg in args}

    def _reset_args(self):
        """ Resets the reserved argument names set before exiting a function definition. """
        self._curr_args = set()

    def _class(self):
        """ Function to parse class_def tokens.
        Expected Tokens: "class" name "{" { attribute_def } { method_def } "}" """
        self._next_token()  # skip "class"
        self._check_curr_token(Identifier, "Class identifier must be of type Identifier")
        name = self._curr_token.value
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after class identifier")
        self._next_token()  # skip "{"

        attrs = []
        while self._curr_token == Keyword.ATTR:
            attrs.append(self._attr_def())
        if self._curr_token == Keyword.VAR:
            interrupt_on_error("Variables can't be defined in classes, use 'attr' instead")

        funcs = []
        while self._curr_token == Keyword.METHOD:
            funcs.append(self._method_def())
        if self._curr_token == Keyword.FUNC:
            interrupt_on_error("Functions can't be defined in classes, use 'method' instead")
        if self._curr_token in [Keyword.ATTR, Keyword.VAR]:
            interrupt_on_error("Can't define class attributes after definition of class methods")

        self._check_curr_token(Separator.R_CURLY, "'}' expected at the end of class definition")
        self._next_token()  # skip "}"

        return ClassNode(name, attrs, funcs)

    def _attr_def(self):
        """ Function to parse attribute_def tokens.
        Expected Tokens: "attr" "int" name "=" num_expression ";" """
        self._next_token()  # skip "attr"

        self._check_curr_token(Keyword.INT, "Class attribute has to be of type Integer, ('int') needs to follow 'attr'")
        self._next_token()

        self._check_curr_token(Identifier, "Attribute identifier must be of type Identifier")
        name = self._curr_token.value
        self._next_token()

        self._check_curr_token(Operator.EQUAL, "Attribute needs to have a value assigned "
                                               "('=' needs to follow attribute identifier)")
        self._next_token()  # skip "="

        value = self._expression(must_be_numeric=True)

        self._check_curr_token(Separator.SEMICOLON, "; expected after expression assigned to attribute")
        self._next_token()  # skip ";"
        return AttributeDefNode(name, value)

    def _method_def(self):
        """ Function to parse method_def tokens.
        Expected Tokens: "method" name "(" [ arg_def ] ")" "{" { variable_def } { statement } [ "return" expression ";" ] "}" """
        self._next_token()  # skip "method"
        self._check_curr_token(Identifier, "Method identifier must be of type Identifier")
        name = self._curr_token.value
        self._next_token()

        self._check_curr_token(Separator.L_ROUND, "( expected after method identifier")
        self._next_token()  # skip "("

        args = self._arg_def(is_function=False)
        self._set_args(args)

        self._check_curr_token(Separator.L_CURLY, "{ expected after method identifier")
        self._next_token()  # skip "{"

        vars_ = []
        while self._curr_token == Keyword.VAR:
            vars_.append(self._variable_def())

        stmts = []
        while self._curr_token not in [Separator.R_CURLY, Keyword.RETURN]:
            stmts.append(self._statement(in_method=True))

        ret = 0
        if self._curr_token == Keyword.RETURN:
            self._next_token()
            ret = self._expression(in_method=True)

            self._check_curr_token(Separator.SEMICOLON, "; expected at end of method return")
            self._next_token()  # skip ";"

        self._check_curr_token(Separator.R_CURLY, "} expected at end of method")
        self._next_token()  # skip "}"
        self._reset_args()
        return MethodDefNode(name, args, vars_, stmts, ret)

    def _function_def(self):
        """ Function to parse function_def tokens.
        Expected Tokens: "function" name "(" [ arg_def ] ")" "{" { variable_def } { statement } [ "return" expression ";" ] "}" """
        self._next_token()  # skip "function"
        self._check_curr_token(Identifier, "Function identifier must be of type Identifier")
        name = self._curr_token.value
        self._next_token()

        self._check_curr_token(Separator.L_ROUND, "( expected after function identifier")
        self._next_token()  # skip "("

        args = self._arg_def()
        self._set_args(args)

        self._check_curr_token(Separator.L_CURLY, "{ expected after function identifier")
        self._next_token()  # skip "{"

        vars_ = []
        while self._curr_token == Keyword.VAR:
            vars_.append(self._variable_def())

        stmts = []
        while self._curr_token not in [Separator.R_CURLY, Keyword.RETURN]:
            stmts.append(self._statement(in_function=True))

        ret = 0
        if self._curr_token == Keyword.RETURN:
            self._next_token()
            ret = self._expression(in_function=True)

            self._check_curr_token(Separator.SEMICOLON, "; expected at end of function return")
            self._next_token()  # skip ";"

        self._check_curr_token(Separator.R_CURLY, "} expected at end of function")
        self._next_token()  # skip "}"
        self._reset_args()
        return FunctionDefNode(name, args, vars_, stmts, ret)

    def _arg_def(self, is_function=True):
        """ Function to parse arg_def tokens.
        Expected Tokens: [ arg1 { "," argx } [ "," ] ] ")" """
        args = []
        while True:
            if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                self._next_token()  # skip ")"
                break

            if type(self._curr_token) == Identifier:
                args.append(ArgDefNode(self._curr_token.value))
                self._next_token()
            elif type(self._curr_token) == Literal:
                interrupt_on_error(f"{'Function' if is_function else 'Method'} argument must be of type Identifier")

            if self._curr_token == Separator.COMMA:
                self._next_token()  # skip ","
            elif self._curr_token == Separator.R_ROUND:
                self._next_token()  # skip ")"
                break
            else:
                interrupt_on_error("',' or ')' expected after argument in "
                                   f"{'function' if is_function else 'method'} definition")

        return args

    def _variable_def(self, program_var=False):
        """ Function to parse prog_variable_def/variable_def tokens.
        Expected Tokens for prog_variable_def: "var" [ "int" | class_name ] name "=" ( num_expression | instance ) ";"
        Expected Tokens for variable_def: "var" "int" name "=" num_expression ";" """
        self._next_token()  # skip "var"

        if not program_var:
            self._check_curr_token(Keyword.INT, "Variable type in a function or class method has to be Integer")
            type_ = "int"
            self._next_token()
        else:
            if self._curr_token == Keyword.INT:
                type_ = "int"
            elif type(self._curr_token) == Identifier:
                type_ = self._curr_token.value
            else:
                interrupt_on_error("Variable can either be an Integer or a previously defined class"
                                   "(variable type missing after 'var')")
            self._next_token()  # skip type

        self._check_curr_token(Identifier, "Variable identifier must be of type Identifier")
        name = self._curr_token.value
        self._next_token()

        if type_ == "int":
            self._check_curr_token(Operator.EQUAL, "Variable needs to have a value assigned "
                                                   "('=' needs to follow variable identifier)")
            self._next_token()  # skip "="

            value = self._expression(must_be_numeric=True)
        else:
            value = InstanceNode(type_)

        self._check_curr_token(Separator.SEMICOLON, "; expected after variable definition")
        self._next_token()  # skip ";"
        return VariableDefNode(name, value, type_)

    def _statement(self, in_method=False, in_function=False):
        """ Function to parse statement tokens.
        Expected Tokens: "print" "(" [ expression ] ")" | if_statement | while_loop | var_or_func_call [ "=" ( "input" "(" ")" | expression ) ] """
        if self._curr_token == Keyword.PRINT:  # print statement
            self._next_token()

            self._check_curr_token(Separator.L_ROUND, "( expected after print")
            self._next_token()

            expr = None
            if self._curr_token != Separator.R_ROUND:
                expr = self._expression(in_method, in_function)
                self._check_curr_token(Separator.R_ROUND, ") expected after print argument")

            self._next_token()  # skip ")"

            self._check_curr_token(Separator.SEMICOLON, "; expected after print")
            self._next_token()  # skip ";"

            return WriteNode(expr)

        if self._curr_token == Keyword.IF:  # if statement
            return self._if_stmt(in_method, in_function)

        if self._curr_token == Keyword.WHILE:  # while statement
            return self._while_loop(in_method, in_function)

        var_or_call = self._var_or_call(in_method, in_function)

        if type(var_or_call) in [AttrUseNode, VarUseNode, InstAttrUseNode]:  # no call => has to be assignment
            var = var_or_call
            self._check_curr_token(Operator.EQUAL, "'=' expected after variable")
            self._next_token()  # skip "="

            if self._curr_token == Keyword.INPUT:  # input value assigned to variable
                self._next_token()

                self._check_curr_token(Separator.L_ROUND, "( expected for input call")
                self._next_token()

                self._check_curr_token(Separator.R_ROUND, ") expected for input call")
                self._next_token()
                value = InputNode()
            else:
                value = self._expression(in_method, in_function)

            self._check_curr_token(Separator.SEMICOLON, "';' expected after value assignment")
            self._next_token()  # skip ";"
            return AssignmentNode(var, value)

        self._check_curr_token(Separator.SEMICOLON, "';' expected after function call")
        self._next_token()  # skip ";"
        return var_or_call

    def _arg_list(self, in_method: bool, in_function: bool):
        """ Function to parse arg_list tokens.
        Expected Tokens: [ expression { "," expression } [ "," ] ] ")" """
        args = []
        while True:
            if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                self._next_token()  # skip ")"
                break

            args.append(self._expression(in_method, in_function))

            if self._curr_token == Separator.COMMA:
                self._next_token()  # skip ","
            elif self._curr_token == Separator.R_ROUND:
                self._next_token()  # skip ")"
                break
            else:
                interrupt_on_error(", or ) expected after argument")

        return args  # doesn't consume ")"

    def _if_stmt(self, in_method: bool, in_function: bool):
        """ Function to parse if_statement tokens
        Expected Tokens: "if" "(" condition ")" "then" "{" { statement } "}" [ "else" "{" { statement } "}" ] """
        self._next_token()  # skip "if"
        self._check_curr_token(Separator.L_ROUND, "( expected before if condition")
        self._next_token()

        condition = self._condition(in_method, in_function)

        self._check_curr_token(Separator.R_ROUND, ") expected after if condition")
        self._next_token()

        self._check_curr_token(Keyword.THEN, "then expected after closed if condition")
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after then")
        self._next_token()

        then = []
        while self._curr_token != Separator.R_CURLY:
            then.append(self._statement(in_method, in_function))
        self._next_token()  # skip "}"

        alternative = []
        if self._curr_token == Keyword.ELSE:
            self._next_token()

            self._check_curr_token(Separator.L_CURLY, "{ expected after else")
            self._next_token()

            while self._curr_token != Separator.R_CURLY:
                alternative.append(self._statement(in_method, in_function))
            self._next_token()  # skip "}"

        return IfThenElseNode(condition, then, alternative)

    def _while_loop(self, in_method: bool, in_function: bool):
        """ Function to parse while_loop tokens.
        Expected Tokens: "while" "(" condition ")" "do" "{" { statement } "}" """
        self._next_token()  # skip "while"
        self._check_curr_token(Separator.L_ROUND, "( expected before while condition")
        self._next_token()

        condition = self._condition(in_method, in_function)

        self._check_curr_token(Separator.R_ROUND, ") expected after while condition")
        self._next_token()

        self._check_curr_token(Keyword.DO, "do expected after closed while condition")
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after do")
        self._next_token()

        stmts = []
        while self._curr_token != Separator.R_CURLY:
            stmts.append(self._statement(in_method, in_function))
        self._next_token()  # skip "}"

        return WhileNode(condition, stmts)

    def _condition(self, in_method: bool, in_function: bool):
        """ Function to parse condition tokens.
        Expected Tokens: ( "!" "(" condition ")" | expression ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) expression ) """
        if self._curr_token == Operator.NOT:  # inverted condition
            self._next_token()  # skip "!"

            self._check_curr_token(Separator.L_ROUND, "( expected at beginning of inverted condition")
            self._next_token()

            cond = self._condition(in_method, in_function)

            self._check_curr_token(Separator.R_ROUND, ") expected after inverted condition")
            self._next_token()
            return InvertNode(cond)

        left = self._expression(in_method, in_function)

        operation = self._curr_token
        if operation not in [Operator.EQUALS, Operator.NOT_EQUAL, Operator.SMALLER, Operator.SMALLER_EQ,
                             Operator.GREATER, Operator.GREATER_EQ]:
            interrupt_on_error("Invalid operation found in condition")
        self._next_token()

        right = self._expression(in_method, in_function)
        return ComparisonNode(left, operation, right)

    def _expression(self, in_method=False, in_function=False, must_be_numeric=False):
        """ Function to parse expression/num_expression tokens.
        Expected Tokens for expression: [ "+" | "-" ] term { ( "+" | "-" ) term }
        Expected Tokens for expression: [ "+" | "-" ] num_term { ( "+" | "-" ) num_term } """
        if self._curr_token in [Operator.PLUS, Operator.MINUS]:
            operation = self._curr_token
            self._next_token()  # skip "+" / "-"
            right = self._term(in_method, in_function, must_be_numeric)
            left = OperationNode(0, operation, right)
        else:
            left = self._term(in_method, in_function, must_be_numeric)

        while self._curr_token in [Operator.PLUS, Operator.MINUS]:
            operation = self._curr_token
            self._next_token()  # skip "+" / "-"
            right = self._term(in_method, in_function, must_be_numeric)
            left = OperationNode(left, operation, right)  # chain after previous operation

        return left

    def _term(self, in_method: bool, in_function: bool, must_be_numeric: bool):
        """ Function to parse term/num_term tokens.
        Expected Tokens for term: factor { ( "*" | "/" ) factor }
        Expected Tokens for num_term: num_factor { ( "*" | "/" ) num_factor } """
        left = self._factor(in_method, in_function, must_be_numeric)

        while self._curr_token in [Operator.MULTIPLY, Operator.DIVIDE]:
            operation = self._curr_token
            self._next_token()  # skip "*" / "/"
            right = self._factor(in_method, in_function, must_be_numeric)
            left = OperationNode(left, operation, right)  # chain after previous operation

        return left

    def _factor(self, in_method: bool, in_function: bool, must_be_numeric: bool):
        """ Function to parse factor/num_factor tokens.
        Expected Tokens for factor: var_or_func | NUMBER | "(" num_expression ")"
        Expected Tokens for num_factor: NUMBER | "(" num_expression ")" """
        if type(self._curr_token) == Literal:
            factor = self._curr_token
            self._next_token()
            return factor.value

        if must_be_numeric:
            interrupt_on_error("Factor needs to be numeric (no variable of function call allowed)")

        if self._curr_token == Separator.L_ROUND:
            self._next_token()  # skip "("

            expr = self._expression(in_method, in_function)

            self._check_curr_token(Separator.R_ROUND, ") expected after expression as a factor")
            self._next_token()
            return expr

        return self._var_or_call(in_method, in_function)

    def _var_or_call(self, in_method: bool, in_function: bool):
        """ Function to parse var_or_func_call tokens.
        Expected Tokens: [ [ name | "this" ] "." ] name [ "(" arg_list ")" ] """
        # either class attribute, class method call, variable or function call
        if in_method:
            if self._curr_token == Keyword.THIS:
                self._next_token()

                self._check_curr_token(Separator.DOT, "'.' expected after 'this' keyword")
                self._next_token()

                self._check_curr_token(Identifier, "Class attribute or method identifier "
                                                   "needs to follow 'this' keyword")
                name = self._curr_token.value
                self._next_token()

                if self._curr_token != Separator.L_ROUND:
                    return AttrUseNode(name)

                self._next_token()  # skip "("

                args = self._arg_list(in_method, in_function)

                return MethodCallNode(name, args)

            # no 'this'
            self._check_curr_token(Identifier, "Variable identifier expected")
            name = self._curr_token.value
            self._next_token()

            # can only be variable, since no functions defined before classes and only int vars in class methods
            return VarUseNode(name) if name not in self._curr_args else ArgUseNode(name)

        if self._curr_token == Keyword.THIS:
            interrupt_on_error("Can't use 'this' keyword outside of class")

        # outside of class => either function def, program var def or program stmt
        self._check_curr_token(Identifier, "Variable or Function identifier expected")
        name = self._curr_token.value
        self._next_token()

        if in_function:  # can only be variable or function call
            if self._curr_token != Separator.L_ROUND:
                return VarUseNode(name) if name not in self._curr_args else ArgUseNode(name)

            self._next_token()  # skip "("

            args = self._arg_list(in_method, in_function)

            return FuncCallNode(name, args)

        # on program layer
        # dot => class instance
        if self._curr_token == Separator.DOT:
            self._next_token()  # skip "."
            class_name = name  # found identifier is of class instance

            self._check_curr_token(Identifier, "Identifier of class attribute or method expected")
            name = self._curr_token.value
            self._next_token()

            if self._curr_token != Separator.L_ROUND:
                return InstAttrUseNode(class_name, name)

            self._next_token()  # skip "("

            args = self._arg_list(in_method, in_function)

            return InstMethodCallNode(class_name, name, args)

        # no dot => no class instance
        if self._curr_token != Separator.L_ROUND:
            return VarUseNode(name)

        self._next_token()  # skip "("

        args = self._arg_list(in_method, in_function)

        return FuncCallNode(name, args)


if __name__ == "__main__":
    lexer = Lexer("token_test.lmu")
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    parser
