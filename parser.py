from copy import deepcopy
from enum import Enum

from lexer import Lexer
from nodes import ArgDefNode, AttributeDefNode, InstanceNode, VariableDefNode, ArgUseNode, AttrUseNode, VarUseNode, \
    InstAttrUseNode, MethodDefNode, FunctionDefNode, FuncCallNode, MethodCallNode, InstMethodCallNode, WriteNode, \
    IfThenElseNode, ForLoop, WhileNode, InputNode, AssignmentNode, ClassNode, ProgramNode, OperationNode, InvertNode, \
    ComparisonNode, ReturnNode
from tokens import Token, Identifier, Literal, Keyword, Separator, Operator
from utils import interrupt_on_error


class ParserStage(Enum):
    CLASS_DEF = 1
    FUNC_DEF = 2
    PROGRAM_DEF = 3


class Parser:
    def __init__(self, token_list: list[Token]):
        """ Expected Tokens: { class } { function_def } { prog_variable_def } { statement } """
        self._tokens = token_list
        self._tokens.reverse()
        self._curr_token = self._tokens.pop()

        self._classes = []
        self._in_stage = ParserStage.CLASS_DEF
        while self._curr_token == Keyword.CLASS:
            self._classes.append(self._class())

        self._funcs = []
        self._in_stage = ParserStage.FUNC_DEF
        while self._curr_token == Keyword.FUNC:
            self._funcs.append(self._function_def())
        if self._curr_token == Keyword.CLASS:
            interrupt_on_error("Can't define classes after function definitions")

        self._vars = []
        self._in_stage = ParserStage.PROGRAM_DEF
        while self._curr_token in [Keyword.INT, Keyword.INST]:
            self._vars.append(self._variable_def())
        if self._curr_token == Keyword.CLASS:
            interrupt_on_error("Can't define classes after variable definitions")
        if self._curr_token == Keyword.FUNC:
            interrupt_on_error("Can't define functions after variable definitions")

        self._stmts = []
        while self._curr_token:
            self._stmts.append(self._statement())

    def get_program_ast(self):
        return deepcopy(ProgramNode(self._classes, self._funcs, self._vars, self._stmts))

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

    def _class(self):
        """ Function to parse class_def tokens.
        Expected Tokens: "class" name "{" { attribute_def } { method_def } "}" """
        self._next_token()  # skip "class"
        self._check_curr_token(Identifier, "Class identifier must be of type Identifier")
        self._c_name = self._curr_token.value
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected after class identifier")
        self._next_token()  # skip "{"

        self._attrs = []
        while self._curr_token == Keyword.INT:
            self._attrs.append(self._attr_def())
        if self._curr_token == Keyword.INST:
            interrupt_on_error("Instances can't be defined as class attributes")

        self._methods = []
        while self._curr_token == Keyword.METHOD:
            self._methods.append(self._method_def())
        if self._curr_token == Keyword.FUNC:
            interrupt_on_error("Functions can't be defined in classes, use 'method' instead")
        if self._curr_token in [Keyword.INT, Keyword.INST]:
            interrupt_on_error("Can't define class attributes after definition of class methods")

        self._check_curr_token(Separator.R_CURLY, "'}' expected at the end of class definition")
        self._next_token()  # skip "}"

        return ClassNode(self._c_name, self._attrs, self._methods)

    def _attr_def(self):
        """ Function to parse attribute_def tokens.
        Expected Tokens: "int" name "=" num_expression ";" """
        self._next_token()  # skip "int"

        self._check_curr_token(Identifier, "Attribute identifier must be of type Identifier")
        name = self._curr_token.value
        if name in [a.name for a in self._attrs]:
            interrupt_on_error(f"Attribute name '{name}' was already used previously")
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
        self._m_name = self._curr_token.value
        if self._m_name in [a.name for a in self._attrs]:
            interrupt_on_error(f"Method identifier '{self._m_name}' was previously defined as an class attribute")
        if self._m_name in [m.name for m in self._methods]:
            interrupt_on_error(f"Method identifier '{self._m_name}' already used for method")
        self._next_token()

        self._check_curr_token(Separator.L_ROUND, "( expected after method identifier")
        self._next_token()  # skip "("

        self._args = self._arg_def()

        self._check_curr_token(Separator.L_CURLY, "{ expected after method identifier")
        self._next_token()  # skip "{"

        self._vars = []
        while self._curr_token == Keyword.INT:
            self._vars.append(self._variable_def())
        if self._curr_token == Keyword.INST:
            interrupt_on_error("No class instances allowed in methods")

        stmts = []
        while self._curr_token != Separator.R_CURLY:
            stmts.append(self._statement())

        self._check_curr_token(Separator.R_CURLY, "} expected at end of method")
        self._next_token()  # skip "}"
        return MethodDefNode(self._m_name, self._args, self._vars, stmts)

    def _function_def(self):
        """ Function to parse function_def tokens.
        Expected Tokens: "function" name "(" [ arg_def ] ")" "{" { variable_def } { statement } [ "return" expression ";" ] "}" """
        self._next_token()  # skip "function"
        self._check_curr_token(Identifier, "Function identifier must be of type Identifier")
        self._f_name = self._curr_token.value
        if self._f_name in [c.name for c in self._classes]:
            interrupt_on_error(f"Function identifier '{self._f_name}' already used as class identifier")
        self._next_token()

        self._check_curr_token(Separator.L_ROUND, "( expected after function identifier")
        self._next_token()  # skip "("

        self._args = self._arg_def()

        self._check_curr_token(Separator.L_CURLY, "{ expected after function identifier")
        self._next_token()  # skip "{"

        self._vars = []
        while self._curr_token == Keyword.INT:
            self._vars.append(self._variable_def())
        if self._curr_token == Keyword.INST:
            interrupt_on_error("No class instances allowed in functions")

        stmts = []
        while self._curr_token != Separator.R_CURLY:
            stmts.append(self._statement())

        self._check_curr_token(Separator.R_CURLY, "} expected at end of function")
        self._next_token()  # skip "}"
        return FunctionDefNode(self._f_name, self._args, self._vars, stmts)

    def _arg_def(self):
        """ Function to parse arg_def tokens.
        Expected Tokens: [ arg1 { "," argx } [ "," ] ] ")" """
        type_ = "function" if self._in_stage == ParserStage.FUNC_DEF else "method"
        args = []
        while True:
            if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                self._next_token()  # skip ")"
                break

            if type(self._curr_token) == Identifier:
                name = self._curr_token.value
                if self._in_stage == ParserStage.CLASS_DEF and name == self._c_name:
                    interrupt_on_error(f"Argument identifier '{name}' can't be identical to the class identifier")
                if self._in_stage == ParserStage.FUNC_DEF:
                    if name == self._f_name:
                        interrupt_on_error(f"Argument identifier '{name}' can't be identical to the {type_} identifier")
                    if name in [c.name for c in self._classes]:
                        interrupt_on_error(f"Argument identifier '{name}' can't be identical to a class identifier")
                if name in [a.name for a in args]:
                    interrupt_on_error(f"Argument identifiers '{name}' must be unique")
                args.append(ArgDefNode(name))
                self._next_token()
            elif type(self._curr_token) == Literal:
                interrupt_on_error(f"{type_} argument must be of type Identifier")

            if self._curr_token == Separator.COMMA:
                self._next_token()  # skip ","
            elif self._curr_token == Separator.R_ROUND:
                self._next_token()  # skip ")"
                break
            else:
                interrupt_on_error(f"',' or ')' expected after argument in {type_} definition")

        return args

    def _variable_def(self):
        """ Function to parse prog_variable_def/variable_def tokens.
        Expected Tokens for prog_variable_def: "int" | ( "inst" class_name ) name [ "=" ( num_expression | instance ) ] ";"
        Expected Tokens for variable_def: "int" name "=" num_expression ";" """

        type_ = "int"
        if not self._in_stage == ParserStage.PROGRAM_DEF:
            self._next_token()  # skip "int"
        else:
            if self._curr_token == Keyword.INST:
                self._next_token()  # skip "inst"

                self._check_curr_token(Identifier, "Class identifier needs to follow 'inst'")
                type_ = self._curr_token.value
                if self._curr_token.value not in [c.name for c in self._classes]:
                    interrupt_on_error(f"Class identifier '{type_}' unknown")

            self._next_token()

        self._check_curr_token(Identifier, "Variable identifier must be of type Identifier")
        name = self._curr_token.value
        if self._in_stage == ParserStage.CLASS_DEF:
            if name in [self._c_name, *[c.name for c in self._classes]]:  # other or own class name
                interrupt_on_error(f"Variable identifier '{name}' can't be identical to a class identifier")
            if name in [a.name for a in self._args]:  # arg name
                interrupt_on_error(f"Variable identifier '{name}' can't be identical to a argument identifier")

        if self._in_stage == ParserStage.FUNC_DEF:
            if name in [c.name for c in self._classes]:
                interrupt_on_error(f"Variable identifier can't be identical to a class identifier")
            if name == [self._f_name, *[f.name for f in self._funcs]]:  # other or own method name
                interrupt_on_error(f"Variable identifier '{name}' can't be identical to a function identifier")
            if name in [a.name for a in self._args]:  # arg name
                interrupt_on_error(f"Variable identifier '{name}' can't be identical to a argument identifier")

        if self._in_stage == ParserStage.PROGRAM_DEF:
            if name in [c.name for c in self._classes]:
                interrupt_on_error(f"Variable identifier '{name}' can't be identical to a class identifier")
            if name in [f.name for f in self._funcs]:
                interrupt_on_error(f"Variable identifier '{name}' can't be identical to a function identifier")

        if name in [v.name for v in self._vars]:
            interrupt_on_error(f"Variable identifier '{name}' can't be identical to another variable identifier")
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

    def _statement(self):
        """ Function to parse statement tokens.
        Expected Tokens: "print" "(" [ expression ] ")" ";" | if_statement | for_loop | while_loop | "return" expression ";" | var_or_func_call [ "=" ( "input" "(" ")" | expression ) ] ";" """
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

        if self._curr_token == Keyword.FOR:  # for loop
            return self._for_loop()

        if self._curr_token == Keyword.WHILE:  # while statement
            return self._while_loop()

        if self._curr_token == Keyword.RETURN:
            self._next_token()
            ret = 0
            if self._curr_token != Separator.SEMICOLON:
                ret = self._expression()

            self._check_curr_token(Separator.SEMICOLON, "; expected at end of return statement")
            self._next_token()  # skip ";"

            return ReturnNode(ret)

        var_or_call = self._var_or_call()

        if type(var_or_call) == ArgUseNode:
            if self._curr_token == Operator.EQUAL:
                interrupt_on_error(f"Argument '{var_or_call.name}' can't be assigned a value")
            interrupt_on_error(f"Argument '{var_or_call.name}' not used correctly")

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
                value = self._expression()

            self._check_curr_token(Separator.SEMICOLON, "';' expected after value assignment")
            self._next_token()  # skip ";"
            return AssignmentNode(var, value)

        self._check_curr_token(Separator.SEMICOLON, "';' expected after function call")
        self._next_token()  # skip ";"
        return var_or_call

    def _arg_list(self, num_args):
        """ Function to parse arg_list tokens.
        Expected Tokens: [ expression { "," expression } [ "," ] ] ")" """
        args = []
        while True:
            if self._curr_token == Separator.R_ROUND:  # enables empty arg list
                self._next_token()  # skip ")"
                break

            if len(args) < num_args:
                args.append(self._expression())
            else:
                interrupt_on_error("Found too many arguments!")

            if self._curr_token == Separator.COMMA:
                self._next_token()  # skip ","
            elif self._curr_token == Separator.R_ROUND:
                self._next_token()  # skip ")"
                break
            else:
                interrupt_on_error(", or ) expected after argument")

        if len(args) < num_args:
            interrupt_on_error("Found too few arguments")
        return args  # doesn't consume ")"

    def _if_stmt(self):
        """ Function to parse if_statement tokens
        Expected Tokens: "if" "(" condition ")" "then" "{" { statement } "}" [ "else" "{" { statement } "}" ] """
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

    def _for_loop(self):
        """ Function to parse for_loop tokens.
        Expected Tokens: "for" name "in" "(" expression "," expression ")" "do" "{" { statement } "}" """
        self._next_token()  # skip "for"

        self._check_curr_token(Identifier, "Variable identifier expected")
        loop_var = self._curr_token.value
        if loop_var not in [v.name for v in
                            self._vars]:  # can only use local var (either method, function or program var)
            interrupt_on_error(f"Loop var has to be defined!")
        self._next_token()

        self._check_curr_token(Keyword.IN, "in expected")
        self._next_token()

        self._check_curr_token(Separator.L_ROUND, "( expected")
        self._next_token()

        lower = self._expression()

        self._check_curr_token(Separator.COMMA, ", expected")
        self._next_token()

        upper = self._expression()

        self._check_curr_token(Separator.R_ROUND, ") expected")
        self._next_token()

        self._check_curr_token(Keyword.DO, "do expected")
        self._next_token()

        self._check_curr_token(Separator.L_CURLY, "{ expected")
        self._next_token()

        stmts = []
        while self._curr_token != Separator.R_CURLY:
            stmts.append(self._statement())

        self._next_token()  # skip "}"

        return ForLoop(VarUseNode(loop_var), lower, upper, stmts)

    def _while_loop(self):
        """ Function to parse while_loop tokens.
        Expected Tokens: "while" "(" condition ")" "do" "{" { statement } "}" """
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
        """ Function to parse condition tokens.
        Expected Tokens: ( "!" "(" condition ")" | expression ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) expression ) """
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

    def _expression(self, must_be_numeric=False):
        """ Function to parse expression/num_expression tokens.
        Expected Tokens for expression: [ "+" | "-" ] term { ( "+" | "-" ) term }
        Expected Tokens for expression: [ "+" | "-" ] num_term { ( "+" | "-" ) num_term } """
        if self._curr_token in [Operator.PLUS, Operator.MINUS]:
            operation = self._curr_token
            self._next_token()  # skip "+" / "-"
            right = self._term(must_be_numeric)
            left = OperationNode(0, operation, right)
        else:
            left = self._term(must_be_numeric)

        while self._curr_token in [Operator.PLUS, Operator.MINUS]:
            operation = self._curr_token
            self._next_token()  # skip "+" / "-"
            right = self._term(must_be_numeric)
            left = OperationNode(left, operation, right)  # chain after previous operation

        return left

    def _term(self, must_be_numeric: bool):
        """ Function to parse term/num_term tokens.
        Expected Tokens for term: factor { ( "*" | "/" ) factor }
        Expected Tokens for num_term: num_factor { ( "*" | "/" ) num_factor } """
        left = self._factor(must_be_numeric)

        while self._curr_token in [Operator.MULTIPLY, Operator.DIVIDE]:
            operation = self._curr_token
            self._next_token()  # skip "*" / "/"
            right = self._factor(must_be_numeric)
            left = OperationNode(left, operation, right)  # chain after previous operation

        return left

    def _factor(self, must_be_numeric: bool):
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

            expr = self._expression()

            self._check_curr_token(Separator.R_ROUND, ") expected after expression as a factor")
            self._next_token()
            return expr

        return self._var_or_call()

    def _var_or_call(self):
        """ Function to parse var_or_func_call tokens.
        Expected Tokens: [ [ name | "this" ] "." ] name [ "(" arg_list ")" ] """
        # either class attribute, class method call, variable or function call
        if self._in_stage == ParserStage.CLASS_DEF:
            if self._curr_token == Keyword.THIS:
                self._next_token()

                self._check_curr_token(Separator.DOT, "'.' expected after 'this' keyword")
                self._next_token()

                self._check_curr_token(Identifier, "Class attribute or method identifier "
                                                   "needs to follow 'this' keyword")
                name = self._curr_token.value
                self._next_token()

                if self._curr_token != Separator.L_ROUND:
                    if name not in [a.name for a in self._attrs]:
                        interrupt_on_error(f"Tried to use class attribute '{name}', not defined")
                    return AttrUseNode(name)

                if name not in [self._m_name, *[m.name for m in self._methods]]:
                    interrupt_on_error(f"Tried to use class method '{name}', not defined")
                self._next_token()  # skip "("

                num_args = len(
                    self._args if name == self._m_name else next(m.args for m in self._methods if name == m.name))
                args = self._arg_list(num_args)

                return MethodCallNode(name, args)

            # no 'this'
            self._check_curr_token(Identifier, "Variable identifier expected")
            name = self._curr_token.value
            self._next_token()

            if name not in [*[v.name for v in self._vars], *[a.name for a in self._args]]:
                interrupt_on_error(f"Tried to use method variable/argument '{name}', not defined")
            # can only be variable, since no functions defined before classes and only int vars in class methods
            return VarUseNode(name) if name in [v.name for v in self._vars] else ArgUseNode(name)

        if self._curr_token == Keyword.THIS:
            interrupt_on_error("Can't use 'this' keyword outside of class")

        # outside of class => either function def, program var def or program stmt
        self._check_curr_token(Identifier, "Variable or Function identifier expected")
        name = self._curr_token.value
        self._next_token()

        if self._in_stage == ParserStage.FUNC_DEF:  # can only be variable or function call
            if self._curr_token != Separator.L_ROUND:
                if name not in [*[v.name for v in self._vars], *[a.name for a in self._args]]:
                    interrupt_on_error(f"Tried to use method variable/argument '{name}', not defined")
                return VarUseNode(name) if name in [v.name for v in self._vars] else ArgUseNode(name)

            if name not in [self._f_name, *[f.name for f in self._funcs]]:
                interrupt_on_error(f"Tried to use function '{name}', not defined")
            self._next_token()  # skip "("

            num_args = len(self._args if name == self._f_name else next(f.args for f in self._funcs if name == f.name))
            args = self._arg_list(num_args)

            return FuncCallNode(name, args)

        # on program layer
        # dot => class instance
        if self._curr_token == Separator.DOT:
            self._next_token()  # skip "."
            instance_name = name  # found identifier is of class instance
            if instance_name not in [i.name for i in self._vars]:
                interrupt_on_error(f"Tried to use instance '{instance_name}', not defined")
            if instance_name not in [i.name for i in self._vars if i.type_ != "int"]:
                interrupt_on_error(f"Variable '{instance_name}' is of type int, doesn't provide attributes or methods")

            type_ = next(v.type_ for v in self._vars if v.name == instance_name)
            class_ = next(c for c in self._classes if c.name == type_)

            self._check_curr_token(Identifier, "Identifier of class attribute or method expected")
            name = self._curr_token.value
            self._next_token()

            if self._curr_token != Separator.L_ROUND:
                if name not in [a.name for a in class_.attrs]:
                    interrupt_on_error(f"Tried to use attribute '{name}' for instance '{instance_name}', not defined")
                return InstAttrUseNode(instance_name, name)

            if name not in [a.name for a in class_.methods]:
                interrupt_on_error(f"Tried to use method '{name}' for instance '{instance_name}', not defined")
            self._next_token()  # skip "("

            num_args = len(next(m.args for m in class_.methods if m.name == name))
            args = self._arg_list(num_args)

            return InstMethodCallNode(name, args, instance_name)
        else:  # no dot => no class instance
            if self._curr_token != Separator.L_ROUND:
                if name not in [v.name for v in self._vars]:
                    interrupt_on_error(f"Tried to use variable '{name}', not defined")
                return VarUseNode(name)

            if name not in [f.name for f in self._funcs]:
                interrupt_on_error(f"Tried to use function '{name}', not defined")
            self._next_token()  # skip "("

            num_args = len(next(f.args for f in self._funcs if f.name == name))
            args = self._arg_list(num_args)

            return FuncCallNode(name, args)


if __name__ == "__main__":
    lexer = Lexer("token_test.lmu")
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    parser
