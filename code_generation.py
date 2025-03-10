from copy import deepcopy
from enum import Enum

from lexer import Lexer
from parser import Parser
from nodes import ArgDefNode, AttributeDefNode, ForLoop, InstanceNode, ReturnNode, VariableDefNode, ArgUseNode, AttrUseNode, VarUseNode, \
    InstAttrUseNode, MethodDefNode, FunctionDefNode, FuncCallNode, MethodCallNode, InstMethodCallNode, WriteNode, \
    IfThenElseNode, WhileNode, InputNode, AssignmentNode, ClassNode, ProgramNode, OperationNode, InvertNode, \
    ComparisonNode, Expression, Statement, UseNode, CallNode, is_expression, is_useNode
from tokens import Operator

from variable import MethodStore, ProgramStore, TypeStore, VariableStore


        
def convert_operator(op: Operator) -> tuple[str, bool]:
    match op:
        case Operator.EQUALS:
            return ("=", False)
        case Operator.SMALLER:
            return ("<", False)
        case Operator.SMALLER_EQ:
            return (">", True)
        case Operator.GREATER:
            return (">", False)
        case Operator.GREATER_EQ:
            return ("<", True)
        case Operator.NOT_EQUAL:
            return ("=", True)
        case _:
            raise ValueError(f"Unexpected value: {op.name} of Operator")
        

#Instructions
RESET = "RST"
LOAD = "LOD"
STORE = "STO"
INCREMENT = "INC"
LITERAL = "LIT"
JUMP = "JMP"
JMP_ON_TRUE = "JOT"
JMP_ON_FALSE = "JOF"
CALL = "CAL"
RETURN = "RET"
OPERATOR = "OPR"
READ = "REA"
WRITE = "WRI"
HALT = "HLT"


class CodeGenerator:
    
    def __init__(self, syntax_tree: ProgramNode, generate_comments: bool = False):
        #Instructions for abstract machine
        self._code = list[tuple[str, list[str]]]()

        #Syntaxtree from parsing
        self._syntax_tree = syntax_tree
        self._generate_comments = generate_comments

        self._program_tables = ProgramStore(dict[str, TypeStore](), dict[str, VariableStore](), dict[str, MethodStore]())

        self._context_var_dict = dict[str, VariableStore]() #For context of method or function calls
        self._context_type = str()
        self._context_method_func = str()

        self._return_adress = 0
        #self._current_base = 0
        
        ###
        #Initialize Type INT
        type_int = TypeStore("int", 1, {}, {})
        self._program_tables.types[type_int.name] = type_int

        #Init Program
        self._add_instruction_line(RESET, [])
        #Jump to adress of Main Program
        main_prog_adress = self._add_instruction_line(JUMP, [])

        #Define Classes of Program
        self._define_classes(self._syntax_tree.classes)

        #Define Functions of Program
        self._define_functions(self._syntax_tree.funcs)

        ### Main Program starts ###
        main_prog_adress.append(self._get_line_number())
        if generate_comments:
            main_prog_adress.append("# JUMP to MAIN")
            self._add_instruction_line(INCREMENT, [0, "# MAIN: [instruction can be deleted]"])

        #declare variables of Main program
        self._declare_variables(self._syntax_tree.vars_, self._program_tables.variables)
        
        #generate statments of Main
        self._context_type = "/MAIN"
        self._handle_multiple_statements(self._syntax_tree.stmts)

        #End execution
        self._add_instruction_line(HALT, [])

#region VARIABLES

    #region general (declare, assign, write)

    def _declare_variables(self, var_list: list[VariableDefNode], store_var: dict[str, VariableStore]):
        
        #Make Space vor Variables
        inc_variable_space = self._add_instruction_line(INCREMENT, [])

        #Variable Storage after head -> t = 2
        stack_pointer = 2
        vars_comulated_size = 0
        
        for  _, variable in enumerate(var_list):
            type_ = self._program_tables.types[variable.type_]
            vars_comulated_size += type_.size

            start_adress = stack_pointer+1
            variable_store = VariableStore(variable.name, start_adress, 0, type_.name)
            store_var[variable_store.name] = variable_store

            match variable.value:
                case int():
                    self._add_instruction_line(LITERAL, [variable.value])
                case InstanceNode(): 
                    self._instanciate(variable.value)
                case OperationNode():
                    self._handle_operation(variable.value)
                case _:
                    raise TypeError(f"Unexpected type: {type(variable.value)}")
            
            #Store  values at right place
            for i in range(type_.size):
                comment_ = self._add_instruction_line(STORE, [0, start_adress+i]) # """TO DO BASE"""
                if self._generate_comments:
                    comment_.append(f"# VAR: {type_.name} {i}_{variable.name}")
        
            stack_pointer += type_.size

        #Make right space for all variables at stack
        inc_variable_space.append(vars_comulated_size)

    def _handle_assignment(self, var: UseNode, value: Expression | InputNode):
        match value:
            case _ if is_expression(value):
                self._handle_expression(value)
            case InputNode():
                self._add_instruction_line(READ, [])
            case _:
                raise TypeError(f"Unexpected type: {type(value)} for value of assignment")

        self._handle_use_node_store(var)

    
    def _handle_write(self, write_node: WriteNode):
        self._handle_expression(write_node.expr)
        self._add_instruction_line(WRITE, [])

    #endregion

    #region LOAD and STORE UseNodes
        #region general

    def _handle_use_node_load(self, use_node: UseNode):
        match use_node:
            case VarUseNode():
                self._load_var(use_node)
            case ArgUseNode():
                self._load_arg(use_node)
            case AttrUseNode():
                self._load_attribute(use_node)
            case InstAttrUseNode():
                self._load_inst_attribute(use_node)
            case _:
                raise TypeError(f"Unexpected type: {type(use_node)} in Handle use node")


    def _handle_use_node_store(self, use_node: UseNode):
        match use_node:
            case VarUseNode():
                self._store_var(use_node)
            case ArgUseNode():
                self._store_arg(use_node)
            case AttrUseNode():
                self._store_attribute(use_node)
            case InstAttrUseNode():
                self._store_inst_attribute(use_node)
            case _:
                raise TypeError(f"Unexpected type: {type(use_node)} in Handle use node")
            
        #endregion


        #region LOAD

    def _load_var(self, variable: VarUseNode):
        #this loads only primitive variables (size = 1)

        #Check if variable is defined in current context
        if variable.name in self._context_var_dict:
            var = self._context_var_dict[variable.name]
        
        #Else pick global variable
        else:
            var = self._program_tables.variables[variable.name]

        address = var.address
        self._add_instruction_line(LOAD, [0, address]) #base = 0 (no global variables)
            

    def _load_arg(self, arg: ArgUseNode):
        #Used by functions and methods

        #Get type of argument
        if self._context_type == "/FUNC":
            method_func_store = self._program_tables.functions[self._context_method_func]
        else:
            type_store = self._program_tables.types[self._context_type]
            method_func_store = type_store.methods[self._context_method_func]
        
        #Load argument
        (offset, type_) = method_func_store.arguments[arg.name]

        address = offset #Argument offset alread computed (as negative int)
        self._add_instruction_line(LOAD, [0, address]) #base = 0 (no global variables)


    def _load_attribute(self, attribute: AttrUseNode):
        #Used by methods if they use a attribute of the class

        type_store = self._program_tables.types[self._context_type]
        variable_store = type_store.instance_vars[attribute.name]

        address = -variable_store.address-1
        self._add_instruction_line(LOAD, [0, address]) #base = 0 (no global variables)


    def _load_inst_attribute(self, inst_attribute: InstAttrUseNode):
        #Used by main program

        variable = self._program_tables.variables[inst_attribute.class_name]
        attribute_name = inst_attribute.name

        variable_type = self._program_tables.types[variable.type_] #Get type of variable
        variable_store = variable_type.instance_vars[attribute_name]

        address = variable.address + variable_store.address
        self._add_instruction_line(LOAD, [0, address]) #base = 0 (no global variables)

        #endregion


        #region STORE

    def _store_var(self, variable: VarUseNode):
        #this stores only primitive variables (size = 1)

        #Check if variable is defined in current context
        if variable.name in self._context_var_dict:
            var = self._context_var_dict[variable.name]
        
        #Else pick global variable
        else:
            var = self._program_tables.variables[variable.name]

        address = var.address
        self._add_instruction_line(STORE, [0, address]) #base = 0 (no global variables)


    def _store_arg(self, arg: ArgUseNode):
        #Used by functions and methods

        #Get type of argument
        if self._context_type == "/FUNC":
            method_func_store = self._program_tables.functions[self._context_method_func]
        else:
            type_store = self._program_tables.types[self._context_type]
            method_func_store = type_store.methods[self._context_method_func]
        
        #Load argument
        (offset, type_) = method_func_store.arguments[arg.name]

        address = offset #Argument offset alread computed (as negative int)
        self._add_instruction_line(STORE, [0, address]) #base = 0 (no global variables)


    def _store_attribute(self, attribute: AttrUseNode):
        #Used by methods if they store a attribute of the class

        type_store = self._program_tables.types[self._context_type]
        variable_store = type_store.instance_vars[attribute.name]

        address = -variable_store.address-1
        self._add_instruction_line(STORE, [0, address]) #base = 0 (no global variables)
            

    def _store_inst_attribute(self, inst_attribute: InstAttrUseNode):
        #Used by main program

        variable = self._program_tables.variables[inst_attribute.class_name]
        attribute_name = inst_attribute.name

        variable_type = self._program_tables.types[variable.type_] #Get type of variable
        variable_store = variable_type.instance_vars[attribute_name]

        address = variable.address + variable_store.address
        self._add_instruction_line(STORE, [0, address]) #base = 0 (no global variables)

        #endregion
    #endregion
#endregion
            
    def _handle_call_node(self, call_node: CallNode):
        match call_node:
            case InstMethodCallNode() | MethodCallNode():
                self._general_method_call(call_node)
            # case MethodCallNode():
            #     self._general_method_call(call_node)
            case FuncCallNode():
                self._function_call(call_node)
            case _:
                raise TypeError(f"Unexpected type: {type(call_node)} for a Call node")

#region FUNCTION handeling
    def _define_functions(self, function_list: list[FunctionDefNode]):

        for _, function in enumerate(function_list):
            self._handle_function_def(function)


    def _handle_function_def(self, func_node: FunctionDefNode):
        functions = self._program_tables.functions

        line = self._get_line_number()
        f_name = func_node.name
        if self._generate_comments:
            self._add_instruction_line(INCREMENT, [0 ,"# FUNCTION: ", f_name, " [instruction can be deleted]"])

        f_args = dict[str, tuple[int, str]]() #offset, type
        var_store = dict[str, VariableStore]() #Variables of Function
        functions[f_name] = MethodStore(f_name, line, f_args, var_store)

        self._context_type = "/FUNC" #Handle functions not classes
        self._context_method_func = f_name

        #Args are only ints! ArgDefNodes are untyped
        #First arg is last on stack:  Bsp. Stack... MAIN... parameter3, parameter2, parameter1, Dynamic,RET,B, ...Method...
        arg_offs = 0

        #Arguments
        for _, arg in enumerate(func_node.args):
            arg_offs -= 1 #-1 for int
            f_args[arg.name] = (arg_offs, "int")

        #Return Value (only int)
        self._return_adress = arg_offs - 1

        self._declare_variables(func_node.vars_, var_store)

        self._context_var_dict = var_store
        self._handle_multiple_statements(func_node.stmts)


    def _function_call(self, call_node : FuncCallNode):
        #1. Add return_value storage
        self._add_instruction_line(INCREMENT, [1]) #change for bigger types

        #2. Push Parameters on stack, reversed
        arg_count = 0
        for param in call_node.args[::-1]:
            arg_count += 1
            self._handle_expression(param)
        
        #3. Call Function
        func = self._program_tables.functions[call_node.name]
        self._add_instruction_line(CALL, [0, func.adress])

        #4. CleanUp after Return delete Arg copies
        self._add_instruction_line(INCREMENT, [-arg_count])

#endregion

#region CLASS handeling
    def _define_classes(self, classes_list: list[ClassNode]):
        for _, class_ in enumerate(classes_list):
            self._handle_class(class_)

    def _handle_class(self, class_node: ClassNode):
        attributes = dict[str, VariableStore]()
        line = self._get_line_number()

        if self._generate_comments:
            self._add_instruction_line(INCREMENT, [0 , "# ", class_node.name, " : CONSTRUCTOR [instruction can be deleted]"])


        #Parameters have to be on stack B -1
        #For Methods of class
        methods_dict = dict[str, MethodStore]()

        #Constructor: AttributeDefNode can consist of Operations
        size = 0

        method_name = ""
        methods_dict[method_name] = MethodStore(method_name, line, {}, {})
        for _, attribute in enumerate(class_node.attrs):
            attributes[attribute.name] = VariableStore(attribute.name, size, 0, "int")
            size += 1 #CHANGE if you allow Instances as attributes
            
            match attribute.value:
                case int():
                    self._add_instruction_line(LITERAL, [attribute.value])
                case OperationNode():
                    self._handle_operation(attribute.value)
                case _:
                    raise TypeError(f"Unexpected type: {type(attribute.value)} in Class attribute")
            
            c_ = self._add_instruction_line(STORE, [0, -size]) 
            if self._generate_comments:
                c_.append(f"# ATTRIBUTE: {attribute.name}")
        self._add_instruction_line(RETURN, [])
        

        self._context_type = class_node.name #Current type (for method defs)

        type_class = TypeStore(class_node.name, size, attributes, methods_dict)
        self._program_tables.types[type_class.name] = type_class

        for method in class_node.methods:
            self._handle_method(method, methods_dict)


    def _handle_method(self, method: MethodDefNode, methods_dict: dict[str, MethodStore]) -> MethodStore:

        line = self._get_line_number() #Call adress
        if self._generate_comments:
            self._add_instruction_line(INCREMENT, [0 ,"# METHOD: ", method.name, " [instruction can be deleted]"])

        m_arguments = dict[str, tuple[int, str]]() #offset, type
        var_store = dict[str, VariableStore]() #Variables of Method
        methods_dict[method.name] = MethodStore(method.name, line, m_arguments, var_store)

        self._context_method_func = method.name

        #Args are only ints!! ArgDefNodes are untyped
        #First arg is Object calling the Method:  Bsp. Stack... MAIN... parameter3, parameter2, parameter1, (this.x, this.y)Object, Dynamic,RET,B, ...Method...
        type_class = self._program_tables.types[self._context_type]
        arg_offs = -type_class.size

        #Arguments of method
        for _, arg in enumerate(method.args):
            arg_offs -= 1 #-1 for int
            m_arguments[arg.name] = (arg_offs, "int")

        #Return Value (only int)
        self._return_adress = arg_offs - 1

        self._declare_variables(method.vars_, var_store)

        self._context_var_dict = var_store
        for statement in method.stmts:
            self._handle_statement(statement)

        self._add_instruction_line(RETURN, []) #Default return at method end


    def _instanciate(self, instance_node: InstanceNode):
        type_store = self._program_tables.types[instance_node.type_]
        constructor = type_store.methods[""] #CONSTRUCTOR
        type_size = type_store.size
        if type_size > 0:
            self._add_instruction_line(INCREMENT, [type_size]) #Make store for object returned by CONSTRUCTOR
            self._add_instruction_line(CALL, [0, constructor.adress])   


    def _general_method_call(self, call_node: MethodCallNode | InstMethodCallNode):
        #1. Add return_value storage
        self._add_instruction_line(INCREMENT, [1]) #change for bigger types

        #2. Push Parameters on stack, reversed
        arg_count = 0
        for argument in call_node.args[::-1]:
            arg_count += 1
            self._handle_expression(argument)
        
        #3. Push Objec on stack
            #Get type of calling object
        match call_node:
            case MethodCallNode():
                type_store = self._program_tables.types[self._context_type]
                start_adress = -type_store.size #Is before call head

            case InstMethodCallNode():
                variable_dict = self._program_tables.variables          #Variables of Main
                var_store = variable_dict[call_node.class_name]         #Variable that calls method       
                type_store = self._program_tables.types[var_store.type_ ]
                start_adress = var_store.address
                

            case _:
                raise TypeError(f"Unexpected type: {type(call_node)} in general method call")

            #Iterate over attributes backwards
        for v in reversed(type_store.instance_vars.values()): 
            self._add_instruction_line(LOAD, [0, start_adress + v.address]) #base should b 0 """TO DO BASE"""

        #4. Call Method
        method = type_store.methods[call_node.name]
        self._add_instruction_line(CALL, [0, method.adress])  # """TO DO BASE"""

        #5. CleanUp after Return
        #5.1 object back
        for v in type_store.instance_vars.values(): 
            self._add_instruction_line(STORE, [0, start_adress + v.address]) #base should b 0 """TO DO BASE"""
            
        #5.2 delete Arg copies
        self._add_instruction_line(INCREMENT, [-arg_count]) #base should b 0
#endregion

    def _handle_multiple_statements(self, statements: list[Statement]):
        for statement in statements:
            self._handle_statement(statement)

    def _handle_statement(self, statement: Statement):
        match statement:
            case AssignmentNode():
                self._handle_assignment(statement.var, statement.value)
            case WriteNode():
                self._handle_write(statement)
            case IfThenElseNode():
                self._handle_if_else(statement)
            case WhileNode():
                self._handle_while(statement)
            case ForLoop():
                self._handle_forloop(statement)
            case ReturnNode():
                self._handle_return(statement)
            case CallNode():
                self._handle_call_node(statement)
                self._add_instruction_line(INCREMENT, [-1]) #Delete Return for call without assignment
            case _:
                raise TypeError(f"Unexpected type: {type(statement)} for a statment")
            
    def _handle_operation(self, operationNode: OperationNode):
        self._handle_expression(operationNode.left)
        self._handle_expression(operationNode.right)
        self._add_instruction_line(OPERATOR, [operationNode.operation.value])
            
    def _handle_expression(self, expression: Expression):
        match expression:
            case int():
                self._add_instruction_line(LITERAL, [expression])
            case OperationNode():
                self._handle_operation(expression)
            case _ if is_useNode(expression):
                self._handle_use_node_load(expression)
            case CallNode():
                self._handle_call_node(expression)
            case _:
                raise TypeError(f"Unexpected type: {type(expression)} in Expression")
        
#region CONTROL STRUCTURES
    def _handle_if_else(self, if_else_node: IfThenElseNode):
        #1. Evaluate condition
        else_on_true = self._handle_comparison_node(if_else_node.condition)
        
        #2. Jump to else if condition is false
        if else_on_true:
            else_adress = self._add_instruction_line(JMP_ON_TRUE, [])
        else:
            else_adress = self._add_instruction_line(JMP_ON_FALSE, [])

        #3. If-statments
        for statement in if_else_node.then:
            self._handle_statement(statement)

        #4. Jump to end/after else
        if len(if_else_node.alternative) != 0:
            end_adress = self._add_instruction_line(JUMP, [])

            #5. Else stament
            else_adress.append(self._get_line_number())
            self._handle_multiple_statements(if_else_node.alternative)
            
            ###
            end_adress.append(self._get_line_number())
        else:
            # No else statment -> end
            else_adress.append(self._get_line_number())
            

    def _handle_while(self, while_node: WhileNode):
        #1. Safe line of condition to jump ouside of loop if condition is false
        condition_line = self._get_line_number()

        #2. Evaluate condition (Jump behind loop, if condition is false)
        end_on_true = self._handle_comparison_node(while_node.condition)
        
        #3. Jump to to end/after if condition is false (end_on_true shows if condition needs to be negated [if end_on_true = True -> Negate])
        if end_on_true:
            end_adress = self._add_instruction_line(JMP_ON_TRUE, [])
        else:
            end_adress = self._add_instruction_line(JMP_ON_FALSE, [])

        #4. Statements inside of loop
        self._handle_multiple_statements(while_node.do)

        #5. Jump back to condition
        self._add_instruction_line(JUMP, [condition_line])

        ###
        end_adress.append(self._get_line_number())
            

    def _handle_forloop(self, for_node: ForLoop):
        #1. Init var as lower
        self._handle_expression(for_node.lower)
        #2. Safe line of condition to jump ouside of loop if condition is false
        condition_line = self._get_line_number()
        #First store lower, next store increased
        self._store_var(for_node.loop_var)


        #3.Load lower and upper and compare
        #load lower
        self._load_var(for_node.loop_var)
        #load upper
        self._handle_expression(for_node.upper)
        #compare
        self._add_instruction_line(OPERATOR, ["<"])

        #4. Evaluate condition (Jump behind loop, if condition is false)
        end_adress = self._add_instruction_line(JMP_ON_FALSE, [])

        #4. Statements inside of loop
        self._handle_multiple_statements(for_node.stmts)

        #5. Increase variable
        self._load_var(for_node.loop_var)
        self._add_instruction_line(LITERAL, [1])
        self._add_instruction_line(OPERATOR, ["+"])

        #6. Jump back to condition
        self._add_instruction_line(JUMP, [condition_line])

        ###
        end_adress.append(self._get_line_number())

        #7. Reduce variable if it was to big
        self._load_var(for_node.loop_var)
        self._add_instruction_line(LITERAL, [1])
        self._add_instruction_line(OPERATOR, ["-"])
        self._store_var(for_node.loop_var)


    def _handle_comparison_node(self, comp_node: ComparisonNode | InvertNode) -> bool:
        match comp_node:
            case ComparisonNode():
                if self._handle_part_of_comparison(comp_node.left):
                    self._add_instruction_line(OPERATOR, ["!"])    #Statment needs to be negated

                if self._handle_part_of_comparison(comp_node.right):
                    self._add_instruction_line(OPERATOR, ["!"])    #Statment needs to be negated

                (operator, is_not) = convert_operator(comp_node.operation)
                self._add_instruction_line(OPERATOR, [operator])
                if is_not: #Statment needs to be negated
                    return True
                return False
            
            case InvertNode():
                self._handle_comparison_node(comp_node.condition)
                return True


    def _handle_part_of_comparison(self, node_lr: Expression | InvertNode):
        #Handle left or right part of a Comparison
        match node_lr:
            case InvertNode():
                return not self._handle_comparison_node(node_lr.condition)
            
            case _ if is_expression(node_lr):
                self._handle_expression(node_lr)

            case _:
                raise TypeError(f"Unexpected type: {type(node_lr)} in Comparison PART")
    
    def _handle_return(self, return_node: ReturnNode):
        if self._context_type == "/MAIN":
            self._add_instruction_line(HALT, [])

        else:
            self._handle_expression(return_node.expr)
            self._add_instruction_line(STORE, [0, self._return_adress]) #Return current base!
            self._add_instruction_line(RETURN, [])

#endregion

#region write code

    def _add_instruction_line(self, instr: str, args: list[str]) -> list[str]:
        #Add one instruction
        self._code.append((instr, args))
        return self._code[-1][1]

    def _get_line_number(self) -> int:
        return len(self._code)

    def get_code(self):
        return deepcopy(self._code)
    
    def write_file(self, file_path):
        string_s = ""
        for (instr, args) in self._code:
            string_s += instr + " "
            string_s += "".join(str(arg) + " " for _, arg in enumerate(args))
            string_s += "\n"

        try:
            with open(file_path, "w") as file:
                file.write(string_s)
        except OSError:
            print(f"Error occurred during reading file at location: {file_path}")
            exit(-1)

#endregion


if __name__ == "__main__":
    path =  "./test_programs/"
    #program = "faculty_function.lmu"
    #program = "faculty_class.lmu"
    program = "car.lmu"
    program = "program.lmu"
    program = "code_example.lmu"
    lexer = Lexer(path + program)
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    abstract_syntax_tree = parser.get_program_ast()

    codeGenerator = CodeGenerator(abstract_syntax_tree, True)
    codeGenerator.write_file("generated_code.txt")
