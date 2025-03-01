from copy import deepcopy

from lexer import Lexer
from parser import Parser
from nodes import ArgDefNode, AttributeDefNode, InstanceNode, ReturnNode, VariableDefNode, ArgUseNode, AttrUseNode, VarUseNode, \
    InstAttrUseNode, MethodDefNode, FunctionDefNode, FuncCallNode, MethodCallNode, InstMethodCallNode, WriteNode, \
    IfThenElseNode, WhileNode, InputNode, AssignmentNode, ClassNode, ProgramNode, OperationNode, InvertNode, \
    ComparisonNode, Expression, Statement, UseNode, CallNode

from tokens import Operator
from variable import MethodStore, TypeStore, VariableStore


        
def is_expression(value) -> bool:
    return isinstance(value, (OperationNode, int)) or is_useNode(value)
        
def is_useNode(value) -> bool:
    return isinstance(value, (ArgUseNode, AttrUseNode, VarUseNode, InstAttrUseNode))
        
def is_statment(value) -> bool:
    return isinstance(value, (WriteNode, IfThenElseNode, WhileNode, AssignmentNode)) or is_callNode(value)
        
def is_callNode(value) -> bool:
    return isinstance(value, (FuncCallNode, MethodCallNode, InstMethodCallNode))

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


class CodeGenerator:
    _code = list[tuple[str, list[str]]]()
    

    def __init__(self, syntax_tree: ProgramNode):

        # init translation of instructions
        # self._instr_dict = {
        #     "RST": self._reset,
        #     "LOD": self._load,
        #     "STO": self._store_,
        #     "INC": self._inc,
        #     "LIT": self._literal,
        #     "JMP": self._jump,
        #     "JOT": self._jump_on_true,
        #     "JOF": self._jump_on_false,
        #     "CAL": self._call,
        #     "RET": self._return,
        #     "OPR": self._operator,
        #     "REA": self._read,
        #     "WRI": self._write,
        #     "HLT": None
        # }



        self._syntax_tree = syntax_tree
        self._global_var_dict = dict[str, VariableStore]()
        self._context_var_dict = dict[str, VariableStore]() #TO DO: solve context attributes, variables ... verschachtelung!!
        self._context_class = str()
        self._context_method = str()
        self._type_dict = dict[str, TypeStore]()

        self._return_adress = 0
        
        #Initialize Type INT
        type_int = TypeStore("int", 1, {}, {})
        self._type_dict[type_int.name] = type_int

        self._current_base = 0

        #Init Program
        self._add_line("RST", [])

        #Global Variables

        #Jump to Main Programm
        main_prog_adress = self._add_line("JMP", [])

        #0 = dynamic link; 1 = return adress; 2 = static link
        self.t = 2

        self._define_classes(self._syntax_tree.classes)

        ###Main starts
        self.t = 2

        main_prog_adress.append(self._get_line_number())
        main_prog_adress.append("# MAIN")
        main_prog_adress = self._add_line("INC", [0, "# MAIN: (delete later)"])

        self._declare_variables(self._syntax_tree.vars_, self._global_var_dict)
        
        for _, statement in enumerate(self._syntax_tree.stmts):
            self._handle_statement(statement)

        ###Just to check all variables in the end ... for development/testing reasons
        # for var in self._global_var_dict.values():
        #     address = var.address
        #     self._add_line("LOD", [0, address])
        #     self._add_line("WRI", [])
        # ###

        self._add_line("HLT", [])

    def _define_classes(self, classes_list: list[ClassNode]):
        for _, class_ in enumerate(classes_list):
            self._handle_class(class_)

    def _handle_class(self, class_node: ClassNode):
        attributes = dict[str, tuple[int, str]]() #name: (offset, type)
        line = self._get_line_number()

        #Make Space vor Class Attributes
        attributes_inc = self._add_line("INC", [0 ,"# CONSTRUCTOR (remove line later!!)"])


        #Parameters have to be on stack B -1
        methods_dict = dict[str, MethodStore]()
        #self._current_base += 1
        #if(self._current_base != 1):
        #    raise ValueError(f"Unexpected value: {self._current_base} of Base in Class definition")


        #Constructor: AttributeDefNode can consist of Operations
        size = 0

        method_name = "CONSTRUCTOR"
        methods_dict[method_name] = MethodStore(method_name, line, {}, {})
        for _, attribute in enumerate(class_node.attrs):
            attributes[attribute.name] = (size, "int")
            size += 1 #If not only ints, CHANGE
            
            match attribute.value:
                case int():
                    self._add_line("LIT", [attribute.value])
                case OperationNode():
                    self._handle_operation(attribute.value)
                case _:
                    raise TypeError(f"Unexpected type: {type(attribute.value)} in Class attribute")
            
            self._add_line("STO", [0, -size , f"# ATTRIBUTE: {attribute.name}"]) #base should be 0 = current_base - 1
        self._add_line("RET", [])
        
        self._context_class = class_node.name #TO DO: make clear what variables and attributes can be accesed from current location!!!

        type_class = TypeStore(class_node.name, size, attributes, methods_dict)
        self._type_dict[type_class.name] = type_class

        for method in class_node.methods:
            line = self._get_line_number()
            self._add_line("INC", [0 ,"# METHOD: ", method.name])
            m_parameter = dict[str, tuple[int, str]]() #offset, type
            var_store = dict[str, VariableStore]() #Variables of Method
            methods_dict[method.name] = MethodStore(method.name, line, m_parameter, var_store)

            self._context_method = method.name

            #Args are only ints!! ArgDefNodes are untyped
            #First arg is Object calling the Method:  Bsp. Stack... MAIN... parameter3, parameter2, parameter1, (this.x, this.y)Object, Dynamic,RET,B, ...Method...
            arg_offs = -size

            #Parameter
            for _, arg in enumerate(method.args):
                arg_offs -= 1 #-1 for int
                m_parameter[arg.name] = (arg_offs, "int")

            #Return Value (only int)
            self._return_adress = arg_offs - 1

            self.t = 2 #Inside a new Method
            self._declare_variables(method.vars_, var_store)

            self._context_var_dict = var_store
            for statement in method.stmts:
                self._handle_statement(statement)

        #self._current_base -= 1


    def _declare_variables(self, var_list: list[VariableDefNode], store_var: dict[str, VariableStore]):

        #Make Space vor Vars
        args_inc = self._add_line("INC", [])
        var_size = 0

        for  _, variable in enumerate(var_list):
            type_ = self._type_dict[variable.type_]
            address = self.t + 1 #next free adress
            self.t += type_.size
            var_size += type_.size

            variable_store = VariableStore(variable.name, address, self._current_base, type_.name)
            store_var[variable_store.name] = variable_store

            match variable.value:
                case int():
                    self._add_line("LIT", [variable.value])
                case InstanceNode(): 
                    self._instanciate(variable.value)
                case OperationNode():
                    self._handle_operation(variable.value)
                case _:
                    raise TypeError(f"Unexpected type: {type(variable.value)}")
            
            for i in range(type_.size):
                self._add_line("STO", [self._current_base, address+i, f"# VAR: {type_.name} {i}_{variable.name}"])
            #TO DO later for classes, store attributes from last to first on stack
        
        args_inc.append(var_size)
        self.t += var_size

    def _instanciate(self, instance_node: InstanceNode):
        type_store = self._type_dict[instance_node.type_]
        constructor = type_store.methods["CONSTRUCTOR"]
        self._add_line("INC", [type_store.size]) #Make store for return value of constructor
        self._add_line("CAL", [self._current_base, constructor.adress])

    def _handle_assignment(self, var: UseNode, value: Expression | InputNode):
        match value:
            case _ if is_expression(value):
                self._handle_expression(value)
            case InputNode():
                self._add_line("REA", [])
            case _:
                raise TypeError(f"Unexpected type: {type(value)} for value of assignment")

        self._handle_use_node_store(var)
            
    def _store_inst_attribute(self, inst_attribute: InstAttrUseNode):
        ### EINFÜGEN und in externe Methode auslagern
        # if variable.name in self._context_var_dict:
        #     var = self._context_var_dict[variable.name]
        # else:
        #     var = self._global_var_dict[variable.name]
        variable = self._global_var_dict[inst_attribute.class_name]
        attribute_name = inst_attribute.name

        variable_type = self._type_dict[variable.type_] #Get type of variable
        (offset, _) = variable_type.instance_vars[attribute_name]

        address = variable.address + offset
        base = self._current_base - variable.base
        self._add_line("STO", [base, address])
            
    def _load_inst_attribute(self, inst_attribute: InstAttrUseNode):
        ### EINFÜGEN und in externe Methode auslagern
        # if variable.name in self._context_var_dict:
        #     var = self._context_var_dict[variable.name]
        # else:
        #     var = self._global_var_dict[variable.name]
        variable = self._global_var_dict[inst_attribute.class_name]
        attribute_name = inst_attribute.name

        variable_type = self._type_dict[variable.type_] #Get type of variable
        (offset, _) = variable_type.instance_vars[attribute_name]

        address = variable.address + offset
        base = self._current_base - variable.base
        self._add_line("LOD", [base, address])

            
    def _load_attribute(self, attribute: AttrUseNode):
        ### EINFÜGEN und in externe Methode auslagern
        # if variable.name in self._context_var_dict:
        #     var = self._context_var_dict[variable.name]
        # else:
        #     var = self._global_var_dict[variable.name]

        
        type_store = self._type_dict[self._context_class]
        (offset, type_) = type_store.instance_vars[attribute.name]

        address = -offset-1
        base = 0 #In current Method
        self._add_line("LOD", [base, address])


            
    def _load_arg(self, arg: ArgUseNode):
        type_store = self._type_dict[self._context_class]
        method_store = type_store.methods[self._context_method]
        (offset, type_) = method_store.parameter[arg.name]

        address = offset #Computed already in class making
        base = 0 #In current Method
        self._add_line("LOD", [base, address])

            
    def _store_arg(self, arg: ArgUseNode):
        type_store = self._type_dict[self._context_class]
        method_store = type_store.methods[self._context_method]
        (offset, type_) = method_store.parameter[arg.name]

        address = offset #Computed already in class making
        base = 0 #In current Method
        self._add_line("STO", [base, address])

            
    def _store_attribute(self, attribute: AttrUseNode):
        ### EINFÜGEN und in externe Methode auslagern
        # if variable.name in self._context_var_dict:
        #     var = self._context_var_dict[variable.name]
        # else:
        #     var = self._global_var_dict[variable.name]

        
        type_store = self._type_dict[self._context_class]
        (offset, type_) = type_store.instance_vars[attribute.name]

        address = -offset-1
        base = 0 #In current Method
        self._add_line("STO", [base, address])

    def _handle_write(self, write_node: WriteNode):
        self._handle_expression(write_node.expr)
        self._add_line("WRI", [])

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
            case ReturnNode():
                self._handle_expression(statement.expr)
                self._add_line("STO", [0, self._return_adress]) #Return current base!
                self._add_line("RET", [])

            case CallNode():
                self._handle_call_node(statement)
            case _:
                raise TypeError(f"Unexpected type: {type(statement)} for a statment")
            
    def _handle_call_node(self, call_node: CallNode):
        match call_node:
            case InstMethodCallNode():
                self._inst_method_call(call_node)
            case MethodCallNode():
                self._method_call(call_node)
            # case FuncCallNode():
            #     call_node
            case _:
                raise TypeError(f"Unexpected type: {type(call_node)} for a Call node")
    
    def _method_call(self, call_node : MethodCallNode):
        #1. Add return_value storage
        self._add_line("INC", [1]) #change for bigger types

        #2. Push Parameters on stack, reversed
        arg_count = 0
        for param in call_node.args[::-1]:
            arg_count += 1
            self._handle_expression(param)
        
        call_node
        
        self._context_method
        #3. Push Objec on stack
        type_store = self._type_dict[self._context_class]

        type_size = type_store.size
        for i in range(type_size): #Can use inst_var dictionary to be type sensitive vor attributes!!!
            self._add_line("LOD", [0, -type_size+i]) #base should b 0

        #4. Call Method
        method = type_store.methods[call_node.name]
        self._add_line("CAL", [0, method.adress]) 

        #5. CleanUp after Return
        #5.1 object back
        for i in range(type_size):
            self._add_line("STO", [0, -i-1]) #base should b 0

        # for (offset, type_) in type_store.instance_vars:
        #     self._add_line("STO", [0, base_adress+offset]) #base should b 0
            
        #5.2 delete Arg copies
        self._add_line("INC", [-arg_count]) #base should b 0

        ####TO DO: Handle Return!!!
        self._add_line("INC", [-1])


    def _inst_method_call(self, call_node : InstMethodCallNode):
        #1. Add return_value storage
        self._add_line("INC", [1]) #change for bigger types

        #2. Push Parameters on stack, reversed
        arg_count = 0
        for param in call_node.args[::-1]:
            arg_count += 1
            self._handle_expression(param)
        
        #3. Push Objec on stack
        var_store = self._global_var_dict[call_node.class_name]
        type_store = self._type_dict[var_store.type_]

        base_adress = var_store.address
        type_size = type_store.size
        for i in range(type_size): #Can use inst_var dictionary to be type sensitive vor attributes!!!
            self._add_line("LOD", [0, base_adress+type_size-1-i]) #base should b 0

        #4. Call Method
        method = type_store.methods[call_node.name]
        self._add_line("CAL", [0, method.adress]) 

        #5. CleanUp after Return
        #5.1 object back
        for i in range(type_size):
            self._add_line("STO", [0, base_adress+i]) #base should b 0

        # for (offset, type_) in type_store.instance_vars:
        #     self._add_line("STO", [0, base_adress+offset]) #base should b 0
            
        #5.2 delete Arg copies
        self._add_line("INC", [-arg_count]) #base should b 0

        ####TO DO: Handle Return!!!
        self._add_line("INC", [-1])

            

    def _handle_if_else(self, if_else_node: IfThenElseNode):
        #1. Evaluate condition
        jump_on = self._handle_comparison_node(if_else_node.condition)
        
        #2. Jump to else if condition is false
        # if jump_on:
        #     else_adress = self._add_line("JOF", []) #Jump on opposite to else
        # else:
        else_adress = self._add_line("JOF", [])

        #3. If-statments
        for statement in if_else_node.then:
            self._handle_statement(statement)

        #4. Jump to end/after else
        end_adress = self._add_line("JMP", [])

        #5. Else stament
        else_adress.append(self._get_line_number())
        for statement in if_else_node.alternative:
            self._handle_statement(statement)
        
        ###
        end_adress.append(self._get_line_number())
            

    def _handle_while(self, while_node: WhileNode):
        condition_line = self._get_line_number()
        #1. Evaluate condition
        jump_on = self._handle_comparison_node(while_node.condition)
        
        #2. Jump to to end/after if condition is false
        # if jump_on:
        #     else_adress = self._add_line("JOF", []) #Jump on opposite to else
        # else:
        end_adress = self._add_line("JOF", [])

        #3. While-statments
        for statement in while_node.do:
            self._handle_statement(statement)

        #4. Jump to condition
        self._add_line("JMP", [condition_line])

        ###
        end_adress.append(self._get_line_number())


    def _handle_comparison_node(self, comp_node: ComparisonNode) -> bool:
        self._handle_comparison_part(comp_node.left)
        self._handle_comparison_part(comp_node.right)
        (operator, is_not) = convert_operator(comp_node.operation)
        self._add_line("OPR", [operator])
        if is_not: #Statment needs to be negated
            self._add_line("OPR", ["!"])
        


    def _handle_comparison_part(self, node_lr: ComparisonNode | InvertNode | UseNode | int):
        match node_lr:
            case int():
                self._add_line("LIT", [node_lr])
            case ComparisonNode():
                self._handle_comparison_node(node_lr)
            case InvertNode():
                self._handle_comparison_node(node_lr.condition)
                self._add_line("OPR", ["!"])
            case UseNode():
                self._handle_use_node_load(node_lr)
            case _:
                raise TypeError(f"Unexpected type: {type(node_lr)} in Comparison PART")
    
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


    def _handle_operation(self, operationNode: OperationNode):
        self._handle_expression(operationNode.left)
        self._handle_expression(operationNode.right)
        self._add_line("OPR", [operationNode.operation.value])
            
    def _handle_expression(self, expression: Expression):
        match expression:
            case int():
                self._add_line("LIT", [expression])
            case OperationNode():
                self._handle_operation(expression)
            case UseNode():
                self._handle_use_node_load(expression)
            case _:
                raise TypeError(f"Unexpected type: {type(expression)} in Expression")

    def _load_var(self, variable: VarUseNode):
        if variable.name in self._context_var_dict:
            var = self._context_var_dict[variable.name]
        else:
            var = self._global_var_dict[variable.name]

        address = var.address
        base = self._current_base - var.base
        self._add_line("LOD", [base, address])

    def _store_var(self, variable: VarUseNode):
        if variable.name in self._context_var_dict:
            var = self._context_var_dict[variable.name]
        else:
            var = self._global_var_dict[variable.name]

        address = var.address
        base = self._current_base - var.base
        self._add_line("STO", [base, address])


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



    def _add_line(self, instr: str, args: list[str]) -> list[str]:
        self._code.append((instr, args))
        return self._code[-1][1]

    def _get_line_number(self) -> int:
        return len(self._code)



if __name__ == "__main__":
    #lexer = Lexer("program.txt")
    lexer = Lexer("token_test.lmu")
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    abstract_syntax_tree = parser.get_program_ast()

    codeGenerator = CodeGenerator(abstract_syntax_tree)
    codeGenerator.write_file("generated_code.txt")
