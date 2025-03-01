from copy import deepcopy

from lexer import Lexer
from parser import Parser
from nodes import ArgDefNode, AttributeDefNode, InstanceNode, ReturnNode, VariableDefNode, ArgUseNode, AttrUseNode, VarUseNode, \
    InstAttrUseNode, MethodDefNode, FunctionDefNode, FuncCallNode, MethodCallNode, InstMethodCallNode, WriteNode, \
    IfThenElseNode, WhileNode, InputNode, AssignmentNode, ClassNode, ProgramNode, OperationNode, InvertNode, \
    ComparisonNode, Expression, Statement, UseNode, CallNode

from variable import MethodStore, TypeStore, VariableStore


        
def is_expression(value) -> bool:
    return isinstance(value, (OperationNode, int)) or is_useNode(value)
        
def is_useNode(value) -> bool:
    return isinstance(value, (ArgUseNode, AttrUseNode, VarUseNode, InstAttrUseNode))
        
def is_statment(value) -> bool:
    return isinstance(value, (WriteNode, IfThenElseNode, WhileNode, AssignmentNode)) or is_callNode(value)
        
def is_callNode(value) -> bool:
    return isinstance(value, (FuncCallNode, MethodCallNode, InstMethodCallNode))

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
        self._context_var_dict = dict[str, VariableStore]()
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
        for var in self._global_var_dict.values():
            address = var.address
            self._add_line("LOD", [0, address])
            self._add_line("WRI", [])
        ###

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
        self._current_base += 1
        if(self._current_base != 1):
            raise ValueError(f"Unexpected value: {self._current_base} of Base in Class definition")


        #Constructor: AttributeDefNode can consist of Operations
        size = 0

        method_name = "CONSTRUCTOR"
        methods_dict[method_name] = MethodStore(method_name, line, {}, {})
        for index, attribute in enumerate(class_node.attrs):
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
        

        for method in class_node.methods:
            line = self._get_line_number()
            self._add_line("INC", [0 ,"# METHOD: ", method.name])
            m_parameter = dict[str, tuple[int, str]]() #offset, type
            var_store = dict[str, VariableStore]() #Variables of Method
            methods_dict[method.name] = MethodStore(method.name, line, m_parameter, var_store)

            #Args are only ints!! ArgDefNodes are untyped
            #First arg is Object calling the Method:  Bsp. Stack... MAIN... parameter3, parameter2, parameter1, (this.x, this.y)Object, Dynamic,RET,B, ...Method...
            arg_offs = -size

            #Parameter
            for _, arg in enumerate(method.args):
                arg_offs -= 1 #-1 for int
                m_parameter[arg] = (arg_offs, "int")

            #Return Value (only int)
            self._return_adress = arg_offs - 1

            self.t = 2 #Inside a new Method
            self._declare_variables(method.vars_, var_store)

            self._context_var_dict = var_store
            for statement in method.stmts:
                self._handle_statement(statement)

        self._current_base -= 1
        type_class = TypeStore(class_node.name, size, attributes, methods_dict)
        self._type_dict[type_class.name] = type_class


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
            #case InputNode():
            #    None
            case _:
                raise TypeError(f"Unexpected type: {type(value)} for value of assignment")


        match var:
            case VarUseNode():
                self._store_var(var)
            # case ArgUseNode():
            #     self._store_var(var)
            # case AttrUseNode():
            #     self._store_var(var)
            case InstAttrUseNode():
                self._store_inst_attribute(var)
            case _:
                raise TypeError(f"Unexpected type: {type(var)} for variable in assignment")
            
    def _store_inst_attribute(self, inst_attribute: InstAttrUseNode):
        ### EINFÜGEN und in externe Methode auslagern
        # if variable.name in self._context_var_dict:
        #     var = self._context_var_dict[variable.name]
        # else:
        #     var = self._global_var_dict[variable.name]
        variable = self._global_var_dict[inst_attribute.class_name]
        attribute_name = inst_attribute.name

        variable_type = self._type_dict[variable.type_] #Get type of variable
        (offset, attribute_type) = variable_type.instance_vars[attribute_name]

        address = variable.address + offset
        base = self._current_base - variable.base
        self._add_line("STO", [base, address])

    def _handle_statement(self, statement: Statement):
        match statement:
            case AssignmentNode():
                self._handle_assignment(statement.var, statement.value)
            # case WriteNode(): TO DO 
            #     statement
            # case IfThenElseNode():
            #     statement
            # case WhileNode():
            #     statement
            case ReturnNode():
                self._handle_expression(statement.expr)
                self._add_line("STO", [self._current_base, self._return_adress])
                self._add_line("RET", [])

            #case _ if is_callNode(statement):
            #    statement
            case _:
                raise TypeError(f"Unexpected type: {type(statement)} for a statment")


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
            case VarUseNode():
                self._load_var(expression)
            # case ArgUseNode(): TO DO
            #     None
            #case AttrUseNode():
            #    self._load_var(expression.name)
            #case InstAttrUseNode():
            #     None
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
    lexer = Lexer("program.txt")
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    abstract_syntax_tree = parser.get_program_ast()

    codeGenerator = CodeGenerator(abstract_syntax_tree)
    codeGenerator.write_file("generated_code.txt")
