from copy import deepcopy

from lexer import Lexer
from parser import Parser
from nodes import ArgDefNode, AttributeDefNode, InstanceNode, VariableDefNode, ArgUseNode, AttrUseNode, VarUseNode, \
    InstAttrUseNode, MethodDefNode, FunctionDefNode, FuncCallNode, MethodCallNode, InstMethodCallNode, WriteNode, \
    IfThenElseNode, WhileNode, InputNode, AssignmentNode, ClassNode, ProgramNode, OperationNode, InvertNode, \
    ComparisonNode, Expression, Statement, UseNode, CallNode

from variable import TypeStore, VariableStore


        
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
        self._var_dict = dict[str, VariableStore]()
        self._type_dict = dict[str, TypeStore]()
        
        #Initialize Type INT
        type_int = TypeStore("int", 1)
        self._type_dict[type_int.name] = type_int

        self._current_base = 0

        #Init Program
        self._add_line("RST", [])

        #0 = dynamic link; 1 = return adress; 2 = static link
        t = 2

        self._declare_variables(self._syntax_tree.vars_, t)
        
        for _, statement in enumerate(self._syntax_tree.stmts):
            self._handle_statement(statement)

        for var in self._var_dict.values():
            address = var.address
            self._add_line("LOD", [0, address])
            self._add_line("WRI", [])

        self._add_line("HLT", [])

    def _declare_variables(self, var_list: list[VariableDefNode], t):

        #Make Space vor Vars
        args_inc = self._add_line("INC", [])
        var_size = 0

        for  _, variable in enumerate(var_list):
            type_ = self._type_dict[variable.type_]
            address = t + 1 + var_size

            variable_store = VariableStore(variable.name, address, self._current_base, type_.name)
            self._var_dict[variable_store.name] = variable_store

            var_size += type_.size

            match variable.value:
                case int():
                    self._add_line("LIT", [variable.value])
                # case InstanceNode(): TO DO Implement
                #     None
                case OperationNode():
                    self._handle_operation(variable.value)
                case _:
                    raise TypeError(f"Unexpected type: {type(variable.value)}")
            
            self._add_line("STO", [self._current_base, address])
            #TO DO later for classes, store attributes from last to first on stack
        
        args_inc.append(var_size)


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
            case _:
                raise TypeError(f"Unexpected type: {type(var)} for variable in assignment")
    
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
            # case FuncCallNode():
            #     statement
            # case MethodCallNode():
            #     statement
            # case InstMethodCallNode():
            #     statement
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
            # case AttrUseNode():
            #     None
            #case InstAttrUseNode():
            #     None
            case _:
                raise TypeError(f"Unexpected type: {type(expression)}")

    def _load_var(self, variable: VarUseNode):
        var = self._var_dict[variable.name]

        address = var.address
        base = self._current_base - var.base
        self._add_line("LOD", [base, address])

    def _store_var(self, variable: VarUseNode):
        var = self._var_dict[variable.name]

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



if __name__ == "__main__":
    lexer = Lexer("program.txt")
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    abstract_syntax_tree = parser.get_program_ast()

    codeGenerator = CodeGenerator(abstract_syntax_tree)
    codeGenerator.write_file("generated_code.txt")
