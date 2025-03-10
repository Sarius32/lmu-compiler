
from code_generation import CodeGenerator
from lexer import Lexer
from nodes import ArgDefNode, AttributeDefNode, ForLoop, InstanceNode, ReturnNode, VariableDefNode, ArgUseNode, AttrUseNode, VarUseNode, \
    InstAttrUseNode, MethodDefNode, FunctionDefNode, FuncCallNode, MethodCallNode, InstMethodCallNode, WriteNode, \
    IfThenElseNode, WhileNode, InputNode, AssignmentNode, ClassNode, ProgramNode, OperationNode, InvertNode, \
    ComparisonNode, Expression, Statement, UseNode, CallNode, is_expression, is_useNode
from parser import Parser

class OptimizeNodes:

    def __init__(self,  syntax_tree: ProgramNode, pre_evaluate: bool = True, remove_unreachable: bool = True, remove_unused: bool = True):

        if pre_evaluate:
            self._syntax_tree = syntax_tree.get_pre_evaluated()
        else:
            self._syntax_tree = syntax_tree

        if remove_unreachable:
            self._syntax_tree.get_reachable_only()

        self._variable_use = dict[str, bool]()

        self._func_use = dict[str, bool]()
        self._class_use = dict[str, bool]() #class, use, methods
        self._method_use = dict[str, dict[str, bool]]() #class, methods
        self._class_context = str()
        self._method_context = str()
        self._var_type = dict[str, str]() #name type

        if remove_unused:
            for cl in self._syntax_tree.classes:
                self._class_use[cl.name] = False
                self._method_use[cl.name] = dict[str, bool]()

            self._remove_unused()

    def _remove_unused(self):

        has_changed = False

        #search use in all statments of MAIN
        self._init_var_types(self._syntax_tree.vars_)
        self._search_in_statments(self._syntax_tree.stmts)

        #Remove unused variables
        opt_vars = list[VariableDefNode]()
        for (variable, used) in self._variable_use.items():
            if used:
                var = next((v for v in self._syntax_tree.vars_ if v.name == variable), None)
                if not var:
                    raise TypeError(f"Unexpected Vartype: {variable}")

                opt_vars.append(var)

            else: 
                has_changed = True
            
            
        
        self._syntax_tree.vars_ = opt_vars

        #Remove write of unused variables
        opt_stmts = list[Statement]()
        for (stmt) in self._syntax_tree.stmts:
            match stmt:
                case AssignmentNode():
                    var_used = self._variable_use.get(stmt.var.name, False)
                    if var_used:
                        opt_stmts.append(stmt)
                
                    else: 
                        has_changed = True

                case InstMethodCallNode():
                    var_used = self._variable_use.get(stmt.class_name, False)
                    if var_used:
                        opt_stmts.append(stmt)
                
                    else: 
                        has_changed = True

                case _:
                    opt_stmts.append(stmt)
        
        self._syntax_tree.stmts = opt_stmts


        opt_funcs = list[FunctionDefNode]()
        for func in self._syntax_tree.funcs:
            func_used = self._func_use.get(func.name, False)
            if func_used:
                opt_funcs.append(func)
            
            else:
                has_changed = True

        self._syntax_tree.funcs = opt_funcs

        opt_class = list[ClassNode]()
        for class_ in self._syntax_tree.classes:
            class_used = self._class_use.get(class_.name, False)
            if class_used:
                
                opt_methods = list[MethodDefNode]()
                for method in class_.methods:
                    method_used = self._method_use.get(class_.name, dict[str, bool]()).get(method.name, False)
                    if method_used:
                        opt_methods.append(method)
                    else:
                        has_changed = True

                class_.methods = opt_methods

                opt_class.append(class_)

            else:
                has_changed = True

        self._syntax_tree.classes = opt_class

        if has_changed:
            self._remove_unused()





    def _init_var_types(self, var_defs: list[VariableDefNode]):
        for var in var_defs:
            self._var_type[var.name] = var.type_

    def _search_in_statments(self, statments: list[Statement]):
        for statment in statments:
            self._s_statment(statment)

    def _s_statment(self, statement: Statement):
        match statement:
            case AssignmentNode():
                self._handle_assignment(statement.value)

            case WriteNode():
                self._handle_expression(statement.expr)

            case IfThenElseNode():
                self._handle_comparison(statement.condition)
                self._search_in_statments(statement.then)
                self._search_in_statments(statement.alternative)

            case WhileNode():
                self._handle_comparison(statement.condition)
                self._search_in_statments(statement.do)

            case ForLoop():
                self._handle_expression(statement.lower)
                self._handle_expression(statement.upper)
                self._search_in_statments(statement.stmts)
                #loop var only var use

            case ReturnNode():
                self._handle_expression(statement.expr)

            case CallNode():
                self._handle_call_node(statement)
                
            case _:
                raise TypeError(f"Unexpected type: {type(statement)} for a statment")

    def _handle_expression(self, expression: Expression):
        match expression:
            case int():
                pass

            case OperationNode():
                self._handle_expression(expression.right)
                self._handle_expression(expression.left)
                
            case CallNode():
                self._handle_call_node(expression)

            case _ if is_useNode(expression):
                self._handle_use_node(expression)

            case _:
                raise TypeError(f"Unexpected type: {type(expression)} in Expression")
    
    def _handle_call_node(self, call_node: CallNode):

        for arg in call_node.args:
            self._handle_expression(arg)

        match call_node:
            case FuncCallNode():
                self._func_use[call_node.name] = True

            case MethodCallNode():
                self._class_use[self._class_context] = True
                self._method_use[self._class_context][call_node.name] = True

            case InstMethodCallNode():
                type_v = self._var_type[call_node.class_name]
                self._class_use[type_v] = True
                self._method_use[type_v][call_node.name] = True

            case _:
                raise TypeError(f"Unexpected type: {type(call_node)} for a Call Node")


    def _handle_assignment(self, value: Expression | InputNode):
        match value:
            case _ if is_expression(value):
                self._handle_expression(value)
            case InputNode():
                pass
            case _:
                raise TypeError(f"Unexpected type: {type(value)} for value of assignment")

        #self._handle_use_node(var) just a write

    def _handle_comparison(self, comp: ComparisonNode):
        self._handle_part_of_comparison(comp.right)
        self._handle_part_of_comparison(comp.left)
        
    def _handle_part_of_comparison(self, node_lr: Expression | InvertNode):
        match node_lr:
            case InvertNode():
                self._handle_part_of_comparison(node_lr.condition.right)
                self._handle_part_of_comparison(node_lr.condition.left)
            case _ if is_expression(node_lr):
                self._handle_expression(node_lr)
            case _:
                raise TypeError(f"Unexpected type: {type(node_lr)} in Comparison PART")
    

    def _handle_use_node(self, use_node: UseNode):
        match use_node:
            case VarUseNode():
                self._variable_use[use_node.name] = True
            case ArgUseNode():
                pass
            case AttrUseNode():
                pass
            case InstAttrUseNode():
                type_v = self._var_type[use_node.class_name]
                self._class_use[type_v] = True

                #Variable use
                self._variable_use[use_node.class_name] = True

            case _:
                raise TypeError(f"Unexpected type: {type(use_node)} in Handle use node")

    

    def get_optimized_tree(self) -> ProgramNode:
        return self._syntax_tree
    


if __name__ == "__main__":
    path =  "./test_programs/"
    program = "optimization_test.lmu"
    lexer = Lexer(path + program)
    tokens = lexer.get_tokens()
    parser = Parser(tokens)
    abstract_syntax_tree = parser.get_program_ast()

    optimizer = OptimizeNodes(abstract_syntax_tree)
    abstract_syntax_tree = optimizer.get_optimized_tree()

    codeGenerator = CodeGenerator(abstract_syntax_tree, True)
    codeGenerator.write_file("generated_code.txt")
