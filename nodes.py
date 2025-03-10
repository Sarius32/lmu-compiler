from __future__ import annotations

from abc import ABC
from dataclasses import dataclass

from lexer import Operator


class Node(ABC):
    def get_pre_evaluated(self):
        return self


### Expression Nodes

@dataclass
class OperationNode(Node):
    left: Expression
    operation: Operator
    right: Expression

    def get_pre_evaluated(self):
        op_dict = {
            Operator.PLUS: lambda left, right: int(left + right),
            Operator.MINUS: lambda left, right: int(left - right),
            Operator.MULTIPLY: lambda left, right: int(left * right),
            Operator.DIVIDE: lambda left, right: int(left / right),
        }

        # if possible => reduce left/right side
        self.left = self.left.get_pre_evaluated() if type(self.left) != int else self.left
        self.right = self.right.get_pre_evaluated() if type(self.right) != int else self.right

        # if possible directly calculate the result and replace the node
        if type(self.left) == int and type(self.right) == int:
            return int(op_dict[self.operation](self.left, self.right))

        return self


@dataclass
class InvertNode(Node):
    condition: ComparisonNode

    def get_pre_evaluated(self):
        # if possible => reduce condition
        self.condition = self.condition.get_pre_evaluated() if type(self.condition) != int else self.condition

        # if possible directly calculate the result and replace the node
        return int(not self.condition) if type(self.condition) == int else self


@dataclass
class ComparisonNode(Node):
    left: Expression
    operation: Operator
    right: Expression

    def get_pre_evaluated(self):
        op_dict = {
            Operator.EQUALS: lambda left, right: int(left == right),
            Operator.SMALLER: lambda left, right: int(left < right),
            Operator.SMALLER_EQ: lambda left, right: int(left <= right),
            Operator.GREATER: lambda left, right: int(left > right),
            Operator.GREATER_EQ: lambda left, right: int(left >= right),
            Operator.NOT_EQUAL: lambda left, right: int(left != right)
        }

        # if possible => reduce left/right side
        self.left = self.left.get_pre_evaluated() if type(self.left) != int else self.left
        self.right = self.right.get_pre_evaluated() if type(self.right) != int else self.right

        # if possible directly calculate the result and replace the node
        if type(self.left) == int and type(self.right) == int:
            return int(op_dict[self.operation](self.left, self.right))

        return self


### Argument/Attribute/Variable Definition Nodes

@dataclass
class ArgDefNode(Node):
    name: str


@dataclass
class AttributeDefNode(Node):
    name: str
    value: OperationNode | int

    def get_pre_evaluated(self):
        self.value = self.value.get_pre_evaluated() if type(self.value) != int else self.value
        return self


@dataclass
class InstanceNode(Node):
    type_: str


@dataclass
class VariableDefNode(Node):
    name: str
    value: InstanceNode | OperationNode | int
    type_: str

    def get_pre_evaluated(self):
        self.value = self.value.get_pre_evaluated() if type(self.value) != int else self.value
        return self


### Argument/Attribute/Variable Reference/Use Nodes


@dataclass
class ArgUseNode(Node):
    name: str


@dataclass
class AttrUseNode(Node):
    name: str


@dataclass
class VarUseNode(Node):
    name: str


@dataclass
class InstAttrUseNode(Node):
    class_name: str
    name: str


UseNode = ArgUseNode | AttrUseNode | VarUseNode | InstAttrUseNode


### Function/Method Definition Nodes


@dataclass
class MethodDefNode(Node):
    name: str
    args: list[ArgDefNode]
    vars_: list[VariableDefNode]
    stmts: list[Statement]

    def get_pre_evaluated(self):
        stmts = []
        for stmt in self.stmts:
            new_stmt = stmt.get_pre_evaluated()
            if new_stmt:  # don't append if stmt collapsed into no statement (i.e. if never TRUE)
                stmts.append(new_stmt) if type(new_stmt) != list else [stmts.append(stmt_) for stmt_ in new_stmt]
        self.stmts = stmts

        return self


class FunctionDefNode(MethodDefNode):
    ...


### Function/Method Call Nodes


@dataclass
class CallNode(Node):
    name: str
    args: list[Expression]


Expression = OperationNode | UseNode | CallNode | int


@dataclass
class FuncCallNode(CallNode):
    ...


@dataclass
class MethodCallNode(CallNode):
    ...


@dataclass
class InstMethodCallNode(CallNode):
    class_name: str


### Statement Nodes


@dataclass
class WriteNode(Node):
    expr: Expression | None


@dataclass
class IfThenElseNode(Node):
    condition: ComparisonNode | InvertNode
    then: list[Statement]
    alternative: list[Statement]

    def get_pre_evaluated(self):
        # if possible => reduce condition
        self.condition = self.condition.get_pre_evaluated() if type(self.condition) != int else self.condition

        then = []
        for stmt in self.then:
            new_stmt = stmt.get_pre_evaluated()
            if new_stmt:  # don't append if stmt collapsed into no statement (i.e. if never TRUE)
                then.append(new_stmt) if type(new_stmt) != list else [then.append(stmt_) for stmt_ in new_stmt]
        self.then = then

        alternative = []
        for stmt in self.alternative:
            new_stmt = stmt.get_pre_evaluated()
            if new_stmt:  # don't append if stmt collapsed into no statement (i.e. if never TRUE)
                alternative.append(new_stmt) if type(new_stmt) != list else [alternative.append(stmt_) for stmt_ in
                                                                             new_stmt]
        self.alternative = alternative

        if type(self.condition) == int:
            return self.then if self.condition else self.alternative
        
        return self


@dataclass
class ForLoop(Node):
    loop_var: VarUseNode
    lower: Expression
    upper: Expression
    stmts: list[Statement]

    def get_pre_evaluated(self):
        self.lower = self.lower.get_pre_evaluated() if type(self.lower) != int else self.lower
        self.upper = self.upper.get_pre_evaluated() if type(self.upper) != int else self.upper

        stmts = []
        for stmt in self.stmts:
            new_stmt = stmt.get_pre_evaluated()
            if new_stmt:  # don't append if stmt collapsed into no statement (i.e. if never TRUE)
                stmts.append(new_stmt) if type(new_stmt) != list else [stmts.append(stmt_) for stmt_ in new_stmt]
        self.stmts = stmts

        return self


@dataclass
class WhileNode(Node):
    condition: ComparisonNode | InvertNode
    do: list[Statement]

    def get_pre_evaluated(self):
        # if possible => reduce condition
        self.condition = self.condition.get_pre_evaluated() if type(self.condition) != int else self.condition

        do = []
        for stmt in self.do:
            new_stmt = stmt.get_pre_evaluated()
            if new_stmt:  # don't append if stmt collapsed into no statement (i.e. if never TRUE)
                do.append(new_stmt) if type(new_stmt) != list else [do.append(stmt_) for stmt_ in new_stmt]
        self.do = do

        return self


@dataclass
class InputNode(Node):
    ...


@dataclass
class AssignmentNode(Node):
    var: UseNode
    value: InputNode | Expression

    def get_pre_evaluated(self):
        self.value = self.value.get_pre_evaluated() if type(self.value) != int else self.value
        return self


@dataclass
class ReturnNode(Node):
    expr: Expression

    def get_pre_evaluated(self):
        self.expr = self.expr.get_pre_evaluated() if type(self.expr) != int else self.expr
        return self


Statement = WriteNode | IfThenElseNode | ForLoop | WhileNode | AssignmentNode | CallNode | ReturnNode


### Container Nodes

@dataclass
class ClassNode(Node):
    name: str
    attrs: list[AttributeDefNode]
    methods: list[MethodDefNode]

    def get_pre_evaluated(self):
        self.methods = [method.get_pre_evaluated() for method in self.methods]
        return self


@dataclass
class ProgramNode(Node):
    classes: list[ClassNode]
    funcs: list[FunctionDefNode]
    vars_: list[VariableDefNode]
    stmts: list[Statement]

    def get_pre_evaluated(self):
        self.classes = [class_.get_pre_evaluated() for class_ in self.classes]
        self.funcs = [func.get_pre_evaluated() for func in self.funcs]

        stmts = []
        for stmt in self.stmts:
            new_stmt = stmt.get_pre_evaluated()
            if new_stmt:  # don't append if stmt collapsed into no statement (i.e. if never TRUE)
                stmts.append(new_stmt) if type(new_stmt) != list else [stmts.append(stmt_) for stmt_ in new_stmt]
        self.stmts = stmts

        return self

### helper functions for matching
        
def is_expression(value: Expression) -> bool:
    return isinstance(value, (OperationNode, UseNode, CallNode, int)) or is_useNode(value)
        
def is_useNode(value: UseNode) -> bool:
    return isinstance(value, (ArgUseNode, AttrUseNode, VarUseNode, InstAttrUseNode))
