from __future__ import annotations

from abc import ABC
from dataclasses import dataclass

from lexer import Operator


class Node(ABC):
    def optimized(self):
        return self


### Expression Nodes

@dataclass
class OperationNode(Node):
    left: Expression
    operation: Operator
    right: Expression

    def optimized(self):
        op_dict = {
            Operator.PLUS: lambda left, right: int(left + right),
            Operator.MINUS: lambda left, right: int(left - right),
            Operator.MULTIPLY: lambda left, right: int(left * right),
            Operator.DIVIDE: lambda left, right: int(left / right),
        }

        # if possible => reduce left/right side
        if type(self.left) != int:
            self.left = self.left.optimized()
        if type(self.right) != int:
            self.right = self.right.optimized()

        # if possible directly calculate the result and replace the node
        if type(self.left) == int and type(self.right) == int:
            return int(op_dict[self.operation](self.left, self.right))

        return self


@dataclass
class InvertNode(Node):
    condition: 'ComparisonNode'

    def optimized(self):
        # if possible => reduce condition
        self.condition = self.condition.optimized() if type(self.condition) != int else None

        # if possible directly calculate the result and replace the node
        return int(not self.condition) if type(self.condition) == int else self


@dataclass
class ComparisonNode(Node):
    left: 'ComparisonNode' | InvertNode | UseNode | int
    operation: Operator
    right: 'ComparisonNode' | InvertNode | UseNode | int

    def optimized(self):
        op_dict = {
            Operator.EQUALS: lambda left, right: int(left == right),
            Operator.SMALLER: lambda left, right: int(left < right),
            Operator.SMALLER_EQ: lambda left, right: int(left <= right),
            Operator.GREATER: lambda left, right: int(left > right),
            Operator.GREATER_EQ: lambda left, right: int(left >= right),
            Operator.NOT_EQUAL: lambda left, right: int(left != right)
        }

        # if possible => reduce left/right side
        self.left = self.left.optimized() if type(self.left) != int else None
        self.right = self.right.optimized() if type(self.right) != int else None

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

    def optimized(self):
        self.value = self.value.optimized() if type(self.value) != int else None
        return self


@dataclass
class InstanceNode(Node):
    type_: str


@dataclass
class VariableDefNode(Node):
    name: str
    value: InstanceNode | OperationNode | int
    type_: str

    def optimized(self):
        self.value = self.value.optimized() if type(self.value) != int else None
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
    stmts: list['Statement']
    ret: 'Expression'

    def optimized(self):
        stmts = []
        for stmt in self.stmts:
            new_stmt = stmt.optimized()
            if new_stmt:  # don't append if stmt collapsed into no statements
                stmts.append(new_stmt) if type(new_stmt) != list else [stmts.append(stmt_) for stmt_ in new_stmt]
        self.stmts = stmts

        self.ret = self.ret.optimized() if type(self.ret) != int else None

        return self


class FunctionDefNode(MethodDefNode):
    ...


### Function/Method Call Nodes


Expression = UseNode | OperationNode | int


@dataclass
class FuncCallNode(Node):
    name: str
    args: list[Expression]


@dataclass
class MethodCallNode(Node):
    name: str
    args: list[Expression]


@dataclass
class InstMethodCallNode(Node):
    class_name: str
    name: str
    args: list[Expression]


CallNode = FuncCallNode | MethodCallNode | InstMethodCallNode


### Statement Nodes


@dataclass
class WriteNode(Node):
    expr: Expression | None


@dataclass
class IfThenElseNode(Node):
    condition: 'ComparisonNode'
    then: list['Statement']
    alternative: list['Statement']

    def optimized(self):
        # if possible => reduce condition
        self.condition = self.condition.optimized() if type(self.condition) != int else None

        if type(self.condition) == int:
            return self.then if self.condition else self.alternative


@dataclass
class WhileNode(Node):
    condition: 'ComparisonNode'
    do: list['Statement']

    def optimized(self):
        # if possible => reduce condition
        self.condition = self.condition.optimized() if type(self.condition) != int else None

        self.do = [do.optimized() for do in self.do]

        return self


@dataclass
class InputNode(Node):
    ...


@dataclass
class AssignmentNode(Node):
    var: UseNode
    value: InputNode | Expression

    def optimized(self):
        self.value = self.value.optimized() if type(self.value) != int else None

        return self


Statement = WriteNode | IfThenElseNode | WhileNode | AssignmentNode | CallNode


### Container Nodes

@dataclass
class ClassNode(Node):
    name: str
    attrs: list[AttributeDefNode]
    methods: list[MethodDefNode]

    def optimized(self):
        self.methods = [method.optimized() for method in self.methods]
        return self


@dataclass
class ProgramNode(Node):
    classes: list[ClassNode]
    funcs: list[FunctionDefNode]
    vars_: list[VariableDefNode]
    stmts: list[Statement]

    def optimized(self):
        self.classes = [class_.optimized() for class_ in self.classes]
        self.funcs = [func.optimized() for func in self.funcs]
        self.stmts = [stmt.optimized() for stmt in self.stmts]
        return self
