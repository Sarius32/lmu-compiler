from __future__ import annotations

from abc import ABC
from dataclasses import dataclass
from typing import Iterable

from lexer import Operator
from utils import only_reachable_code


class Node(ABC):
    def get_pre_evaluated(self):
        """ Function prototype to evaluate expressions that are numeric only """
        return self

    def get_reachable_only(self):
        """ Function prototype to remove nodes that are unreachable """
        return self


### Expression Nodes

@dataclass
class OperationNode(Node):
    left: Expression
    operation: Operator
    right: Expression

    def get_pre_evaluated(self):
        """ Evaluates if both sides can be reduced into int => just returns the int result (ELSE keeps reduced node) """
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
        """ Tries to evaluate the condition into int and return the inverted value if possible """
        # if possible => reduce condition
        self.condition = self.condition.get_pre_evaluated() if type(self.condition) != int else self.condition

        # if possible directly calculate the result and replace the node
        return int(not bool(self.condition)) if type(self.condition) == int else self


@dataclass
class ComparisonNode(Node):
    left: Expression
    operation: Operator
    right: Expression

    def get_pre_evaluated(self):
        """ Evaluates if both sides can be reduced into int => just returns the int result (ELSE keeps reduced node) """
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
        self.vars_ = [var.get_pre_evaluated() for var in self.vars_]
        self.stmts = [stmt.get_pre_evaluated() for stmt in self.stmts]
        return self

    def get_reachable_only(self):
        self.stmts = only_reachable_code(self.stmts)
        if ReturnNode in [type(t) for t in self.stmts]:
            ret_loc = [type(t) for t in self.stmts].index(ReturnNode)
            self.stmts = self.stmts[:ret_loc + 1]

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

    def get_pre_evaluated(self):
        self.expr = self.expr.get_pre_evaluated() if self.expr is not None and type(self.expr) != int else self.expr
        return self


@dataclass
class IfThenElseNode(Node):
    condition: ComparisonNode | InvertNode
    then: list[Statement]
    alternative: list[Statement]

    def get_pre_evaluated(self):
        self.condition = self.condition.get_pre_evaluated() if type(self.condition) != int else self.condition
        self.then = [stmt.get_pre_evaluated() for stmt in self.then]
        self.alternative = [stmt.get_pre_evaluated() for stmt in self.alternative]
        return self

    def get_reachable_only(self):
        self.then = only_reachable_code(self.then)
        if ReturnNode in [type(t) for t in self.then]:
            ret_loc = [type(t) for t in self.then].index(ReturnNode)
            self.then = self.then[:ret_loc + 1]

        self.alternative = only_reachable_code(self.alternative)
        if ReturnNode in [type(t) for t in self.alternative]:
            ret_loc = [type(t) for t in self.alternative].index(ReturnNode)
            self.alternative = self.alternative[:ret_loc + 1]

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
        self.stmts = [stmt.get_pre_evaluated() for stmt in self.stmts]
        return self

    def get_reachable_only(self):
        self.stmts = only_reachable_code(self.stmts)
        if ReturnNode in [type(t) for t in self.stmts]:
            ret_loc = [type(t) for t in self.stmts].index(ReturnNode)
            self.stmts = self.stmts[:ret_loc + 1]

        return self


@dataclass
class WhileNode(Node):
    condition: InvertNode | ComparisonNode
    do: list[Statement]

    def get_pre_evaluated(self):
        self.condition = self.condition.get_pre_evaluated() if type(self.condition) != int else self.condition
        self.do = [stmt.get_pre_evaluated() for stmt in self.do]
        return self

    def get_reachable_only(self):
        self.do = only_reachable_code(self.do)
        if ReturnNode in [type(t) for t in self.do]:
            ret_loc = [type(t) for t in self.do].index(ReturnNode)
            self.do = self.do[:ret_loc + 1]

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

    def get_reachable_only(self):
        self.methods = [method.get_reachable_only() for method in self.methods]
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
        self.vars_ = [var.get_pre_evaluated() for var in self.vars_]
        self.stmts = [stmt.get_pre_evaluated() for stmt in self.stmts]
        return self

    def get_reachable_only(self):
        self.classes = [class_.get_reachable_only() for class_ in self.classes]
        self.funcs = [func.get_reachable_only() for func in self.funcs]

        self.stmts = only_reachable_code(self.stmts)
        if ReturnNode in [type(t) for t in self.stmts]:
            ret_loc = [type(t) for t in self.stmts].index(ReturnNode)
            self.stmts = self.stmts[:ret_loc + 1]

        return self


### helper functions for matching

def is_expression(value: Expression) -> bool:
    return isinstance(value, (OperationNode, UseNode, CallNode, int)) or is_useNode(value)


def is_useNode(value: UseNode) -> bool:
    return isinstance(value, (ArgUseNode, AttrUseNode, VarUseNode, InstAttrUseNode))
