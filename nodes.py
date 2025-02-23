from abc import ABC
from dataclasses import dataclass

from lexer_new import Operator


class Node(ABC):
    def optimized(self):
        return self


@dataclass
class ClassNode(Node):
    name: str
    vars_: list
    funcs: list


@dataclass
class VariableDefNode(Node):
    name: str
    value: any

    def optimized(self):
        if type(self.value) != int:
            self.value = self.value.optimized()

        return self


@dataclass
class VariableNode(Node):
    name: str


@dataclass
class FunctionDefNode(Node):
    name: str
    args: list
    vars_: list
    stmts: list
    ret: any

    def optimized(self):
        stmts = []
        for stmt in self.stmts:
            stmts.append(stmt.optimized())
        self.stmts = stmts

        if type(self.ret) != int:
            self.ret = self.ret.optimized()

        return self


@dataclass
class OperationNode(Node):
    left: any
    operation: any
    right: any

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
class AssignmentNode(Node):
    var: VariableNode
    value: any

    def optimized(self):
        if type(self.value) != int:
            self.value = self.value.optimized()

        return self


@dataclass
class FunctionCallNode(Node):
    name: str
    args: list


@dataclass
class IfThenElseNode(Node):
    condition: any
    then: list
    alternative: list


@dataclass
class WhileNode(Node):
    condition: any
    do: list


@dataclass
class InvertNode(Node):
    condition: any


@dataclass
class ComparisonNode(Node):
    left: any
    operation: any
    right: any


@dataclass
class InputNode(Node):
    ...


@dataclass
class WriteNode(Node):
    expr: any
