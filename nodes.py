from abc import ABC
from dataclasses import dataclass

from lexer_new import Operator


class Node(ABC):
    def optimized(self):
        return self

@dataclass
class ProgramNode:
    classes: list
    funcs: list
    vars_: list
    stmts: list


@dataclass
class ClassNode(Node):
    name: str
    vars_: list
    funcs: list

    def optimized(self):
        funcs = []
        for func in self.funcs:
            funcs.append(func.optimized())
        self.funcs = funcs

        return self


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
            new_stmt = stmt.optimized()
            if new_stmt:  # don't append if stmt collapsed into no statements
                stmts.append(new_stmt) if type(new_stmt) != list else [stmts.append(stmt_) for stmt_ in new_stmt]
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

    def optimized(self):
        # if possible => reduce condition
        if type(self.condition) != int:
            self.condition = self.condition.optimized()

        if type(self.condition) == int:
            return self.then if self.condition else self.alternative


@dataclass
class WhileNode(Node):
    condition: any
    do: list


@dataclass
class InvertNode(Node):
    condition: any

    def optimized(self):
        # if possible => reduce condition
        if type(self.condition) != int:
            self.condition = self.condition.optimized()

        # if possible directly calculate the result and replace the node
        if type(self.condition) == int:
            return int(not self.condition)

        return self


@dataclass
class ComparisonNode(Node):
    left: any
    operation: any
    right: any

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
        if type(self.left) != int:
            self.left = self.left.optimized()
        if type(self.right) != int:
            self.right = self.right.optimized()

        # if possible directly calculate the result and replace the node
        if type(self.left) == int and type(self.right) == int:
            return int(op_dict[self.operation](self.left, self.right))

        return self


@dataclass
class InputNode(Node):
    ...


@dataclass
class WriteNode(Node):
    expr: any
