from dataclasses import dataclass


@dataclass
class ClassNode:
    name: str
    vars_: list
    funcs: list


@dataclass
class VariableDefNode:
    name: str
    value: any


@dataclass
class VariableNode:
    name: str


@dataclass
class FunctionDefNode:
    name: str
    args: list
    vars_: list
    stmts: list


@dataclass
class OperationNode:
    left: any
    operation: any
    right: any


@dataclass
class AssignmentNode:
    var: VariableNode
    value: any


@dataclass
class FunctionCallNode:
    name: str
    args: list


@dataclass
class IfThenElseNode:
    condition: any
    then: list
    alternative: list


@dataclass
class WhileNode:
    condition: any
    do: list


@dataclass
class InvertNode:
    condition: any


@dataclass
class ComparisonNode:
    left: any
    operation: any
    right: any


@dataclass
class InputNode:
    ...


@dataclass
class WriteNode:
    expr: any
