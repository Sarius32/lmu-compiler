
from dataclasses import dataclass

@dataclass
class VariableStore:
    name: str
    address: int
    base: int
    type_: str


@dataclass
class MethodStore:
    name: str
    adress: int
    arguments: dict[str, tuple[int, str]] #name: (offset, type)
    variables: dict[str, VariableStore]


@dataclass
class TypeStore:
    name: str
    size: int
    instance_vars: dict[str, VariableStore]
    methods: dict[str, MethodStore] #name adresse


@dataclass
class ProgramStore:
    types: dict[str, TypeStore]
    variables: dict[str, VariableStore]
    functions: dict[str, MethodStore]