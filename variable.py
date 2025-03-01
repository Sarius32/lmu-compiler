
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
    parameter: dict[str, tuple[int, str]] #name: (offset, type)
    variables: dict[str, VariableStore]


@dataclass
class TypeStore:
    name: str
    size: int
    instance_vars: dict[str, tuple[int, str]] #name: (offset, type)
    methods: dict[str, MethodStore] #name adresse
