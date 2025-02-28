
from dataclasses import dataclass

@dataclass
class VariableStore:
    name: str
    address: int
    base: int
    type_: str


@dataclass
class TypeStore:
    name: str
    size: int
    #components? variables?
