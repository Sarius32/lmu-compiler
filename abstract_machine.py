from typing import Literal, get_origin, get_args
from typing import get_args as get_literal_args
from inspect import get_annotations as get_expect_args


class AbstractMachine:
    _storemax = 1_000_000
    _store = [None] * _storemax

    P, T, B = 0, 0, 0

    def __init__(self, file_path: str):
        # init translation of instructions
        self._instr_dict = {
            "RST": self._reset,
            "LOD": self._load,
            "STO": self._store_,
            "INC": self._inc,
            "LIT": self._literal,
            "JMP": self._jump,
            "JOT": self._jump_on_true,
            "JOF": self._jump_on_false,
            "CAL": self._call,
            "RET": self._return,
            "OPR": self._operator,
            "REA": self._read,
            "WRI": self._write,
            "HLT": None
        }

        try:
            with open(file_path) as file:
                code = file.readlines()
        except OSError:
            print(f"Error occurred during reading file at location: {file_path}")
            exit(-1)

        self._convert_to_exec(code)

    def _convert_to_exec(self, code: list[str]):
        self._code = []

        for row, line in enumerate(code):
            row += 1  # 1-indexed

            instr_tuple = line.split("#")[0].strip().split(" ")
            if len(instr_tuple) > 3:
                print(f"Malformed code found in OBJECT code at")
                print(f"    line {str(row).rjust(4)}:    {line}")
                exit(-1)

            instr, args = instr_tuple[0], instr_tuple[1:]
            instr_func = self._instr_dict.get(instr, -1)
            if instr_func == -1:
                print(f"Unknown instruction '{instr}' found in OBJECT code at")
                print(f"    line {str(row).rjust(4)}:    {line}")
                print(f"               ‾‾‾{"^" * len(instr)}‾‾‾")
                exit(-1)

            if instr_func is None:
                self._code.append(None)
                if row != len(code):  # row now 1-indexed
                    print(f"Instruction 'HLT' found before EOF in OBJECT code at")
                    print(f"    line {str(row).rjust(4)}:    {line}")
                    exit(-1)
                else:
                    return

            arg_dict = get_expect_args(instr_func)
            num_args, arg_types = len(arg_dict), list(arg_dict.values())
            if len(args) > num_args:
                len_args = len(line.replace(instr + " ", ""))
                print(
                    f"Too many arguments for instruction '{instr}' ({num_args} expected, {len(args)} found) found in OBJECT code at")
                print(f"    line {str(row).rjust(4)}:    {line}")
                print(f"               {" " * len(instr)} ‾‾‾{"^" * len_args}‾‾‾")

                exit(-1)
            if len(args) < num_args:
                print(
                    f"Too few arguments for instruction '{instr}' ({num_args} expected, {len(args)} found) found in OBJECT code at")
                print(f"    line {str(row).rjust(4)}:    {line}")
                exit(-1)

            conv_args = []
            for idx, (arg, type_to_be) in enumerate(zip(args, arg_types)):
                # type is just one type
                if type(type_to_be) is type:
                    try:
                        conv_args.append(type_to_be(arg))
                    except ValueError:
                        print(
                            f"Error occurred during argument conversion ({type_to_be.__name__} expected, {type(arg).__name__} found) in OBJECT code at")
                        print(f"    line {str(row).rjust(4)}:    {line}")
                        space_len = len(instr) + 1 + ((len(args[0]) + 1) if idx == 1 else 0)
                        print(f"               {" " * space_len}‾‾‾{"^" * len(arg)}‾‾‾")
                        exit(-1)
                # type is something like ["<", ">", ...]
                elif get_origin(type_to_be) is Literal:
                    if arg in get_literal_args(type_to_be):
                        conv_args.append(arg)
                    else:
                        print(
                            f"Unexpected Literal '{arg}' for instruction '{instr}' found in OBJECT code at")
                        print(f"    line {str(row).rjust(4)}:    {line}")
                        space_len = len(instr) + 1 + ((len(args[0]) + 1) if idx == 1 else 0)
                        print(f"               {" " * space_len}‾‾‾{"^" * len(arg)}‾‾‾")
                        exit(-1)

            self._code.append((instr_func, conv_args))

    def _base(self, s: int) -> int:
        A = self.B
        for _ in range(s):
            A = self._store[A + 2]
        return A

    def _reset(self):
        self.B = 0
        self._store[self.B + 0] = 0
        self._store[self.B + 1] = 0
        self._store[self.B + 2] = 0
        self.T = self.B + 2

    def _load(self, s: int, i: int):
        self.T += 1
        self._store[self.T] = self._store[self._base(s) + i]

    def _store_(self, s: int, i: int):
        self._store[self._base(s) + i] = self._store[self.T]
        self.T = self.T - 1

    def _inc(self, i: int):
        self.T += i

    def _literal(self, n: int):
        self.T += 1
        self._store[self.T] = n

    def _jump(self, a: int):
        self.P = a

    def _jump_on_true(self, a: int):
        if self._store[self.T] == int(True):
            self.P = a
        self.T -= 1

    def _jump_on_false(self, a: int):
        if self._store[self.T] == int(False):
            self.P = a
        self.T -= 1

    def _call(self, s: int, a: int):
        self.T += 1
        self._store[self.T] = self.B
        self._store[self.T + 1] = self.P
        self._store[self.T + 2] = self._base(s)
        self.B = self.T
        self.T = self.B + 2
        self.P = a

    def _return(self):
        self.P = self._store[self.B + 1]
        self.T = self.B - 1
        self.B = self._store[self.B]

    def _operator(self, op: Literal["!", "=", "<", ">", "+", "-", "*", "/"]):
        if op == "!":
            self._store[self.T] = int(not self._store[self.T])
            return

        if op == "=": self._store[self.T - 1] = int(self._store[self.T - 1] == self._store[self.T])
        if op == "<": self._store[self.T - 1] = int(self._store[self.T - 1] < self._store[self.T])
        if op == ">": self._store[self.T - 1] = int(self._store[self.T - 1] > self._store[self.T])
        if op == "+": self._store[self.T - 1] = int(self._store[self.T - 1] + self._store[self.T])
        if op == "-": self._store[self.T - 1] = int(self._store[self.T - 1] - self._store[self.T])
        if op == "*": self._store[self.T - 1] = int(self._store[self.T - 1] * self._store[self.T])
        if op == "/": self._store[self.T - 1] = int(self._store[self.T - 1] / self._store[self.T])
        self.T -= 1

    def _read(self):
        self.T += 1
        self._store[self.T] = int(input())

    def _write(self):
        print(self._store[self.T])
        self.T -= 1

    def run(self):
        i = self._code[self.P]

        while i is not None:
            self.P += 1
            instruction, args = i
            instruction(*args)
            i = self._code[self.P]


if __name__ == "__main__":
    machine = AbstractMachine("test_object_code.loc")
    machine.run()
