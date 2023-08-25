import copy
from dataclasses import dataclass, field
from sys import stderr
from typing import Self


class UnknownOpcode(Exception):
    pass


@dataclass
class Op:
    code: int
    modes: tuple[int, int, int]

    @classmethod
    def from_int(cls, n: int) -> Self:
        op = f"{n:05}"
        modes = tuple(int(m) for m in reversed(op[:3]))
        return cls(
            code=int(op[-2:]),
            modes=modes,
        )


@dataclass
class IntcodeInterpreter:
    tape: list[int]
    debug: bool = False
    input: list[int] = field(default_factory=list)
    output: list[int] = field(default_factory=list)
    _p: int = 0
    _memory: list[int] = field(init=False)
    _halted: bool = False

    def __post_init__(self):
        self._memory = copy.copy(self.tape)

    @classmethod
    def loads(cls, tape: str, *args, debug: bool = False) -> Self:
        try:
            program = [int(n) for n in tape.split(",")]
        except ValueError as e:
            print("Invalid intcode program.", file=stderr)
            raise e
        interpreter = cls(program, debug=debug)
        if args:
            interpreter.set_args(*args)
        return interpreter

    def set_args(self, *args):
        for idx, n in enumerate(args, start=1):
            self.tape[idx] = int(n)
        self.reset()

    def result(self) -> int:
        return self.output[0]

    def reset(self):
        self.output = []
        self.input = []
        self._halted = False
        self._p = 0
        self._memory = copy.copy(self.tape)

    def execute(self) -> int:
        if self.debug:
            print(f"Starting: {self}")
        while not self._halted:
            self._run_instruction()
        return self._memory[0]

    def _run_instruction(self):
        opcode = self._next_opcode()
        if self.debug:
            print(f"Running {opcode} | Output: {self.output}")
        match opcode:
            case Op(1, (m0, m1, _)):
                x = self._get_arg(1, m0)
                y = self._get_arg(2, m1)
                z = self._get_arg(3)
                self._memory[z] = x + y
                self._p += 4
            case Op(2, (m0, m1, _)):
                x = self._get_arg(1, m0)
                y = self._get_arg(2, m1)
                z = self._get_arg(3)
                self._memory[z] = x * y
                self._p += 4
            case Op(3, _):
                x = self._get_arg(1)
                self._memory[x] = self.input.pop(0)
                self._p += 2
            case Op(4, (m0, _, _)):
                x = self._get_arg(1, m0)
                self.output.append(x)
                self._p += 2
            case Op(5, (m0, m1, _)):
                x = self._get_arg(1, m0)
                y = self._get_arg(2, m1)
                if x != 0:
                    self._p = y
                else:
                    self._p += 3
            case Op(6, (m0, m1, _)):
                x = self._get_arg(1, m0)
                y = self._get_arg(2, m1)
                if x == 0:
                    self._p = y
                else:
                    self._p += 3
            case Op(7, (m0, m1, _)):
                x = self._get_arg(1, m0)
                y = self._get_arg(2, m1)
                z = self._get_arg(3)
                if x < y:
                    self._memory[z] = 1
                else:
                    self._memory[z] = 0
                self._p += 4
            case Op(8, (m0, m1, _)):
                x = self._get_arg(1, m0)
                y = self._get_arg(2, m1)
                z = self._get_arg(3)
                if x == y:
                    self._memory[z] = 1
                else:
                    self._memory[z] = 0
                self._p += 4
            case Op(99, _):
                self._halted = True
            case _:
                raise UnknownOpcode(opcode)

    def _get_arg(self, offset: int, mode: int = 1) -> int:
        immediate = self._memory[self._p + offset]
        return immediate if mode == 1 else self._memory[immediate]

    def _next_opcode(self) -> Op:
        return Op.from_int(self._memory[self._p])


if __name__ == "__main__":
    tape = "1002,4,3,4,33"
    i = IntcodeInterpreter.loads(tape, debug=True)
    i.execute()
