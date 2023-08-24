from __future__ import annotations

import copy
from dataclasses import dataclass
from sys import stderr


class UnknownOpcode(Exception):
    pass


@dataclass
class IntcodeInterpreter:
    tape: list[int]
    debug: bool = False
    _p: int = 0
    _memory: list[int] = None
    _halted: bool = False

    def __post_init__(self):
        self._memory = copy.copy(self.tape)

    @classmethod
    def loads(cls, tape: str, *args, debug: bool = False) -> IntcodeInterpreter:
        try:
            program = [int(n) for n in tape.split(",")]
        except ValueError as e:
            print("Invalid intcode program.", file=stderr)
            raise e
        interpreter = IntcodeInterpreter(program, debug=debug)
        if args:
            interpreter.set_args(*args)
        return interpreter

    def set_args(self, *args):
        for idx, n in enumerate(args, start=1):
            self.tape[idx] = int(n)
        self.reset()

    def result(self) -> int:
        return self._memory[0]

    def reset(self):
        self._halted = False
        self._p = 0
        self._memory = copy.copy(self.tape)

    def execute(self) -> int:
        if self.debug:
            print(f"Starting: {self}")
        while not self._halted:
            self._run_instruction()
            if self.debug:
                print(f"Mem: {self._memory}, Halted: {self._halted}")
        return self._memory[0]

    def _run_instruction(self):
        instruction = self._memory[self._p : self._p + 4]
        if self.debug:
            print(f"Running {instruction}")
        match instruction:
            case [1, x, y, z]:
                self._memory[z] = self._memory[x] + self._memory[y]
                self._p += 4
            case [2, x, y, z]:
                self._memory[z] = self._memory[x] * self._memory[y]
                self._p += 4
            case [99, *_]:
                self._halted = True
            case _:
                raise UnknownOpcode(instruction[0])


if __name__ == "__main__":
    i = IntcodeInterpreter.loads("1,9,10,3,2,3,11,0,99,30,40,50", debug=True)
    i.execute()
