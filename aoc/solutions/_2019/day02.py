from aoc.utils.intcode import IntcodeInterpreter
from aoc.utils.interfaces import Solution

MAGIC_NUMBER = 19_690_720


def main(puzzle_input: list[str]) -> Solution:
    tape = puzzle_input[0]
    interpreter = IntcodeInterpreter.loads(tape, 12, 2)

    part1 = interpreter.execute()
    part2 = solution_search(interpreter)

    return Solution(part1, part2)


def solution_search(interpreter: IntcodeInterpreter) -> int:
    for i in range(100):
        for j in range(100):
            interpreter.set_args(i, j)
            candidate = interpreter.execute()
            if candidate == MAGIC_NUMBER:
                return 100 * i + j
