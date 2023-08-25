from aoc.utils.intcode import IntcodeInterpreter
from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    interpreter = IntcodeInterpreter.loads(puzzle_input[0])
    interpreter.input = [1]
    interpreter.execute()
    part1 = interpreter.output.pop()
    interpreter.reset()
    interpreter.input = [5]
    interpreter.execute()
    part2 = interpreter.output.pop()
    return Solution(part1, part2)
