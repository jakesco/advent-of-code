from aoc.utils.intcode import IntcodeInterpreter
from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    interpreter = IntcodeInterpreter.loads(puzzle_input[0])
    interpreter.input = [1]
    interpreter.execute()
    part1 = interpreter.output[-1]
    interpreter.reset()
    interpreter.input = [5]
    interpreter.execute()
    part2 = interpreter.output[-1]
    return Solution(part1, part2)


if __name__ == "__main__":
    print(main(["Hello,", "World!"]))
