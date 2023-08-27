from itertools import permutations

from aoc.utils.intcode import IntcodeInterpreter
from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    part1 = run_part1(puzzle_input[0])
    part2 = run_part2(puzzle_input[0])
    return Solution(part1, part2)


def run_part1(tape: str) -> int:
    machine = IntcodeInterpreter.loads(tape)
    max_ = 0
    for sequence in permutations(range(5), r=5):
        out = 0
        for s in sequence:
            machine.reset()
            machine.input = [s, out]
            machine.execute()
            out = machine.output.pop(0)
        max_ = max(max_, out)
    return max_


def run_part2(tape: str) -> int:
    max_ = 0
    for sequence in permutations(range(5, 10), r=5):
        output = 0
        amp_array = build_amp_array(tape, sequence)
        output = run_array(amp_array)
        max_ = max(max_, output)
    return max_


def build_amp_array(tape: str, sequence: tuple[int, ...]) -> list[IntcodeInterpreter]:
    def fn(tape: str, phase: int) -> IntcodeInterpreter:
        m = IntcodeInterpreter.loads(tape)
        m.input = [phase]
        return m

    return [fn(tape, s) for s in sequence]


def run_array(amp_array: list[IntcodeInterpreter]) -> int:
    output = 0
    while not all([m._halted for m in amp_array]):
        for machine in amp_array:
            machine.input.append(output)
            machine.execute()
            output = machine.output.pop(0)
    return output


if __name__ == "__main__":
    print(
        main(
            [
                "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
            ]
        )
    )
