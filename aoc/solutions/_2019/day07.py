from aoc.utils.intcode import IntcodeInterpreter
from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    machine = IntcodeInterpreter.loads(puzzle_input[0])
    sequence = [4, 3, 2, 1, 0]
    out = 0
    for s in sequence:
        out = run_amp(machine, s, out)
    return Solution(out)


def run_amp(m: IntcodeInterpreter, phase: int, in0: int) -> int:
    m.reset()
    m.input = [phase, in0]
    m.execute()
    return m.output.pop(0)


if __name__ == "__main__":
    print(main(["3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"]))
