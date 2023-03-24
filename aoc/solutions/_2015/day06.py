from __future__ import annotations

import enum
import re
from dataclasses import dataclass

from aoc.utils.interfaces import Solution

INSTRUCTION = re.compile(r"^.*(on|off|toggle) (\d+,\d+) through (\d+,\d+)")


class Action(enum.Enum):
    ON = "on"
    OFF = "off"
    TOGGLE = "toggle"


@dataclass
class Instruction:
    action: bool
    start: tuple[int, int]
    end: tuple[int, int]

    @classmethod
    def from_string(cls, string: str) -> Instruction:
        action, start, end = INSTRUCTION.fullmatch(string).group(1, 2, 3)
        start = tuple(map(int, start.split(",")))
        end = tuple(map(int, end.split(",")))
        return cls(Action(action), start, end)


def main(puzzle_input: list[str]) -> Solution:
    instructions = [Instruction.from_string(line) for line in puzzle_input]

    grid1 = [[0 for _ in range(1000)] for _ in range(1000)]
    grid2 = [[0 for _ in range(1000)] for _ in range(1000)]

    for instruction in instructions:
        for x in range(instruction.start[0], instruction.end[0] + 1):
            for y in range(instruction.start[1], instruction.end[1] + 1):
                if instruction.action == Action.ON:
                    grid1[x][y] = 1
                    grid2[x][y] += 1
                elif instruction.action == Action.OFF:
                    grid1[x][y] = 0
                    grid2[x][y] = max(0, grid2[x][y] - 1)
                elif instruction.action == Action.TOGGLE:
                    grid1[x][y] = 1 - grid1[x][y]
                    grid2[x][y] += 2

    part1 = sum(sum(row) for row in grid1)
    part2 = sum(sum(row) for row in grid2)
    return Solution(part1, part2)
