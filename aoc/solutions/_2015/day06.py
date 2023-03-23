from __future__ import annotations

import re
from dataclasses import dataclass

from aoc.utils.interfaces import Solution

# TODO: look into using a z-order curve for this


INSTRUCTION = re.compile(r"turn (on|off) (\d+,\d+) through (\d+,\d+)")


@dataclass
class Instruction:
    action: bool
    start: tuple[int, ...]
    end: tuple[int, ...]

    @classmethod
    def from_string(cls, string: str) -> Instruction:
        action, start, end = INSTRUCTION.fullmatch(string).group(1, 2, 3)
        print(action, start, end)
        start = tuple(map(int, start.split(",")))
        end = tuple(map(int, end.split(",")))
        return cls(action == "on", start, end)


@dataclass
class Grid(list):
    pass


def main(puzzle_input: list[str]) -> Solution:
    print(Instruction.from_string(puzzle_input[0]))
    return Solution()
