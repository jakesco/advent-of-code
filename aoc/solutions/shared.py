from contextvars import ContextVar
from dataclasses import dataclass

verbose = ContextVar("verbose", default=False)


@dataclass
class Solution:
    part1: int = 0
    part2: int = 0

    def __str__(self):
        return f"Part 1: {self.part1}\nPart 2: {self.part2}"
