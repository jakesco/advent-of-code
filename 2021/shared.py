from dataclasses import dataclass


@dataclass
class Solution:
    part1: int | str = 0
    part2: int | str = 0

    def __str__(self):
        return f"Part 1: {self.part1}\nPart 2: {self.part2}"
