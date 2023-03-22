from __future__ import annotations

from dataclasses import dataclass

from aoc.utils.interfaces import Solution


@dataclass
class Box:
    l: int
    w: int
    h: int

    def surface_area(self) -> int:
        return 2 * self.l * self.w + 2 * self.w * self.h + 2 * self.h * self.l

    def smallest_side(self) -> int:
        return min(self.l * self.w, self.w * self.h, self.h * self.l)

    def smallest_perimeter(self) -> int:
        return min(2 * (self.l + self.w), 2 * (self.w + self.h), 2 * (self.h + self.l))

    def volume(self) -> int:
        return self.l * self.w * self.h

    @classmethod
    def from_str(cls, box_str: str) -> Box:
        l, w, h = map(int, box_str.split("x"))
        return cls(l, w, h)


def main(puzzle_input: list[str]) -> Solution:
    boxes = [Box.from_str(box_str) for box_str in puzzle_input]
    part1 = sum(box.surface_area() + box.smallest_side() for box in boxes)
    part2 = sum(box.volume() + box.smallest_perimeter() for box in boxes)
    return Solution(part1, part2)
