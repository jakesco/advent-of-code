from __future__ import annotations

from dataclasses import dataclass
from functools import reduce

from aoc.utils.interfaces import Solution


@dataclass(frozen=True)
class Point:
    x: int = 0
    y: int = 0


@dataclass
class Move:
    dir: str
    dist: int

    @classmethod
    def loads(cls, text: str) -> Move:
        return cls(
            dir=text[0],
            dist=int(text[1:]),
        )


DIRECTIONS = {
    "U": lambda p: Point(p.x, p.y + 1),
    "D": lambda p: Point(p.x, p.y - 1),
    "L": lambda p: Point(p.x - 1, p.y),
    "R": lambda p: Point(p.x + 1, p.y),
}


def main(puzzle_input: list[str]) -> Solution:
    lines = [
        follow_line([Move.loads(move) for move in line.split(",")])
        for line in puzzle_input
    ]
    intersections = reduce(lambda a, b: a.keys() & b.keys(), lines)

    part1 = min([abs(p.x) + abs(p.y) for p in intersections])
    part2 = min(
        sum([line[intersection] for line in lines]) for intersection in intersections
    )

    return Solution(part1, part2)


def follow_line(line: list[Move]) -> dict[Point, int]:
    points: dict[Point, int] = {}
    steps = 0
    current = Point()
    for move in line:
        direction = DIRECTIONS[move.dir]
        for i in range(1, move.dist + 1):
            current = direction(current)
            steps += 1
            points[current] = steps
    return points


if __name__ == "__main__":
    print(main(["R8,U5,L5,D3", "U7,R6,D4,L4"]))
