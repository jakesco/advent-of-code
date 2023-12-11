from __future__ import annotations

from dataclasses import dataclass
from itertools import combinations
from typing import TypeAlias

from aoc.utils.interfaces import Solution

# TODO: find a way to abstract the padding


@dataclass(frozen=True, eq=True)
class Point:
    x: int
    y: int

    def manhattan_distance(self, p: Point) -> int:
        return abs(self.x - p.x) + abs(self.y - p.y)


Grid: TypeAlias = dict[Point, str]


def main(puzzle_input: list[str]) -> Solution:
    grid = parse_graph(puzzle_input)
    galaxy_paths = [k for k, v in grid.items() if v != "."]
    return Solution(
        sum(g1.manhattan_distance(g2) for g1, g2 in combinations(galaxy_paths, r=2)),
    )


def parse_graph(lines: list[str]) -> Grid:
    # Inflate
    new_rows = [idx for idx, line in enumerate(lines) if all(c == "." for c in line)]

    new_cols = []
    for col in range(len(lines[0])):
        if all(lines[row][col] == "." for row in range(len(lines))):
            new_cols.append(col)

    for offset, row in enumerate(new_rows):
        lines.insert(row + offset, "." * len(lines[0]))

    new_lines = []
    for line in lines:
        new_line = [c for c in line]
        for offset, col in enumerate(new_cols):
            new_line.insert(col + offset, ".")
        new_lines.append("".join(new_line))

    # Build graph
    graph = {}
    galaxy = 1
    for y, line in enumerate(new_lines):
        for x, c in enumerate(line):
            if c != ".":
                graph[Point(x, y)] = str(galaxy)
                galaxy += 1
            else:
                graph[Point(x, y)] = c
    return graph


if __name__ == "__main__":
    sample_1 = """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"""
    s = main(sample_1.splitlines())
    print(s)
    assert s.part1 == 374
