from __future__ import annotations

from dataclasses import dataclass
from itertools import combinations

from aoc.utils.interfaces import Solution


@dataclass(frozen=True, eq=True)
class Point:
    x: int
    y: int

    def manhattan_distance(self, p: Point) -> int:
        return abs(self.x - p.x) + abs(self.y - p.y)


def main(puzzle_input: list[str]) -> Solution:
    galaxy_positions_1 = parse_graph(puzzle_input)
    galaxy_positions_2 = parse_graph(puzzle_input, scale=1_000_000)
    return Solution(
        sum(
            g1.manhattan_distance(g2)
            for g1, g2 in combinations(galaxy_positions_1, r=2)
        ),
        sum(
            g1.manhattan_distance(g2)
            for g1, g2 in combinations(galaxy_positions_2, r=2)
        ),
    )


def parse_graph(lines: list[str], scale: int = 2) -> list[Point]:
    blank_rows = [idx for idx, line in enumerate(lines) if all(c == "." for c in line)]
    blank_cols = []
    for col in range(len(lines[0])):
        if all(lines[row][col] == "." for row in range(len(lines))):
            blank_cols.append(col)

    galaxy_positions = []
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if c == ".":
                continue
            adj_x, adj_y = x, y
            for blank in blank_rows:
                if y > blank:
                    adj_y += scale - 1
            for blank in blank_cols:
                if x > blank:
                    adj_x += scale - 1
            galaxy_positions.append(Point(adj_x, adj_y))
    return galaxy_positions


if __name__ == "__main__":
    sample = """...#......
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
    s = main(sample.splitlines())
    print(s)
    assert s.part1 == 374
    a = sum(
        g1.manhattan_distance(g2)
        for g1, g2 in combinations(parse_graph(sample.splitlines(), scale=10), r=2)
    )
    assert a == 1030
    b = sum(
        g1.manhattan_distance(g2)
        for g1, g2 in combinations(parse_graph(sample.splitlines(), scale=100), r=2)
    )
    assert b == 8410
