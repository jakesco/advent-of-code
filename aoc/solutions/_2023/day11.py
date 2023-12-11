from __future__ import annotations
from pprint import pprint
from dataclasses import dataclass
from typing import TypeAlias
import math
from itertools import combinations

from aoc.utils.interfaces import Solution

# TODO: need faster algorithm


@dataclass(frozen=True, eq=True)
class Point:
    x: int
    y: int

    def neighbours(self) -> set[Point]:
        return {
            Point(self.x - 1, self.y),
            Point(self.x + 1, self.y),
            Point(self.x, self.y - 1),
            Point(self.x, self.y + 1),
        }


Grid: TypeAlias = dict[Point, str]

def main(puzzle_input: list[str]) -> Solution:
    grid = parse_graph(puzzle_input)
    galaxy_paths = {k: shortest_paths(grid, k) for k, v in grid.items() if v != "."}
    sum_ = 0
    for g1, g2 in combinations(galaxy_paths, r=2):
        sum_ += galaxy_paths[g1][g2]
    return Solution(sum_)


def shortest_paths(grid: Grid, start: Point) -> dict[Point, int]:
    print(f"Checking {start}...")
    distances = {p: math.inf for p in grid}
    distances[start] = 0

    q = list(distances.keys())
    while q:
        q.sort(key=lambda node: distances[node], reverse=True)
        current = q.pop()

        for adj in current.neighbours():
            new_dist = distances[current] + 1
            if new_dist < distances.get(adj, math.inf):
                distances[adj] = new_dist

    return distances


def parse_graph(lines: list[str]) -> Grid:
    # Inflate
    new_rows = [
        idx for idx, line in enumerate(lines) if all(c == '.' for c in line)
    ]

    new_cols = []
    for col in range(len(lines[0])):
        if all(lines[row][col] == '.' for row in range(len(lines))):
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
            if c != '.':
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
