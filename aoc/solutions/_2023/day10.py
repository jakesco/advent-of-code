from pprint import pprint
from dataclasses import dataclass
from typing import TypeAlias

from aoc.utils.interfaces import Solution

# TODO: Ideas for Part2
#  1. Flood Fill
#  2. Scale up graph 2x
#  3. Check even horizontal line crossings


@dataclass(frozen=True, eq=True)
class Point:
    x: int
    y: int


Graph: TypeAlias = dict[Point, list[Point]]


def main(puzzle_input: list[str]) -> Solution:
    rows, cols = len(puzzle_input), len(puzzle_input[0])
    start, graph = parse_graph(puzzle_input)
    visited = set()
    current = start
    steps = 0
    while start not in visited:
        candidates = graph[current]
        for candidate in candidates:
            if steps < 2 and candidate == start:
                continue
            if candidate in visited:
                continue
            current = candidate
            break
        else:
            raise Exception("Could not find next node.")
        visited.add(current)
        steps += 1

    return Solution(steps // 2)


def parse_graph(lines: list[str]) -> tuple[Point, Graph]:
    start = None
    graph = {}
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            p = Point(x, y)
            n, s, e, w = Point(x, y - 1), Point(x, y + 1), Point(x + 1, y), Point(x - 1, y)
            match c:
                case "|":
                    graph[p] = [n, s]
                case "-":
                    graph[p] = [e, w]
                case "L":
                    graph[p] = [n, e]
                case "J":
                    graph[p] = [n, w]
                case "7":
                    graph[p] = [s, w]
                case "F":
                    graph[p] = [s, e]
                case "S":
                    start = p
                case _:
                    pass

    graph[start] = [
        p for p, connections in graph.items() if start in connections
    ]
    return start, graph


if __name__ == "__main__":
    sample_1 = """.....
.S-7.
.|.|.
.L-J.
....."""
    sample_2 = """..F7.
.FJ|.
SJ.L7
|F--J
LJ..."""
    sample_3 = """...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."""
    sample_4 = """.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."""
    sample_5 = """FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"""
    s = main(sample_1.splitlines())
    assert s.part1 == 4
    s = main(sample_2.splitlines())
    assert s.part1 == 8
    s = main(sample_3.splitlines())
    assert s.part2 == 4
    s = main(sample_4.splitlines())
    assert s.part2 == 8
    s = main(sample_5.splitlines())
    assert s.part2 == 10
    print(s)
