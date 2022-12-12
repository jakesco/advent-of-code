from __future__ import annotations

from copy import deepcopy
from dataclasses import dataclass

from .shared import Grid, P, Solution


@dataclass
class Path:
    current: P
    previous: list[P]
    cost: float


def main(input_: list[str]) -> Solution:
    grid = Grid.from_lines(input_, convert=str)
    start, end = find_start_and_end(grid)
    grid[start] = "a"
    grid[end] = "z"

    shortest_path = None
    init = Path(start, [], 0)
    visited: set[P] = set()
    q: list[Path] = [init]

    while q:
        candidate = q.pop()

        if candidate.current in visited:
            continue

        if candidate.current == end:
            shortest_path = candidate
            break

        for node in grid.neighbors(candidate.current):
            if node in visited:
                continue

            if not one_step_up(grid.get(candidate.current), grid.get(node)):
                continue

            cost = end.dist(node) + candidate.cost
            previous = deepcopy(candidate.previous)
            previous.append(candidate.current)
            new_path = Path(node, previous, cost)
            q.append(new_path)

        visited.add(candidate.current)
        q.sort(key=lambda p: p.cost, reverse=True)

    render_path(shortest_path, grid)
    part1 = len(shortest_path.previous) or 0
    return Solution(part1)


def find_start_and_end(grid: Grid) -> tuple[P, P]:
    start = end = P(0, 0)
    for point, height in grid.items():
        if height == "S":
            start = point
        if height == "E":
            end = point
    return start, end


def next_step(grid: Grid, current: P, target: P) -> P:
    candidates = [
        n
        for n in grid.neighbors(current)
        if one_step_up(grid[current], grid.get(n, "a"))
    ]
    if target in candidates:
        return target
    direction = [(n, target.diff(n).mag()) for n in candidates]
    return sorted(direction, key=lambda x: x[1])[0][0]


def one_step_up(start: str, end: str) -> bool:
    if start is None or end is None:
        return False
    start = str_to_ascii(start)
    end = str_to_ascii(end)
    return end <= start + 1


def str_to_ascii(char: str) -> int:
    return int.from_bytes(bytes(char, "ascii"), byteorder="big")


def render_path(path: Path, grid: Grid):
    g = deepcopy(grid)
    for p in path.previous:
        g[p] = "."
    g[path.current] = "."
    print(g)
