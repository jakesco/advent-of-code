import math

from .shared import Grid, P, Solution


def main(input_: list[str]) -> Solution:
    grid = Grid.from_lines(input_, convert=str)
    start, end = find_start_and_end(grid)
    grid[start] = "a"
    grid[end] = "z"

    paths = dijkstra_shortest_path(grid, end)
    part1 = paths[start]
    part2 = min([dist for node, dist in paths.items() if grid[node] == "a"])

    return Solution(part1, part2)


def find_start_and_end(grid: Grid) -> tuple[P, P]:
    start = end = P(0, 0)
    for point, height in grid.items():
        if height == "S":
            start = point
        if height == "E":
            end = point
    return start, end


def dijkstra_shortest_path(grid: Grid, start: P) -> dict[P, int]:
    distances = {p: math.inf for p in grid.keys()}
    distances[start] = 0

    q = list(distances.keys())
    while q:
        q.sort(key=lambda node: distances[node], reverse=True)
        current = q.pop()

        for adj in adjacent(grid, current):
            new_dist = distances[current] + 1
            if new_dist < distances[adj]:
                distances[adj] = new_dist

    return distances


def adjacent(grid: Grid, point: P) -> list[P]:
    height = str_to_ascii(grid[point])
    neighbors = []
    for node, h in grid.neighbors(point).items():
        if h is None:
            continue
        if str_to_ascii(h) >= height - 1:
            neighbors.append(node)
    return neighbors


def str_to_ascii(char: str) -> int:
    return int.from_bytes(bytes(char, "ascii"), byteorder="big")
