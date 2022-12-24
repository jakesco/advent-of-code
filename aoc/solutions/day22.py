import re
from collections import deque
from itertools import chain, zip_longest

from .shared import Grid, P, Solution

FACING = deque(
    [
        P(1, 0),  # right
        P(0, -1),  # down
        P(-1, 0),  # left
        P(0, 1),  # up
    ]
)

FACING_TOKEN = {
    P(1, 0): ">",
    P(0, 1): "v",
    P(-1, 0): "<",
    P(0, -1): "^",
}

FACING_VALUE = {
    P(1, 0): 0,
    P(0, 1): 1,
    P(-1, 0): 2,
    P(0, -1): 3,
}


def main(input_: list[str]) -> Solution:
    password, grid = parse_grid(input_)
    part1 = traverse(password, grid)
    part2 = traverse(password, grid, cube=True, debug=True)
    grid.render_graph(range(20), range(12))
    return Solution(part1, part2)


def traverse(
    password: list[int], grid: Grid, cube: bool = False, debug: bool = False
) -> int:
    current = min([p for p in grid.keys() if p.y == 0])
    get_wrap = get_wrap_cube if cube else get_wrap_plane
    for idx, action in enumerate(password, start=1):
        print(current)
        if idx % 2 == 0:
            FACING.rotate(action)
            continue
        for _ in range(action):
            if debug:
                grid[current] = FACING_TOKEN[FACING[0]]
            n = current + FACING[0]
            target = grid.get(n)
            if target is None:
                n = get_wrap(n, grid)
                target = grid.get(n)
            if target == "#":
                break
            current = n
    final_row = current.y + 1
    final_col = current.x + 1
    facing = FACING_VALUE[FACING[0]]
    return 1000 * final_row + 4 * final_col + facing


def get_wrap_plane(n: P, grid: Grid) -> P:
    if FACING[0] == P(1, 0):  # right
        return min([p for p in grid.keys() if p.y == n.y])
    if FACING[0] == P(0, -1):  # down
        return max([p for p in grid.keys() if p.x == n.x])
    if FACING[0] == P(-1, 0):  # left
        return max([p for p in grid.keys() if p.y == n.y])
    if FACING[0] == P(0, 1):  # up
        return min([p for p in grid.keys() if p.x == n.x])


def get_wrap_cube(n: P, grid: Grid) -> P:
    return get_wrap_plane(n, grid)


def parse_grid(lines: list[str]) -> (list[int], Grid):
    grid = Grid()
    line = lines.pop()
    dist = map(int, re.findall(r"\d+", line))
    dirs = map(lambda x: 1 if x == "R" else -1, re.findall(r"[A-Z]", line))
    password = [x for x in chain(*zip_longest(dist, dirs)) if x]

    lines.pop()
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char == " ":
                continue
            grid[P(x, y)] = char

    return password, grid
