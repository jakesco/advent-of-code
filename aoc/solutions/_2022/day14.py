import re
from itertools import pairwise

from .shared import Grid, P, Solution

DOWN = P(0, 1)
DOWN_LEFT = P(-1, 1)
DOWN_RIGHT = P(1, 1)

BASICALLY_INF = 10_000


def main(input_: list[str]) -> Solution:
    sand_source = P(0, 0)  # 500,0 shift everything left 500
    grid = parse_rock_formations(input_)
    grid[sand_source] = "+"

    # Part 1
    done = False
    while not done:
        done = drop_sand(grid, sand_source, part=1)
    part1 = len([p for p, v in grid.items() if v == "o"])

    # Add floor
    floor = P(-BASICALLY_INF, grid.max_y() + 2).line(P(BASICALLY_INF, grid.max_y() + 2))
    for p in floor:
        grid[p] = "#"

    # Part 2
    done = False
    while not done:
        done = drop_sand(grid, sand_source, part=2)
    part2 = len([p for p, v in grid.items() if v == "o"])

    return Solution(part1, part2)


def parse_rock_formations(scans: list[str]) -> Grid:
    grid = Grid()
    for scan in scans:
        points = [
            P(*map(int, p.split(","))).add(P(-500, 0))
            for p in re.findall(r"\d+,\d+", scan)
        ]
        rock = []
        for a, b in pairwise(points):
            rock.extend(a.line(b))
        for r in rock:
            grid[r] = "#"
    return grid


def drop_sand(grid: Grid, source: P, part: int) -> bool:
    at_rest = False
    current = source
    while not at_rest:
        if part == 1 and current.y > grid.max_y():
            return True

        if part == 2 and grid.get(source) == "o":
            return True

        next_ = current.add(DOWN)
        if grid.get(next_) is None:
            current = next_
            continue
        next_ = current.add(DOWN_LEFT)
        if grid.get(next_) is None:
            current = next_
            continue
        next_ = current.add(DOWN_RIGHT)
        if grid.get(next_) is None:
            current = next_
            continue
        at_rest = True

    grid[current] = "o"
    return False
