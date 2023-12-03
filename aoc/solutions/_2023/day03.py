from collections import defaultdict
from collections.abc import Callable
from dataclasses import dataclass
from functools import reduce
from typing import TypeAlias

from aoc.utils.interfaces import Solution

Grid: TypeAlias = Callable[[int, int], [str]]


@dataclass(frozen=True, eq=True)
class Symbol:
    glyph: str
    x: int
    y: int


@dataclass
class Part:
    id: int
    symbol: Symbol


def main(puzzle_input: list[str]) -> Solution:
    rows = len(puzzle_input)
    cols = len(puzzle_input[0])
    grid = gridgetter(puzzle_input)

    parts = []
    number_acc = ""
    adj = None
    for x in range(rows):
        for y in range(cols):
            c = grid(x, y)

            if c.isdigit():
                number_acc += c
                if adj is None:
                    adj = find_adj(grid, x, y)
                continue

            if adj is not None:
                p = Part(int(number_acc), adj)
                parts.append(p)

            number_acc = ""
            adj = None

    part1 = sum([part.id for part in parts])
    part2 = find_gear_ratio(parts)
    return Solution(part1, part2)


def gridgetter(grid: list[str]) -> Grid:
    """Wrapper to return . if out of bounds"""

    def f(x: int, y: int) -> str:
        try:
            return grid[x][y]
        except IndexError:
            return "."

    return f


def find_adj(grid: Grid, x: int, y: int) -> Symbol | None:
    """Checks points adjacent to grid[x][y].
    Returns True if a symbol is adjacent."""
    for i in (-1, 0, 1):
        for j in (-1, 0, 1):
            c = grid(x + i, y + j)
            if c == "." or c.isdigit():
                continue
            return Symbol(c, x + i, y + j)
    return None


def find_gear_ratio(parts: list[Part]) -> int:
    symbol_types = defaultdict(list)
    for part in parts:
        if part.symbol.glyph == "*":
            symbol_types[part.symbol].append(part.id)

    return sum(
        [reduce(lambda x, y: x * y, v, 1) for v in symbol_types.values() if len(v) == 2]
    )


if __name__ == "__main__":
    sample = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
    s = main(sample.split("\n"))
    print(s)
    assert s.part1 == 4361
    assert s.part2 == 467835
