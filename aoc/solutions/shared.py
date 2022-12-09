from __future__ import annotations

from dataclasses import dataclass
from itertools import product
from typing import Any, Callable


@dataclass
class Solution:
    part1: int | str = 0
    part2: int | str = 0

    def __str__(self):
        return f"Part 1: {self.part1}\nPart 2: {self.part2}"


@dataclass(frozen=True, slots=True, order=True)
class P:
    x: int
    y: int

    def add(self, p: P) -> P:
        return P(self.x + p.x, self.y + p.y)

    def diff(self, p: P) -> P:
        return P(self.x - p.x, self.y - p.y)

    def mag(self) -> float:
        return abs(complex(self.x, self.y))


_NEIGHBORS = [P(0, -1), P(-1, 0), P(1, 0), P(0, 1)]
_DIAG_NEIGHBORS = [
    P(x, y) for x, y in product((-1, 0, 1), (-1, 0, 1)) if (x, y) != (0, 0)
]


class Grid(dict):
    def __init__(self, *args, **kwargs):
        super(Grid, self).__init__(*args, **kwargs)

    @property
    def cols(self) -> int:
        return len({p.x for p in self.keys()})

    @property
    def rows(self) -> int:
        return len({p.y for p in self.keys()})

    @classmethod
    def from_lines(
        cls, lines: list[str], convert: Callable[[str], Any] = int
    ) -> Grid[P, Any]:
        grid = cls()
        for y, line in enumerate(lines):
            for x, point in enumerate(line):
                grid[P(x, y)] = convert(point)
        return grid

    def __str__(self) -> str:
        return "\n".join(
            [
                "".join([str(self.get(P(x, y), " ")) for x in range(self.cols)])
                for y in range(self.rows)
            ]
        )

    def row(self, y: int) -> list[Any]:
        return [value for point, value in self.items() if point.y == y]

    def col(self, x: int) -> list[Any]:
        return [value for point, value in self.items() if point.x == x]

    def neighbors(self, p: P, *, diag: bool = False) -> dict[P, Any]:
        neigh = (p.add(point) for point in (_DIAG_NEIGHBORS if diag else _NEIGHBORS))
        return {point: self.get(point) for point in neigh}

    def is_edge(self, p: P) -> bool:
        neigh = self.neighbors(p, diag=True)
        return any(n is None for n in neigh.values())
