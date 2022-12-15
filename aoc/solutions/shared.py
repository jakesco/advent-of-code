from __future__ import annotations

from dataclasses import dataclass
from itertools import islice, product
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

    def dist(self, other: P) -> float:
        return self.diff(other).mag()

    def m_dist(self, other: P) -> int:
        d = self.diff(other)
        return abs(d.x) + abs(d.y)

    def line(self, b: P) -> list[P]:
        start = min(self, b)
        end = max(self, b)
        if self.x == b.x:
            c = end.diff(start)
            return [P(start.x, start.y + y) for y in range(c.y + 1)]
        if self.y == b.y:
            c = end.diff(start)
            return [P(start.x + x, start.y) for x in range(c.x + 1)]
        raise NotImplementedError("Diagonal lines not supported")

    def points_in_range(self, m_dist: int) -> set[P]:
        """Outputs all points within `range` of Self"""
        points = set()
        for x in range(-m_dist, m_dist + 1):
            for y in range(-m_dist, m_dist + 1):
                p = P(self.x + x, self.y + y)
                dist = self.m_dist(p)
                if dist <= m_dist and dist != 0:
                    points.add(p)
        return points


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

    def max_x(self) -> int:
        return max({p.y for p in self.keys()})

    def max_y(self) -> int:
        return max({p.y for p in self.keys()})

    @classmethod
    def from_lines(
        cls, lines: list[str], convert: Callable[[str], Any] = int
    ) -> Grid[P, Any]:
        grid = cls()
        for y, line in enumerate(lines):
            for x, point in enumerate(line):
                grid[P(x, y)] = convert(point)
        return grid

    def render_graph(self, x_range: range, y_range: range):
        print(
            "\n".join(
                [
                    "".join([str(self.get(P(x, y), ".")) for x in x_range])
                    for y in y_range
                ]
            )
        )
        print()

    def __str__(self) -> str:
        x_min = min([p.x for p in self.keys()]) - 2
        y_min = min([p.y for p in self.keys()])
        return "\n".join(
            [
                "".join(
                    [
                        str(self.get(P(x, y), "."))
                        for x in range(x_min, x_min + self.cols + 4)
                    ]
                )
                for y in range(y_min, y_min + self.rows + 4)
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


def batched(iterable, n):
    """
    Batch data into lists of length n. The last batch may be shorter.
    Recipe from https://docs.python.org/3/library/itertools.html#itertools-recipes
    """
    # batched('ABCDEFG', 3) --> ABC DEF G
    if n < 1:
        raise ValueError("n must be at least one")
    it = iter(iterable)
    while batch := list(islice(it, n)):
        yield batch
