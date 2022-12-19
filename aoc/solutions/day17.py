from dataclasses import dataclass
from itertools import cycle
from typing import Iterator

from .shared import Grid, P, Solution

FALLEN_ROCKS = 2022


@dataclass
class Rock:
    shape: list[int]
    top_left: P = P(0, 0)


def main(input_: list[str]) -> Solution:
    grid = Grid()
    jets = air_jet(input_[0])
    rocks = falling_rocks()

    rock = next(rocks)

    return Solution()


def air_jet(pattern: str) -> Iterator[int]:
    """Pushes -1 for left and 1 for right."""
    yield from cycle(1 if x == ">" else -1 for x in pattern)


def falling_rocks() -> Iterator[Rock]:
    # fmt: off
    rocks = (
        Rock([
            1, 1, 1, 1,
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
        ]),
        Rock([
            0, 1, 0, 0,
            1, 1, 1, 0,
            0, 1, 0, 0,
            0, 0, 0, 0,
        ]),
        Rock([
            0, 0, 1, 0,
            0, 0, 1, 0,
            1, 1, 1, 0,
            0, 0, 0, 0,
        ]),
        Rock([
            1, 0, 0, 0,
            1, 0, 0, 0,
            1, 0, 0, 0,
            1, 0, 0, 0,
        ]),
        Rock([
            1, 1, 0, 0,
            1, 1, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
        ]),
    )
    # fmt: on
    yield from cycle(rocks)
