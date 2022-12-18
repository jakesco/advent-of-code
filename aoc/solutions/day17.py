from dataclasses import dataclass

from .shared import Solution


@dataclass
class Rock:
    shape: list[int]


# fmt: off
rocks = [
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
]
# fmt: on


def main(input_: list[str]) -> Solution:
    for rock in rocks:
        print(rock)
    print(input_)
    return Solution()
