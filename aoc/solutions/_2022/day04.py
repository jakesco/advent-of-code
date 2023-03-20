import re
from dataclasses import dataclass

from .shared import Solution


@dataclass
class Section:
    min: int
    max: int


def main(input_: list[str]) -> Solution:
    sections = [parse_section(s) for s in input_]
    full = len([s for s in sections if contained(s)])
    partial = len([s for s in sections if overlap(s)])
    return Solution(full, partial)


def parse_section(s: str) -> tuple[Section, Section]:
    bounds = [int(x) for x in re.split(r",|-", s)]
    return Section(*bounds[:2]), Section(*bounds[2:])


def contained(sections: tuple[Section, Section]) -> bool:
    left, right = sections
    return (
        left.min <= right.min <= right.max <= left.max
        or right.min <= left.min <= left.max <= right.max
    )


def overlap(sections: tuple[Section, Section]) -> bool:
    left, right = sections
    return left.min <= right.min <= left.max or right.min <= left.min <= right.max
