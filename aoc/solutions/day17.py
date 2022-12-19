from collections import deque
from dataclasses import dataclass, field
from itertools import cycle, islice
from typing import Iterable, Iterator, NewType

from .shared import Solution

FALLEN_ROCKS = 2022

Row = NewType("Row", tuple[int, ...])


def row_to_str(row: Iterable) -> str:
    return "".join(["#" if r else "." for r in row])


class Rock:
    def __init__(self, shape: list[deque[int]]):
        self.shape = shape
        self.overlap = deque(maxlen=len(shape))

    def __str__(self):
        output = []
        for row in self.shape:
            output.append(f"|{row_to_str(row)}|")
        return "\n".join(output)

    def _collide_left(self, stack_rows: list[Row] = None) -> bool:
        if any([r[0] for r in self.shape]):
            return True
        return False

    def _collide_right(self, stack_rows: list[Row] = None) -> bool:
        if any([r[-1] for r in self.shape]):
            return True
        return False

    def _collide_down(self, stack_rows: list[Row] = None) -> bool:
        # TODO
        return False

    def move(self, direction: int, stack_rows: list[Row] = None) -> bool:
        if direction < 0 and not self._collide_left(stack_rows):
            for row in self.shape:
                row.rotate(-1)
            return False
        if direction > 0 and not self._collide_right(stack_rows):
            for row in self.shape:
                row.rotate(1)
            return False
        return self._collide_down(stack_rows)

    def add_overlap(self, row: Row):
        self.overlap.append(row)

    def settle(self) -> list[Row]:
        return [Row(tuple(islice(r, 7))) for r in reversed(self.shape)]


class Chamber(deque):
    def __init__(self, *args, **kwargs):
        super(Chamber, self).__init__(*args, **kwargs)

    def __str__(self):
        output = []
        output.append("|.......|")
        for row in self:
            output.append(f"|{row_to_str(row)}|")
        output.append("+-------+")
        return "\n".join(output)

    def pop_many(self, n: int = 1) -> Iterator[Row]:
        for _ in range(n):
            try:
                yield self.popleft()
            except IndexError:
                break


def main(input_: list[str]) -> Solution:
    chamber = Chamber()
    jets = air_jet(input_[0])
    rocks = falling_rocks()
    for _ in range(5):
        simulate_fall(chamber, rocks, jets)
    print(chamber)

    part1 = part2 = 0
    part1 = len(chamber)
    return Solution(part1, part2)


def simulate_fall(chamber: Chamber, rocks: Iterator[Rock], jets: Iterator[int]):
    rock = next(rocks)
    for _ in range(4):
        rock.move(next(jets))
    chamber.extendleft(rock.settle())


def air_jet(pattern: str) -> Iterator[int]:
    """Pushes -1 for left and 1 for right."""
    yield from cycle(1 if x == ">" else -1 for x in pattern)


def falling_rocks() -> Iterator[Rock]:
    # fmt: off
    rocks = (
        Rock([deque([0, 0, 1, 1, 1, 1, 0])]),
        Rock([
            deque([0, 0, 0, 1, 0, 0, 0]),
            deque([0, 0, 1, 1, 1, 0, 0]),
            deque([0, 0, 0, 1, 0, 0, 0]),
        ]),
        Rock([
            deque([0, 0, 0, 0, 1, 0, 0]),
            deque([0, 0, 0, 0, 1, 0, 0]),
            deque([0, 0, 1, 1, 1, 0, 0]),
        ]),
        Rock([
            deque([0, 0, 1, 0, 0, 0, 0]),
            deque([0, 0, 1, 0, 0, 0, 0]),
            deque([0, 0, 1, 0, 0, 0, 0]),
            deque([0, 0, 1, 0, 0, 0, 0]),
        ]),
        Rock([
            deque([0, 0, 1, 1, 0, 0, 0]),
            deque([0, 0, 1, 1, 0, 0, 0]),
        ]),
    )
    # fmt: on
    yield from cycle(rocks)
