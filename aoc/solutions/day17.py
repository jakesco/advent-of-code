from collections import deque
from copy import deepcopy
from dataclasses import dataclass
from itertools import cycle, islice, zip_longest
from typing import Iterable, Iterator, NewType

from .shared import Solution

# Test ans: 1514285714288
FALLEN_ROCKS = 2022
IMPRESS_THE_ELEPHANTS = 1_000_000_000_000
EMPTY_ROW = (0, 0, 0, 0, 0, 0, 0)

Row = NewType("Row", tuple[int, ...])


def row_to_str(row: Iterable) -> str:
    return "".join(["#" if r else "." for r in row])


def merge_rows(r1: Row, r2: Row) -> Row:
    return Row(tuple(a | b for a, b in zip(r1, r2)))


class Rock:
    def __init__(self, shape: list[deque[int]]):
        self._initial_shape = shape
        self.shape = deepcopy(shape)
        self.overlap = deque()

    def __str__(self):
        output = []
        for row in self.shape:
            output.append(f"|{row_to_str(row)}|")
        return "\n".join(output)

    def _reset_shape(self):
        self.shape = deepcopy(self._initial_shape)

    def _get_overlaps(self) -> Iterator[tuple[Row, Row]]:
        yield from zip_longest(reversed(self.shape), self.overlap, fillvalue=EMPTY_ROW)

    def _collide_left(self) -> bool:
        if any([r[0] for r in self.shape]):
            return True
        for a, b in self._get_overlaps():
            for i in range(1, 7):
                if a[i] == 1 and b[i - 1] == 1:
                    return True
        return False

    def _collide_right(self) -> bool:
        if any([r[-1] for r in self.shape]):
            return True
        for a, b in self._get_overlaps():
            for i in range(6):
                if a[i] == 1 and b[i + 1] == 1:
                    return True
        return False

    def _collide_down(self) -> bool:
        for a, b in self._get_overlaps():
            filled = sum(a) + sum(b)
            if sum(merge_rows(a, b)) < filled:
                self.overlap.popleft()
                return True
        return False

    def move(self, direction: int) -> bool:
        if direction < 0 and not self._collide_left():
            for row in self.shape:
                row.rotate(-1)
            # print(f"Moved Left\n{self}", end="\n\n")
            return False
        if direction > 0 and not self._collide_right():
            for row in self.shape:
                row.rotate(1)
            # print(f"Moved Right\n{self}", end="\n\n")
            return False
        return self._collide_down()

    def add_overlap(self, row: Row):
        self.overlap.append(row)

    def settle(self) -> list[Row]:
        rows = list()
        for a, b in self._get_overlaps():
            merged = merge_rows(a, b)
            rows.append(merged)
        self.overlap.clear()
        self._reset_shape()
        return rows


class Chamber(deque):
    def __init__(self, *args, **kwargs):
        super(Chamber, self).__init__(*args, **kwargs)

    def __str__(self):
        output = list()
        output.append("|.......|")
        for row in self:
            output.append(f"|{row_to_str(row)}|")
        output.append("+-------+")
        return "\n".join(output)

    def pop_many(self, n: int = 1) -> list[Row]:
        popped = []
        for _ in range(n):
            try:
                popped.append(self.popleft())
            except IndexError:
                break
        return popped


@dataclass
class Cycle:
    height: int
    rocks: int


def main(input_: list[str]) -> Solution:
    chamber = Chamber()
    jets = air_jet(input_[0])
    rocks = falling_rocks()

    rocks_fallen = 0
    for _ in range(FALLEN_ROCKS):
        simulate_fall(chamber, rocks, jets)
        rocks_fallen += 1
    part1 = len(chamber)

    # Try to find a cycle
    cycle_detected = False
    while not cycle_detected:
        cycle_detected = find_cycle(chamber, rocks, jets)
        rocks_fallen += 1
        print(rocks_fallen)
    rock_cycle = Cycle(len(chamber) // 2, rocks_fallen)

    part2 = len(chamber)
    while rocks_fallen < IMPRESS_THE_ELEPHANTS:
        part2 += rock_cycle.height
        rocks_fallen += rock_cycle.rocks

    return Solution(part1, part2)


def simulate_fall(chamber: Chamber, rocks: Iterator[Rock], jets: Iterator[int]):
    rock = next(rocks)
    for _ in range(4):
        rock.move(next(jets))
    overlap = 0
    while overlap < len(chamber):
        rock.overlap.appendleft(chamber[overlap])
        if rock.move(0):
            break
        rock.move(next(jets))
        overlap += 1
    chamber.pop_many(overlap)
    chamber.extendleft(rock.settle())


def find_cycle(chamber: Chamber, rocks: Iterator[Rock], jets: Iterator[int]) -> bool:
    simulate_fall(chamber, rocks, jets)
    first_half = islice(chamber, len(chamber) // 2)
    second_half = islice(chamber, len(chamber) // 2, len(chamber))
    return all([a == b for a, b in zip_longest(first_half, second_half)])


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
