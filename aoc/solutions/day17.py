from collections import deque
from copy import deepcopy
from dataclasses import dataclass
from itertools import cycle, islice, zip_longest
from typing import Iterable, Iterator, NewType

from .shared import Solution

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

    def col_heights(self) -> tuple[int, ...]:
        count = [0, 0, 0, 0, 0, 0, 0]
        check = {0, 1, 2, 3, 4, 5, 6}
        found = set()
        for row in self:
            if not check:
                return tuple(count)
            for idx in check:
                if row[idx] == 1:
                    found.add(idx)
                else:
                    count[idx] += 1
            check.difference_update(found)


class Cave:
    def __init__(self, pattern: str):
        self.air_jets = self._air_jets(pattern)
        self.rocks = self._falling_rocks()

    def _air_jets(self, pattern: str) -> Iterator[int]:
        """Pushes -1 for left and 1 for right."""
        pattern_len = len(pattern)
        for idx, jet in enumerate(cycle(1 if x == ">" else -1 for x in pattern)):
            self.jet_index = idx % pattern_len
            yield jet

    def _falling_rocks(self) -> Iterator[Rock]:
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
        for idx, rock in enumerate(cycle(rocks)):
            self.rock_index = idx % 5
            yield rock


@dataclass(frozen=True, slots=True, eq=True)
class Iteration:
    jet_index: int = 0
    rock_index: int = 0
    delta_height: int = 0
    col_heights: tuple[int, ...] = (0, 0, 0, 0, 0, 0, 0)


def main(input_: list[str]) -> Solution:
    chamber = Chamber()
    cave = Cave(input_[0])

    iterations = [Iteration()]
    prev_height = 0
    while True:
        simulate_fall(chamber, cave)
        height = len(chamber)
        iter = Iteration(
            cave.jet_index, cave.rock_index, height - prev_height, chamber.col_heights()
        )
        prev_height = height
        # Find Cycle
        if iter in iterations:
            break
        iterations.append(iter)

    idx = iterations.index(iter)
    rocks_so_far = len(iterations)
    delta_rocks = len(iterations) - idx
    delta_height = sum([i.delta_height for i in iterations[idx : len(iterations)]])
    print(rocks_so_far, delta_rocks, delta_height)
    x = FALLEN_ROCKS - rocks_so_far
    z = x / delta_rocks
    part1 = int(len(chamber) + z * delta_height)
    rocks_so_far += x
    print(rocks_so_far)

    part2 = 0
    print(f"Part 2 TEST: {part2 == 1514285714288}")
    return Solution(part1, part2)


def simulate_fall(chamber: Chamber, cave: Cave):
    rock = next(cave.rocks)
    for _ in range(4):
        rock.move(next(cave.air_jets))
    overlap = 0
    while overlap < len(chamber):
        rock.overlap.appendleft(chamber[overlap])
        if rock.move(0):
            break
        rock.move(next(cave.air_jets))
        overlap += 1
    chamber.pop_many(overlap)
    chamber.extendleft(rock.settle())


def find_cycle():
    pass
