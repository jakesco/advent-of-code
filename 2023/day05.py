from collections.abc import Iterator
from dataclasses import dataclass
from itertools import batched
from math import sqrt
from typing import Callable, Self

from aoc.utils.functions import apply
from aoc.utils.interfaces import Solution


@dataclass
class Conversion:
    dest: int
    src: int
    length: int

    @classmethod
    def from_range(cls, line: str) -> Self:
        return cls(*map(int, line.split()))


@dataclass
class Mapper:
    ranges: list[Conversion]

    @classmethod
    def from_str(cls, lines: list[str]) -> Self:
        return cls([Conversion.from_range(line) for line in lines])

    def forward(self, x: int) -> int:
        for r in self.ranges:
            if r.src <= x <= r.src + r.length:
                return x + r.dest - r.src
        return x

    def backward(self, x: int) -> int:
        for r in self.ranges:
            if r.dest <= x <= r.dest + r.length:
                return x + r.src - r.dest
        return x


def feed_input(lines: list[str]) -> Iterator[list[str]]:
    blanks = (idx for idx, line in enumerate(lines) if line == "")
    start = 1
    for blank in blanks:
        yield lines[start:blank]
        start = blank + 2
    yield lines[start:]


def in_seed_range(seed: int, ranges: list[tuple[int, int]]) -> bool:
    for r in ranges:
        if r[0] <= seed <= r[1]:
            return True
    return False


def find_min_seed(
    seed_ranges: list[tuple[int, int]], mappers: list[Callable[[int], int]]
) -> int:
    """Starts at location 0 and applies the mappers backwards until a
    seed is in range. This makes some jumps first to find an approximate seed
    then refines the seed after."""
    end = max(b for _, b in seed_ranges)
    step = round(sqrt(end)) or 1
    approx = 0
    for check in range(0, end, step):
        seed = apply(check, mappers)
        if in_seed_range(seed, seed_ranges):
            approx = check
            break

    for check in range(approx - step, approx):
        seed = apply(check, mappers)
        if in_seed_range(seed, seed_ranges):
            return seed


def main(puzzle_input: list[str]) -> Solution:
    mappers = [Mapper.from_str(line) for line in feed_input(puzzle_input[2:])]
    forward = [mapper.forward for mapper in mappers]
    backward = [mapper.backward for mapper in mappers]
    backward.reverse()

    seeds = list(map(int, puzzle_input[0].split(":")[1].split()))
    part1 = min(apply(seed, forward) for seed in seeds)

    seed_ranges = [
        (start, start + length)
        for start, length in batched(map(int, puzzle_input[0].split(":")[1].split()), 2)
    ]
    min_seed = find_min_seed(seed_ranges, backward)
    print(apply(min_seed - 1, forward))
    print(apply(min_seed, forward))
    print(apply(min_seed + 1, forward))

    return Solution(part1, apply(min_seed, forward))


if __name__ == "__main__":
    sample = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""
    s = main(sample.split("\n"))
    print(s)
    assert s.part1 == 35
    assert s.part2 == 46
    # 79874952 too high ?
