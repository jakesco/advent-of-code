from collections.abc import Iterator
from dataclasses import dataclass
from itertools import batched
from math import sqrt
from typing import Callable, Self

from aoc.utils.functions import apply
from aoc.utils.interfaces import Solution

# TODO: for part 2 randomly pick some seeds to find a plausible
#  range of local minima. Check all the seeds in that min range.


@dataclass
class Conversion:
    lower: int
    upper: int
    diff: int

    @classmethod
    def from_range(cls, line: str) -> Self:
        dest, src, length = map(int, line.split())
        return cls(src, src + length, dest - src)


def feed_input(lines: list[str]) -> Iterator[list[str]]:
    blanks = (idx for idx, line in enumerate(lines) if line == "")
    start = 1
    for blank in blanks:
        yield lines[start:blank]
        start = blank + 2
    yield lines[start:]


def mapper_factory(map_: list[str]) -> Callable[[int], int]:
    conversions = [Conversion.from_range(line) for line in map_]

    def mapper(x: int) -> int:
        for c in conversions:
            if c.lower <= x <= c.upper:
                return x + c.diff
        return x

    return mapper


def check_seed_range(
    start: int, length: int, mappers: list[Callable[[int], int]]
) -> tuple[int, int]:
    """Returns an approximate minimum seed value"""
    step = round(sqrt(length))
    min_check, min_val = start, apply(start, mappers)
    check = start + step
    while check < start + length:
        val = apply(check, mappers)
        if val < min_val:
            min_check, min_val = check, val
        check += step
    return min_check, min_val


def refine_min_seed(start: int, mappers: list[Callable[[int], int]]) -> int:
    min_val = apply(start, mappers)
    left = apply(start - 1, mappers)
    right = apply(start + 1, mappers)
    if left < min_val:
        curr = start - 1
        val = min_val = left
        while val <= min_val:
            curr -= 1
            val = apply(curr, mappers)
        return curr + 1
    if right < min_val:
        curr = start + 1
        val = min_val = right
        while val <= min_val:
            curr += 1
            val = apply(curr, mappers)
        return curr - 1
    else:
        return start


def main(puzzle_input: list[str]) -> Solution:
    mappers = [mapper_factory(line) for line in feed_input(puzzle_input[2:])]

    seeds = list(map(int, puzzle_input[0].split(":")[1].split()))
    part1 = min(apply(seed, mappers) for seed in seeds)

    seed_ranges = [
        (int(a), int(b)) for a, b in batched(puzzle_input[0].split(":")[1].split(), 2)
    ]
    approx_min_seed, min_val = check_seed_range(*seed_ranges[0], mappers)
    for r in seed_ranges[1:]:
        check, val = check_seed_range(*r, mappers)
        if val <= min_val:
            approx_min_seed, min_val = check, val
    min_seed = refine_min_seed(approx_min_seed, mappers)
    part2 = apply(min_seed, mappers)

    return Solution(part1, part2)


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
    # Testing mapper function
    test_mapper = mapper_factory(["50 98 2", "52 50 48"])
    assert test_mapper(98) == 50
    assert test_mapper(99) == 51
    assert test_mapper(53) == 55
    assert test_mapper(10) == 10

    s = main(sample.split("\n"))
    print(s)
    assert s.part1 == 35
    assert s.part2 == 46
    # 79874952 too high
