from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from functools import reduce
from itertools import batched
from typing import Callable, Self

from aoc.utils.interfaces import Solution


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


def feed_seeds(seeds: Iterable[int]) -> Iterator[int]:
    for start, length in batched(seeds, 2):
        yield from range(start, start + length)


def main(puzzle_input: list[str]) -> Solution:
    mappers = [mapper_factory(line) for line in feed_input(puzzle_input[2:])]
    seeds = list(map(int, puzzle_input[0].split(":")[1].split()))

    part1 = min(
        [reduce(lambda x, y: y(x), mappers[1:], mappers[0](seed)) for seed in seeds]
    )
    part2 = min(
        [
            reduce(lambda x, y: y(x), mappers[1:], mappers[0](seed))
            for seed in feed_seeds(seeds)
        ]
    )
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
