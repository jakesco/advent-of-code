from collections.abc import Iterator
from functools import reduce
from typing import Callable

from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    seeds = map(int, puzzle_input[0].split(":")[1].split())
    mappers = []
    for line in feed_input(puzzle_input[2:]):
        mappers.append(mapper_factory(line[1:]))

    mapped_seeds = [
        reduce(lambda x, y: y(x), mappers[1:], mappers[0](seed)) for seed in seeds
    ]

    return Solution(min(mapped_seeds))


def mapper_factory(map_: list[str]) -> Callable[[int], int]:
    conversion = {}
    for range_ in map_:
        dest, src, length = map(int, range_.split())
        for i in range(length):
            conversion[src + i] = dest + i
    return lambda x: conversion.get(x, x)


def feed_input(lines: list[str]) -> Iterator[list[str]]:
    blanks = (idx for idx, line in enumerate(lines) if line == "")
    start = 0
    for blank in blanks:
        yield lines[start:blank]
        start = blank + 1
    yield lines[start:]


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
    # assert s.part2 == 30
