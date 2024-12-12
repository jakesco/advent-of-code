from collections import defaultdict

from aoc.utils.interfaces import Solution


def main(input_: list[str]) -> Solution:
    elf = 0
    calories = defaultdict(int)

    for line in input_:
        if line == "":
            elf += 1
        else:
            calories[elf] += int(line)

    calories = sorted(calories.values(), reverse=True)
    return Solution(calories[0], sum(calories[:3]))
