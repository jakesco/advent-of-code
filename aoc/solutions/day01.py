from collections import defaultdict
from pathlib import Path

from .shared import Solution


def main(filename: Path) -> Solution:
    elf = 0
    calories = defaultdict(int)

    with filename.open() as f:
        for line in f.read().splitlines():
            if line == "":
                elf += 1
            else:
                calories[elf] += int(line)

    calories = sorted(calories.values(), reverse=True)
    return Solution(calories[0], sum(calories[:3]))
