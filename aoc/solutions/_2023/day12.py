from contextlib import suppress
from dataclasses import dataclass
from itertools import product
from typing import Self

from aoc.utils.interfaces import Solution

# TODO: Dynamic programming for part 2


@dataclass
class Record:
    springs: list[str]
    records: list[int]

    @classmethod
    def from_string(cls, line: str) -> Self:
        springs, records = line.split()
        return cls(
            [s for s in springs],
            [int(a) for a in records.split(",")],
        )


def main(puzzle_input: list[str]) -> Solution:
    records = [Record.from_string(line) for line in puzzle_input]
    return Solution(sum(find_arrangements(record) for record in records))


def find_arrangements(record: Record) -> int:
    unknowns = [idx for idx, c in enumerate(record.springs) if c == "?"]
    count = 0
    for candidate in product(("#", "."), repeat=len(unknowns)):
        test_springs = record.springs.copy()
        for idx, c in zip(unknowns, candidate):
            test_springs[idx] = c
        if valid_arrangement(test_springs, record.records):
            count += 1
    return count


def valid_arrangement(springs: list[str], records: list[int]) -> bool:
    """Checks if spring/record pair is valid."""
    if "?" in springs:
        return False

    record = iter(records)
    c = next(record)
    count = 0
    with suppress(StopIteration):
        for idx, s in enumerate(springs):
            if s == "#":
                count += 1
            elif count == 0:
                continue
            elif count != c:
                return False
            else:
                count = 0
                c = next(record)
        return count == c and not next(record, False)
    return "#" not in springs[idx:]


if __name__ == "__main__":
    sample = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""
    x = main(sample.splitlines())
    print(x)
    assert x.part1 == 21
    # assert x.part2 == 525152
