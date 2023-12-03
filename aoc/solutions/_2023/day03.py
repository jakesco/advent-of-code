from dataclasses import dataclass
from typing import Self

from aoc.utils.interfaces import Solution


@dataclass
class Number:
    id: int
    row: int
    left: int
    right: int

    @classmethod
    def new(cls, digits: str, row, col) -> Self:
        return cls(
            int(digits),
            row=row,
            left=col - len(digits),
            right=col - 1,
        )


def main(puzzle_input: list[str]) -> Solution:
    numbers = []
    symbols = set()
    digit_accumulator = ""
    for row, line in enumerate(puzzle_input):
        if digit_accumulator:
            numbers.append(Number.new(digit_accumulator, row, len(puzzle_input[0])))
            digit_accumulator = ""
        for col, char in enumerate(line):
            if char.isdigit():
                digit_accumulator += char
                continue
            if digit_accumulator:
                numbers.append(Number.new(digit_accumulator, row, col))
                digit_accumulator = ""
            if not char == ".":
                symbols.add((row, col))

    parts = find_parts(numbers, symbols)
    part1 = sum([part.id for part in parts])
    return Solution(part1)


def find_parts(numbers: list[Number], symbols: set[tuple[int, int]]) -> list[Number]:
    out = []
    for n in numbers:
        print(f"Checking {n}")
        if is_part(n, symbols):
            out.append(n)
    return out


def is_part(number: Number, symbols: set[tuple[int, int]]) -> bool:
    for x in range(number.row - 1, number.row + 2):
        for y in range(number.left - 1, number.right + 2):
            print(f"Checking: ({x}, {y})")
            if (x, y) in symbols:
                print(f"Found!")
                return True
    return False


if __name__ == "__main__":
    sample = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""
    # part1 4361
    print(main(sample.split("\n")))
    # 553825
