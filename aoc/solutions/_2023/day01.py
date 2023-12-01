from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    return Solution(part1(puzzle_input))


def part1(lines: list[str]) -> int:
    sum = 0
    for l in lines:
        y = [c for c in l if c.isdigit()]
        y = int("".join([y[0], y[-1]]))
        print(y)
        sum += y
    return sum

def part2(lines: list[str]) -> int:
    return 0

if __name__ == "__main__":
    x = [
        "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet",
    ]
    y = part1(x)
    print(y)
