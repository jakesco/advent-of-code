from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    return Solution(part1(puzzle_input), part2(puzzle_input))


def part1(lines: list[str]) -> int:
    sum = 0
    for l in lines:
        y = [c for c in l if c.isdigit()]
        y = int("".join([y[0], y[-1]]))
        print(y)
        sum += y
    return sum


def part2(lines: list[str]) -> int:
    y = {
        "1": "1",
        "2": "2",
        "3": "3",
        "4": "4",
        "5": "5",
        "6": "6",
        "7": "7",
        "8": "8",
        "9": "9",
        "one": "1",
        "two": "2",
        "three": "3",
        "four": "4",
        "five": "5",
        "six": "6",
        "seven": "7",
        "eight": "8",
        "nine": "9",
    }
    sum = 0
    for l in lines:
        d = {}
        for n in y:
            try:
                d[n] = l.index(n)
            except ValueError:
                pass
        min_ = y[min(d, key=d.get)]
        max_ = y[max(d, key=d.get)]
        x = int("".join([min_, max_]))
        sum += x
    return sum


if __name__ == "__main__":
    x = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""
    y = part2(x.split())
    print(y)
