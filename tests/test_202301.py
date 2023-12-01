from aoc.solutions._2023.day01 import part1, part2


def test_part1():
    x = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
    assert part1(x.split()) == 142


def test_part2():
    x = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""
    assert part2(x.split()) == 281
