from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    position = floor = 0
    for i, c in enumerate(puzzle_input[0], start=1):
        if c == "(":
            floor += 1
        elif c == ")":
            floor -= 1
        if position == 0 and floor < 0:
            position = i
    return Solution(part1=floor, part2=position)
