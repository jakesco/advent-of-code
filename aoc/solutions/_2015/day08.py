from collections.abc import Iterator

from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    total = 0
    printable = 0
    for i in puzzle_input:
        p, t = count_chars(i)
        total += t
        printable += p
    return Solution(total - printable)


def count_chars(line: str) -> tuple[int, int]:
    total = len(line)
    line = iter(line)
    printable = 0
    try:
        while c := next(line):
            if c == '"':
                continue
            if c == "\\":
                printable += parse_escape(line)
                continue
            printable += 1
    except StopIteration:
        pass
    return printable, total


def parse_escape(line_iter: Iterator) -> int:
    match next(line_iter):
        case "x":
            next(line_iter)
            next(line_iter)
        case '"':
            pass
        case _:
            return 0
    return 1
