import argparse
import os
from collections import deque

CHUNK_MAP = {
    "(": ")",
    "<": ">",
    "[": "]",
    "{": "}",
}

POINTS = {")": 3, "]": 57, "}": 1197, ">": 25137}

POINTS2 = {")": 1, "]": 2, "}": 3, ">": 4}


def read_input(filepath: str):
    with open(filepath, "r") as f:
        return [l.strip() for l in f.readlines()]


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 10 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


def validate_chunk(line: str, start: int) -> int:
    if line[start] not in CHUNK_MAP.keys():
        return 0

    stack = deque()
    stack.append(CHUNK_MAP[line[start]])
    for i in range(start + 1, len(line)):
        char = line[i]
        if char in CHUNK_MAP.keys():
            stack.append(CHUNK_MAP[char])
        elif char in CHUNK_MAP.values() and len(stack) > 0:
            match = stack.pop()
            if char != match:
                return POINTS[char]
    return 0


def find_chunks(line: str) -> int:
    for i in range(len(line)):
        if (points := validate_chunk(line, i)) != 0:
            return points
    return 0


def part_1(lines: list[str]) -> int:
    points = 0
    for line in lines:
        points += find_chunks(line)
    return points


def complete_line(line: str) -> str:
    stack = deque()
    for i in range(len(line)):
        char = line[i]
        if char in CHUNK_MAP.keys():
            stack.append(CHUNK_MAP[char])
        elif char in CHUNK_MAP.values() and len(stack) > 0:
            stack.pop()
    stack.reverse()
    return "".join([s for s in stack])


def calculate_points(completion: str) -> int:
    points = 0
    for char in completion:
        points *= 5
        points += POINTS2[char]
    return points


def part_2(lines: list[str]) -> int:
    incomplete = [line for line in lines if find_chunks(line) == 0]

    points = []
    for line in incomplete:
        completion = complete_line(line)
        points.append(calculate_points(completion))

    points.sort()
    return points[len(points) // 2]


if __name__ == "__main__":
    path = init_parser()

    lines = read_input(path)

    print(f"Part 1: {part_1(lines)}")

    print(f"Part 2: {part_2(lines)}")


def main(_):
    raise NotImplementedError
