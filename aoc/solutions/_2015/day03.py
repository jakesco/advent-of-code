from dataclasses import dataclass

from aoc.utils.interfaces import Solution


@dataclass(frozen=True)
class Point:
    x: int
    y: int


def main(puzzle_input: list[str]) -> Solution:
    part1 = single_santa(puzzle_input[0])
    part2 = double_santa(puzzle_input[0])
    return Solution(part1, part2)


def single_santa(directions: str) -> int:
    santa: set[Point] = set()
    current: Point = Point(0, 0)

    for direction in directions:
        current = move_direction(direction, current)
        santa.add(current)

    return len(santa)


def double_santa(directions: str) -> int:
    santa: set[Point] = set()
    robo_santa: set[Point] = set()
    current_santa: Point = Point(0, 0)
    current_robo_santa: Point = Point(0, 0)

    for i, direction in enumerate(directions):
        if i % 2 == 0:
            current_santa = move_direction(direction, current_santa)
            santa.add(current_santa)
        else:
            current_robo_santa = move_direction(direction, current_robo_santa)
            robo_santa.add(current_robo_santa)

    return len(santa.union(robo_santa))


def move_direction(direction: str, start_point: Point) -> Point:
    if direction == ">":
        return Point(start_point.x + 1, start_point.y)
    if direction == "<":
        return Point(start_point.x - 1, start_point.y)
    if direction == "^":
        return Point(start_point.x, start_point.y + 1)
    if direction == "v":
        return Point(start_point.x, start_point.y - 1)
