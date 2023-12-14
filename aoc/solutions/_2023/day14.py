from dataclasses import dataclass

from aoc.utils.interfaces import Solution

# TODO: make the cycle code and look for repeating patterns


@dataclass(frozen=True, eq=True, order=True)
class Point:
    x: int
    y: int


def main(puzzle_input: list[str]) -> Solution:
    grid = parse_input(puzzle_input)
    return Solution(
        settle(grid, len(puzzle_input)),
    )


def settle(grid: dict[Point, str], top: int) -> int:
    rocks = [p for p, v in grid.items() if v == "O"]
    rocks.sort()

    for rock in rocks:
        move = 0
        for j in reversed(range(rock.y)):
            if grid[Point(rock.x, j)] != ".":
                break
            move += 1
        grid[rock] = "."
        grid[Point(rock.x, rock.y - move)] = "O"

    # for y in range(10):
    #     print("".join(grid[Point(x, y)] for x in range(10)))

    return sum(top - p.y for p, v in grid.items() if v == "O")


def parse_input(puzzle_input: list[str]) -> dict[Point, str]:
    grid = {}
    for y in range(len(puzzle_input)):
        for x in range(len(puzzle_input[0])):
            grid[Point(x, y)] = puzzle_input[y][x]
    return grid


if __name__ == "__main__":
    sample = """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""
    test = main(sample.splitlines())
    print(test)
    assert test.part1 == 136
    assert test.part2 == 0
