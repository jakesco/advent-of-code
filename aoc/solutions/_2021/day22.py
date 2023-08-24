import argparse
import os
from dataclasses import dataclass
from functools import cache, reduce
from itertools import combinations, product
from pprint import pprint


class Grid:
    """Straight forward Grid model used for part 1"""

    def __init__(self):
        self.__on_nodes = set()

    @property
    def cubes_on(self) -> int:
        return len(self.__on_nodes)

    def switch(self, x_range: range, y_range: range, z_range: range, on: bool = True):
        if any(
            [
                x_range.start < -50,
                x_range.stop > 51,
                y_range.start < -50,
                y_range.stop > 51,
                z_range.start < -50,
                z_range.stop > 51,
            ]
        ):
            return
        if on:
            for node in product(x_range, y_range, z_range, repeat=1):
                self.__on_nodes.add(node)
        else:
            for node in product(x_range, y_range, z_range, repeat=1):
                self.__on_nodes.discard(node)


def part1(input_) -> int:
    grid = Grid()
    for instruction, x, y, z in input_:
        grid.switch(x, y, z, instruction == "on")
        # print(f"Instruction Executed: {instruction} {x} {y} {z}")
    return grid.cubes_on


# Start of part 2 code
@dataclass(frozen=True)
class Cuboid:
    on: bool
    x: range
    y: range
    z: range

    def __repr__(self):
        return (
            f"Cuboid({'on' if self.on else 'off'}: x=[{self.x.start}..{self.x.stop - 1}] "
            f"y=[{self.y.stop}..{self.y.stop - 1}] z=[{self.z.start}..{self.z.stop - 1}])"
        )

    @property
    def magnitude(self) -> int:
        return len(self.x) * len(self.y) * len(self.z)


def cuboid_intersect(c1: Cuboid, c2: Cuboid) -> Cuboid | None:
    def intersect_1d(a, b):
        start = max(a.start, b.start)
        stop = min(a.stop, b.stop)
        if start < stop:
            return range(start, stop)
        return None

    x = intersect_1d(c1.x, c2.x)
    y = intersect_1d(c1.y, c2.y)
    z = intersect_1d(c1.z, c2.z)
    if any([a is None for a in (x, y, z)]):
        return None
    return Cuboid(c1.on, x, y, z)


def intersects(c1: Cuboid, c2: Cuboid) -> bool:
    return cuboid_intersect(c1, c2) is not None


@cache
def intersect_all(cuboids: set[Cuboid]) -> Cuboid | None:
    def reduce_cuboids(a, b):
        if a is None or b is None:
            return None
        return cuboid_intersect(a, b)

    return reduce(reduce_cuboids, cuboids)


def part2(cuboids: set[Cuboid]) -> int:
    """
    Inclusion-exclusion principle:

    |Union of sets|=∑|Singletons|−∑|Pairs|+∑|Triples|−∑|Quadruples|+...+(−1)n+1|n-tuples|
    """
    return 0


# Inclusion-exclusion principle for finding common points in sets
# or sweep line algo

# Input reading
def make_range(input_: str) -> range:
    x = input_.split("=")[1]
    a, b = x.split("..")
    a = int(a)
    b = int(b)
    start = min(a, b)
    end = max(a, b)
    return range(int(start), int(end) + 1)


def read_input(filepath: str) -> list[str]:
    with open(filepath, "r") as f:
        for line in f.readlines():
            on_off, ranges = line.strip().split()
            dims = ranges.split(",")
            x = make_range(dims[0])
            y = make_range(dims[1])
            z = make_range(dims[2])
            yield on_off, x, y, z


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 22 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    print(f"Part 1: {part1(read_input(path))}")
    print(
        f"Part 2: {part2({Cuboid(i == 'on', x, y, z) for i, x, y, z in read_input(path)})}"
    )
    print(f"Part 2: 2758514936282235 (test)")


def main(_):
    raise NotImplementedError
