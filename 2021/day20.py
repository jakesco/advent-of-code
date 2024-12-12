import argparse
import os
from dataclasses import dataclass
from itertools import product


@dataclass
class Grid:
    w: int
    h: int
    __grid: list[str]
    background: str = "0"

    def __repr__(self):
        string = ["." if x == "0" else "#" for x in self.__grid]
        for i in range(self.w, self.w * self.h, self.w + 1):
            string.insert(i, "\n")
        return "".join(string)

    @property
    def lit(self):
        return len([x for x in self.__grid if x == "1"])

    def flip_background(self):
        self.background = "0" if self.background == "1" else "1"

    def get(self, row: int, col: int) -> str:
        idx = row * self.w + col
        if not (0 <= row < self.h and 0 <= col < self.w):
            return self.background
        return self.__grid[idx]

    def convolve(self, row: int, col: int, algorithm: list[str]) -> str:
        idx = list()
        for i, j in product([-1, 0, 1], repeat=2):
            idx.append(self.get(row + i, col + j))
        idx = int("".join(idx), 2)
        return algorithm[idx]


def enhance(grid: Grid, algorithm: list[str]) -> Grid:
    enhanced_map = list()
    for i in range(-1, grid.h + 1):
        for j in range(-1, grid.w + 1):
            enhanced_map.append(grid.convolve(i, j, algorithm))
    return Grid(grid.w + 2, grid.h + 2, enhanced_map, background=grid.background)


def read_input(filepath: str) -> (list[int], Grid):
    map_ = list()
    rows = 0
    cols = 0
    with open(filepath, "r") as f:
        algo = ["0" if x == "." else "1" for x in f.readline().strip()]
        f.readline()
        for line in f.readlines():
            if cols == 0:
                cols = len(line.strip())
            map_ = map_ + ["0" if x == "." else "1" for x in line.strip()]
            rows += 1
    return algo, Grid(cols, rows, map_)


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 20 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    algo, grid = read_input(path)
    flip = algo[0] == "1"

    steps = 2
    for i in range(steps):
        grid = enhance(grid, algo)
        if flip:
            grid.flip_background()
    print(f"Part 1: {grid.lit}")

    steps = 48
    for i in range(steps):
        grid = enhance(grid, algo)
        if flip:
            grid.flip_background()
    print(f"Part 2: {grid.lit}")


def main(_):
    raise NotImplementedError
