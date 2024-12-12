import argparse
import os
from dataclasses import dataclass


@dataclass(frozen=True)
class Fold:
    axis: str
    line: int

    def __repr__(self):
        return f"Fold({self.axis}={self.line})"


@dataclass(frozen=True)
class Point:
    x: int
    y: int

    def __repr__(self):
        return f"({self.x}, {self.y})"


def split_set(set_: set[Point], along: Fold) -> (set[Point], set[Point]):
    cutoff = Point(-1, along.line) if along.axis == "y" else Point(along.line, -1)
    split = {p for p in set_ if p.x > cutoff.x and p.y > cutoff.y}
    set_ -= split
    return set_, split


def reflect(point: Point, fold_: Fold) -> Point:
    x = point.x
    y = point.y

    if fold_.axis == "x":
        x = -x
        x += 2 * fold_.line
    else:
        y = -y
        y += 2 * fold_.line

    return Point(x, y)


def fold(points: set[Point], fold_: Fold) -> set[Point]:
    bottom, top = split_set(points, fold_)
    reflections = {reflect(p, fold_) for p in top}
    return bottom.union(reflections)


def render(points: set[Point]):
    width = max(p.x for p in points) + 1
    height = max(p.y for p in points) + 1
    map_ = [False] * width * height
    for p in points:
        map_[p.y * width + p.x] = True

    str_map = ["#" if p else " " for p in map_]
    for i in range(width, len(str_map), width + 1):
        str_map.insert(i, "\n")
    print("".join(str_map))


def read_input(filepath: str) -> (set[Point], list[Fold]):
    points = set()
    folds = list()
    with open(filepath, "r") as f:
        for line in f.readlines():
            if line.startswith("fold along"):
                data = line.rstrip().split("=")
                axis = data[0][-1]
                val = int(data[1])
                folds.append(Fold(axis, val))
            else:
                data = line.rstrip().split(",")
                try:
                    x = int(data[0])
                    y = int(data[1])
                    points.add(Point(x, y))
                except ValueError:
                    pass
    return points, folds


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 13 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    points, folds = read_input(path)

    folded = fold(points, folds[0])
    print(f"Part 1 visible dots: {len(folded)}")

    for f in folds[1:]:
        folded = fold(folded, f)
    print("\nPart 2: ")
    render(folded)


def main(_):
    raise NotImplementedError
