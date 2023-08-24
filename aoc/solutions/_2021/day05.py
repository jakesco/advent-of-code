import argparse
import math
import os
from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int


@dataclass
class Line:
    p0: Point
    p1: Point


class Map:
    def __init__(self, size: int):
        self.size = size
        self.map_ = [0] * size * size

    def draw_line(self, line: Line):
        # DDA line drawing algorithm
        dx = line.p1.x - line.p0.x
        dy = line.p1.y - line.p0.y

        side_length = max(abs(dx), abs(dy))

        x_inc = dx / side_length
        y_inc = dy / side_length

        current_x = line.p0.x
        current_y = line.p0.y

        for i in range(side_length + 1):
            self.draw_point(round(current_x), round(current_y))
            current_x += x_inc
            current_y += y_inc

    def draw_line_part1(self, line: Line):
        if line.p0.x == line.p1.x:
            x = line.p0.x
            start = min(line.p0.y, line.p1.y)
            end = max(line.p0.y, line.p1.y) + 1
            for y in range(start, end):
                self.draw_point(x, y)
        elif line.p0.y == line.p1.y:
            y = line.p0.y
            start = min(line.p0.x, line.p1.x)
            end = max(line.p0.x, line.p1.x) + 1
            for x in range(start, end):
                self.draw_point(x, y)

    def draw_point(self, x: int, y: int):
        self.map_[y * self.size + x] += 1

    def overlaps(self):
        return len([x for x in self.map_ if x >= 2])

    def __str__(self):
        strings = [str(x) for x in self.map_]
        output = []
        for i in range(0, self.size * self.size, self.size):
            output.append(" ".join(strings[i : i + self.size]))
        return "\n".join(output)


def read_input(filepath: str) -> (list[Line], int):
    with open(filepath, "r") as f:
        max_ = 0
        lines = []
        for line in f.readlines():
            data = line.split()
            x0 = int(data[0].split(",")[0])
            y0 = int(data[0].split(",")[1])
            x1 = int(data[-1].split(",")[0])
            y1 = int(data[-1].split(",")[1])

            if max_ < max(x0, y0, x1, y1):
                max_ = max(x0, y0, x1, y1)

            a = Point(x0, y0)
            b = Point(x1, y1)
            lines.append(Line(a, b))

    return lines, max_ + (10 - max_ % 10)


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 5 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()

    lines, max_ = read_input(path)

    map_ = Map(max_)

    for line in lines:
        # map_.draw_line_part1(line)
        map_.draw_line(line)

    # print(map_, end='\n\n')
    print(f"Overlaps: {map_.overlaps()}")


def main(_):
    raise NotImplementedError
