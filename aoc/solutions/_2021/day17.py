import argparse
import os
import re
from dataclasses import dataclass


@dataclass(frozen=True)
class Target:
    x: tuple[int, int]
    y: tuple[int, int]


@dataclass
class Probe:
    x: int = 0
    y: int = 0
    dx: int = 0
    dy: int = 0

    def step(self):
        self.x += self.dx
        self.y += self.dy
        if self.dx > 0:
            self.dx -= 1
        elif self.dx < 0:
            self.dx += 1
        self.dy -= 1

    def simulate(self, dx: x, dy: y, target: Target) -> int | None:
        """Simulates shot at trajectory. Returns height if in target area."""
        self.x = self.y = 0
        max_height = self.y
        self.dx = dx
        self.dy = dy
        while self.y > target.y[0]:
            self.step()
            max_height = max(max_height, self.y)
            if self.in_target(target):
                return max_height
        return None

    def in_target(self, target: Target) -> bool:
        return (target.x[0] <= self.x <= target.x[1]) and (
            target.y[0] <= self.y <= target.y[1]
        )


def read_input(filepath: str) -> Target:
    with open(filepath, "r") as f:
        match_ = re.findall(r"-\d+|\d+", f.read())
    return Target((int(match_[0]), int(match_[1])), (int(match_[2]), int(match_[3])))


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 17 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    target = read_input(path)

    probe = Probe()
    max_height = list()
    for x in range(250):
        for y in range(-250, 250):
            new_height = probe.simulate(x, y, target)
            if new_height is not None:
                max_height.append(new_height)

    print(f"Part 1: {max(max_height)}")
    print(f"Part 2: {len(max_height)}")


def main(_):
    raise NotImplementedError
