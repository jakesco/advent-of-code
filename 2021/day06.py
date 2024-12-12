import argparse
import os
from collections import deque
from dataclasses import dataclass


@dataclass
class Population:
    adults: deque[int]
    children: deque[int]

    def simulate_day(self):
        self.adults.rotate(-1)
        mature = self.children[0]
        self.children.rotate(-1)
        self.children[-1] = self.adults[-1]
        self.adults[-1] += mature

    @staticmethod
    def from_fish_list(adults: list[int]):
        seed = [0] * 7
        for a in adults:
            seed[a] += 1
        return Population(deque(seed, maxlen=7), deque([0, 0], maxlen=2))

    @property
    def size(self) -> int:
        return sum(self.adults) + sum(self.children)

    def to_list(self):
        return list(self.adults) + list(self.children)

    def __repr__(self):
        return f"Population(size={self.size})"


def read_input(filepath: str) -> list[int]:
    with open(filepath, "r") as f:
        return [int(n) for n in f.readline().split(",")]


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 6 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    days = 256

    fish = read_input(path)
    pop = Population.from_fish_list(fish)
    for d in range(days):
        # print(f"Adults: {pop.adults}, Children: {pop.children}, Size: {pop.size}")
        pop.simulate_day()

    print(f"\nFish count after {days} day(s): {pop.size}")


def main(_):
    raise NotImplementedError
