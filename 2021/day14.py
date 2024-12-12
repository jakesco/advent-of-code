import argparse
import os
from collections import Counter
from functools import cache


def insert_char(pair: str, ins: str) -> (str, str):
    return pair[0] + ins, ins + pair[1]


@cache
def simulate(pair: str, steps: int) -> Counter:
    if steps <= 0:
        return Counter(pair)
    left, right = insert_char(pair, rules[pair])
    return simulate(left, steps - 1) + simulate(right, steps - 1) - Counter(rules[pair])


def score(counter: Counter) -> int:
    common = counter.most_common()
    most = common[0][1]
    least = common[-1][1]
    return most - least


def get_pairs(template: str) -> list[str]:
    return [template[i : i + 2] for i in range(len(template) - 1)]


def read_input(filepath: str) -> (str, dict[str, str]):
    rules = dict()
    with open(filepath, "r") as f:
        template = f.readline().rstrip()
        f.readline()
        for line in f.readlines():
            data = line.split("->")
            rules[data[0].strip()] = data[1].strip()
    return template, rules


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 14 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    template, rules = read_input(path)
    pairs = get_pairs(template)

    dont_double_count = Counter(template[1:-1])

    part1 = Counter()
    for pair in pairs:
        part1 += simulate(pair, 10)
    part1 -= dont_double_count
    print(f"Part 1: {score(part1)}")

    part2 = Counter()
    for pair in pairs:
        part2 += simulate(pair, 40)
    part2 -= dont_double_count
    print(f"Part 2: {score(part2)}")


def main(_):
    raise NotImplementedError
