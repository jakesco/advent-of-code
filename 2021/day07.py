import argparse
import os


def read_input(filepath: str) -> list[int]:
    with open(filepath, "r") as f:
        input_ = [int(n) for n in f.readline().split(",")]
        input_.sort()
        return input_


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 7 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


def median(data: list[int]) -> int:
    idx = len(data) // 2
    return data[idx]


def fuel_cost_part1(data: list[int], destination: int) -> int:
    fuel = 0
    for d in data:
        fuel += abs(d - destination)
    return fuel


def fuel_cost_part2(data: list[int], destination: int) -> int:
    fuel = 0
    for d in data:
        steps = abs(d - destination)
        # 1 + 2 + 3 + ... + n = n(n + 1) / 2
        cost = (steps * (steps + 1)) / 2
        fuel += cost
    return int(fuel)


def search_field(
    data: list[int], search_space: range, start: (int, int), cost_function: callable
) -> (int, int):
    best = start
    for i in search_space:
        new = (i, cost_function(data, i))
        if new[1] < best[1]:
            best = new
        else:
            return best
    return best


def find_min_fuel(data: list[int], cost_function: callable) -> (int, int):
    med = median(data)

    search_left = range(med - 1, data[0], -1)
    search_right = range(med + 1, data[-1])

    start = (med, cost_function(data, med))

    best_left = search_field(data, search_left, start, cost_function)
    best_right = search_field(data, search_right, start, cost_function)

    return min(best_left, best_right, key=lambda x: x[1])


if __name__ == "__main__":
    path = init_parser()
    positions = read_input(path)

    # best = find_min_fuel(positions, fuel_cost_part1)
    best = find_min_fuel(positions, fuel_cost_part2)

    print(f"Best position: {best[0]} | Cost: {best[1]}")


def main(_):
    raise NotImplementedError
