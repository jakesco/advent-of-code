from dataclasses import dataclass

from aoc.utils.interfaces import Solution

# TODO: Try using quadratic equation


@dataclass
class Race:
    time: int
    record: int


def main(puzzle_input: list[str]) -> Solution:
    races = [
        Race(*map(int, r))
        for r in zip(puzzle_input[0].split()[1:], puzzle_input[1].split()[1:])
    ]
    part1 = 1
    for race in races:
        part1 *= upper_bound(race) - lower_bound(race) + 1

    race_2 = Race(
        *map(
            int, ("".join(filter(lambda c: c.isdigit(), line)) for line in puzzle_input)
        )
    )
    part2 = upper_bound(race_2) - lower_bound(race_2) + 1

    return Solution(part1, part2)


def lower_bound(race: Race) -> int:
    lb, ub = 0, race.time // 2
    while ub >= lb:
        check = lb + (ub - lb) // 2
        d = calculate_distance(check, race.time)
        if d <= race.record:
            lb = check + 1
        else:
            ub = check - 1
    return check + 1 if d <= race.record else check


def upper_bound(race: Race) -> int:
    lb, ub = race.time // 2, race.time
    while ub >= lb:
        check = lb + (ub - lb) // 2
        d = calculate_distance(check, race.time)
        if d <= race.record:
            ub = check - 1
        else:
            lb = check + 1
    return check - 1 if d <= race.record else check


def calculate_distance(hold: int, time: int) -> int:
    return (time - hold) * hold


if __name__ == "__main__":
    sample = """Time:      7  15   30
Distance:  9  40  200"""
    s = main(sample.split("\n"))
    print(s)
    assert s.part1 == 288
    assert s.part2 == 71503
    # 449550
    # 28360140
