from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    part1 = sum([fuel_mass(mass) for mass in map(int, puzzle_input)])
    part2 = sum([recursive_fuel_mass(mass) for mass in map(int, puzzle_input)])
    return Solution(part1, part2)


def fuel_mass(mass: int) -> int:
    return max(mass // 3 - 2, 0)


def recursive_fuel_mass(mass: int) -> int:
    fuel = fuel_mass(mass)
    if fuel <= 0:
        return 0
    return fuel + recursive_fuel_mass(fuel)
