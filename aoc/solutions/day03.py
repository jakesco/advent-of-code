from .shared import Solution


def main(rucksacks: list[str]) -> Solution:
    priorities = [shared_priority(rucksack) for rucksack in rucksacks]
    part1 = sum(priorities)
    part2 = group_priorities(rucksacks)

    return Solution(part1, part2)


def shared_priority(rucksack: str) -> int:
    middle = len(rucksack) // 2
    left, right = rucksack[:middle], rucksack[middle:]
    for item in left:
        if item in right:
            return priority(item)
    return 0


def group_priorities(rucksacks: list[str]) -> int:
    total = 0
    while len(rucksacks) > 0:
        r1, r2, r3 = rucksacks.pop(), rucksacks.pop(), rucksacks.pop()
        for item in r1:
            if item in r2 and item in r3:
                total += priority(item)
                break
    return total


def priority(char: str) -> int:
    """Converts character's ascii code to priority
    a - z = 1 - 26
    A - Z = 27 - 52
    """
    byte = int.from_bytes(bytes(char, "ascii"), byteorder="big")
    return byte - 96 if byte > 96 else byte - 38
