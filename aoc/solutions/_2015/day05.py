import re
from collections import Counter

from aoc.utils.interfaces import Solution

VOWEL = "aeiou"
FORBIDDEN_STRINGS = {"ab", "cd", "pq", "xy"}
TWICE_IN_A_ROW = re.compile(r"(.)\1")

REPEATING_PAIR = re.compile(r"(..).*\1")
LETTER_BETWEEN = re.compile(r"(.).\1")


def main(puzzle_input: list[str]) -> Solution:
    part1 = sum(int(is_string_nice_1(string)) for string in puzzle_input)
    part2 = sum(int(is_string_nice_2(string)) for string in puzzle_input)
    return Solution(part1, part2)


def is_string_nice_1(string: str) -> bool:
    return all(
        [
            at_least_3_vowels(string),
            twice_in_a_row(string),
            no_forbidden_strings(string),
        ]
    )


def is_string_nice_2(string: str) -> bool:
    return all(
        [
            repeating_pair(string),
            letter_between(string),
        ]
    )


def at_least_3_vowels(string: str) -> bool:
    letter_count = Counter(string)
    vowels = sum([letter_count[c] for c in VOWEL])
    return vowels >= 3


def twice_in_a_row(string: str) -> bool:
    return TWICE_IN_A_ROW.search(string) is not None


def no_forbidden_strings(string: str) -> bool:
    return all([s not in string for s in FORBIDDEN_STRINGS])


def repeating_pair(string: str) -> bool:
    return REPEATING_PAIR.search(string) is not None


def letter_between(string: str) -> bool:
    return LETTER_BETWEEN.search(string) is not None
