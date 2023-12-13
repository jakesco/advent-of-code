from collections.abc import Iterator
from dataclasses import dataclass
from pprint import pprint

from aoc.utils.interfaces import Solution

# TODO: Some kind of bug for part 2


@dataclass
class Pattern:
    pattern: list[str]

    def flip(self) -> list[str]:
        size = len(self.pattern[0])
        new_lines = [[] for _ in range(size)]
        for line in self.pattern:
            for i in range(size):
                new_lines[i].append(line[i])
        self.pattern = ["".join(line) for line in new_lines]
        return self.pattern

    def smudge(self) -> Iterator[list[str]]:
        for i in range(len(self.pattern)):
            for j in range(len(self.pattern[0])):
                c = self.pattern[i][j]
                self.pattern[i] = (
                    self.pattern[i][:j] + flip(c) + self.pattern[i][j + 1 :]
                )
                yield self.pattern
                self.pattern[i] = self.pattern[i][:j] + c + self.pattern[i][j + 1 :]


def main(puzzle_input: list[str]) -> Solution:
    patterns = parse_patterns(puzzle_input)
    part1 = sum(find_symetry(pattern) for pattern in patterns)
    patterns = parse_patterns(puzzle_input)
    part2 = sum(find_symetry_smudge(pattern) for pattern in patterns)
    return Solution(part1, part2)


def find_symetry(pattern: Pattern) -> int:
    """Finds symetry line and returns number of lines above/left of line."""
    return 100 * get_symetry_point(pattern.pattern) or get_symetry_point(pattern.flip())


def find_symetry_smudge(pattern: Pattern) -> int:
    old_h = get_symetry_point(pattern.pattern)
    for p in pattern.smudge():
        pprint(p)
        result = get_symetry_point(p)
        if result > 0 and result != old_h:
            return 100 * result
    pattern.flip()
    old_v = get_symetry_point(pattern.pattern)
    for p in pattern.smudge():
        result = get_symetry_point(p)
        if result > 0 and result != old_v:
            return result
    print(f"No alternate symetry found {pattern}")
    return 0


def get_symetry_point(pattern: list[str]) -> int:
    prev = pattern[0]
    for idx, line in enumerate(pattern[1:], start=1):
        if line == prev and all(
            x == y for x, y in zip(pattern[idx:], reversed(pattern[:idx]))
        ):
            return idx
        prev = line
    return 0


def parse_patterns(lines: list[str]) -> list[Pattern]:
    patterns = []
    acc = []
    for line in lines:
        if not line:
            patterns.append(Pattern(acc))
            acc = []
            continue
        acc.append(line)
    else:
        patterns.append(Pattern(acc))

    return patterns


def flip(c: str) -> str:
    return "#" if c == "." else "."


if __name__ == "__main__":
    sample = """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""
    x = main(sample.splitlines())
    print(x)
    assert x.part1 == 405
    assert x.part2 == 400
