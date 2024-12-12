from dataclasses import dataclass
from functools import reduce
from typing import Self

from aoc.utils.interfaces import Solution


@dataclass
class Round:
    red: int
    green: int
    blue: int

    def power(self) -> int:
        return self.red * self.green * self.blue


@dataclass
class Game:
    id: int
    rounds: list[Round]

    @classmethod
    def from_str(cls, string: str) -> Self:
        game, rounds = string.split(":")
        id_ = int(game.split()[1])
        parsed_rounds = []
        for round in rounds.split(";"):
            colors = {
                "red": 0,
                "green": 0,
                "blue": 0,
            }
            for balls in round.split(","):
                number, color = balls.strip().split()
                colors[color] = int(number)
            parsed_rounds.append(Round(**colors))
        return Game(id_, parsed_rounds)


def main(puzzle_input: list[str]) -> Solution:
    games = [Game.from_str(line) for line in puzzle_input if line]
    part1 = sum(
        game.id
        for game in games
        if not any([r.red > 12 or r.green > 13 or r.blue > 14 for r in game.rounds])
    )
    part2 = sum(
        reduce(
            lambda a, b: Round(
                max(a.red, b.red), max(a.green, b.green), max(a.blue, b.blue)
            ),
            g.rounds[1:],
            g.rounds[0],
        ).power()
        for g in games
    )
    return Solution(part1, part2)


if __name__ == "__main__":
    sample = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""
    # part1: 8
    # part2: 2286
    print(main(sample.split("\n")))
