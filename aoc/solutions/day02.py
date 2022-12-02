import enum
from pathlib import Path

from .shared import Solution


class Outcome(enum.IntEnum):
    LOSE = 0
    DRAW = 3
    WIN = 6


class Move(enum.IntEnum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3


OUTCOMES = {
    "X": Outcome.LOSE,
    "Y": Outcome.DRAW,
    "Z": Outcome.WIN,
}

MOVES = {
    "A": Move.ROCK,
    "B": Move.PAPER,
    "C": Move.SCISSORS,
    "X": Move.ROCK,
    "Y": Move.PAPER,
    "Z": Move.SCISSORS,
}


def main(filename: Path) -> Solution:
    solution = Solution()
    with filename.open() as f:
        for line in f.read().splitlines():
            moves = line.split(" ")
            opponent, you = [MOVES[m] for m in moves]
            solution.part1 += score(opponent, you)
            you = OUTCOMES[moves[1]]
            solution.part2 += score(opponent, you)
    return solution


def score(opponent: Move, you: Move | Outcome) -> int:
    if isinstance(you, Move):
        return you + get_outcome(opponent, you)
    else:
        return you + get_move(opponent, you)


def get_outcome(opponent: Move, you: Move) -> int:
    if opponent == you:
        return Outcome.DRAW

    # fmt: off
    match (opponent, you):
        case (Move.ROCK, Move.PAPER) \
             | (Move.PAPER, Move.SCISSORS) \
             | (Move.SCISSORS, Move.ROCK):
            return Outcome.WIN
        case _:
            return Outcome.LOSE
    # fmt: on


def get_move(opponent: Move, outcome: Outcome) -> int:
    if outcome == Outcome.DRAW:
        return opponent

    shift = opponent - 1
    if outcome == Outcome.WIN:
        return ((shift + 1) % 3) + 1

    if outcome == Outcome.LOSE:
        return ((shift - 1) % 3) + 1
