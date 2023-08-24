import argparse
import os
from collections import deque
from dataclasses import dataclass
from functools import cache
from itertools import product

DIRAC_ROLL_COMBOS = [sum(r) for r in product((1, 2, 3), repeat=3)]


class Die:
    def __init__(self):
        self.__gen = deque(range(1, 101))
        self.__rolls = 0

    @property
    def rolls(self) -> int:
        return self.__rolls

    def roll(self):
        self.__rolls += 1
        if len(self.__gen) <= 0:
            self.__gen = deque(range(1, 101))
        return self.__gen.popleft()


@dataclass(frozen=True)
class Player:
    position: int
    points: int = 0


def calc_position(player: Player, move: int) -> int:
    return ((player.position - 1 + move) % 10) + 1


def roll(player: Player, die: Die) -> Player:
    move = 0
    for i in range(3):
        move += die.roll()
    new_position = calc_position(player, move)
    return Player(new_position, player.points + new_position)


def winner(player: Player) -> bool:
    return player.points >= 1000


def part1(p1: Player, p2: Player) -> int:
    die = Die()
    while True:
        p1 = roll(p1, die)
        if winner(p1):
            break
        p2 = roll(p2, die)
        if winner(p2):
            break
    loser = p1 if winner(p2) else p2
    return loser.points * die.rolls


def simulate_turn(player: Player, move: int) -> Player:
    new_position = calc_position(player, move)
    new_score = player.points + new_position
    return Player(new_position, new_score)


@cache
def part2(p1: Player, p2: Player) -> dict[int, int]:
    wins = {1: 0, 2: 0}
    for dirac_roll in DIRAC_ROLL_COMBOS:
        next_p1 = simulate_turn(p1, dirac_roll)
        if next_p1.points >= 21:
            wins[1] += 1
        else:
            next_turn = part2(p2, next_p1)
            wins[1] += next_turn[2]
            wins[2] += next_turn[1]
    return wins


def read_input(filepath: str) -> (Player, Player):
    with open(filepath, "r") as f:
        start1 = int(f.readline().split(":")[1].strip())
        start2 = int(f.readline().split(":")[1].strip())
    return Player(start1), Player(start2)


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 21 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    player1, player2 = read_input(path)

    print(f"Part 1: {part1(player1, player2)}")

    wins = part2(player1, player2)
    print(f"Part 2: {max(wins[1], wins[2])}")


def main(_):
    raise NotImplementedError
