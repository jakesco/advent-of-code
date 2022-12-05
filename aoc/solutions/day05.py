import copy
import re
from collections import deque
from dataclasses import dataclass

from .shared import Solution


@dataclass
class Move:
    times: int
    src: int
    dest: int


def main(input_: list[str]) -> Solution:

    split = input_.index("")
    crates = input_[:split]
    moves = input_[split + 1 :]

    todo = parse_moves(moves)

    stacks_1 = parse_stacks(crates)
    stacks_2 = copy.deepcopy(stacks_1)
    for t in todo:
        apply_move_single(t, stacks_1)
        apply_move_multi(t, stacks_2)

    return Solution(
        "".join([s[0] for s in stacks_1.values()]),
        "".join([s[0] for s in stacks_2.values()]),
    )


def parse_stacks(crates: list[str]) -> dict[int, deque]:
    stack_numbers = [int(n) for n in re.findall(r"\d", crates.pop())]
    stacks = {num: deque() for num in stack_numbers}
    for line in crates:
        for idx, i in enumerate(range(1, len(stacks.keys()) * 4, 4), start=1):
            letter = line[i]
            if letter == " ":
                continue
            stacks[idx].append(letter)
    return stacks


def parse_moves(moves: list[str]) -> list[Move]:
    todo = []
    for line in moves:
        todo.append(Move(*[int(m) for m in re.findall(r"\d+", line)]))
    return todo


def apply_move_single(move: Move, stacks: dict[int, deque]):
    for i in range(move.times):
        stacks[move.dest].appendleft(stacks[move.src].popleft())


def apply_move_multi(move: Move, stacks: dict[int, deque]):
    tmp = []
    for i in range(move.times):
        tmp.append(stacks[move.src].popleft())
    tmp.reverse()
    stacks[move.dest].extendleft(tmp)
