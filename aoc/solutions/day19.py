from __future__ import annotations

import enum
import re
import time
from collections import deque
from collections.abc import Generator
from dataclasses import dataclass
from functools import cache
from itertools import combinations_with_replacement, islice, product
from multiprocessing import Pool
from typing import NewType

from .shared import Solution

MAX_TICKS = 24


class Build(enum.StrEnum):
    ORE_BOT = "ore"
    CLAY_BOT = "clay"
    OBSIDIAN_BOT = "obsidian"
    GEODE_BOT = "geode"
    NOP = ""


Sequence = NewType("Sequence", tuple[Build, ...])


@dataclass(frozen=True, slots=True)
class Resource:
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geode: int = 0

    @classmethod
    def make(cls, bot: Build) -> Resource:
        match bot:
            case Build.ORE_BOT:
                return cls(ore=1)
            case Build.CLAY_BOT:
                return cls(clay=1)
            case Build.OBSIDIAN_BOT:
                return cls(obsidian=1)
            case Build.GEODE_BOT:
                return cls(geode=1)
            case _:
                return cls()

    def scalar(self, scalar: int) -> Resource:
        return Resource(
            self.ore * scalar,
            self.clay * scalar,
            self.obsidian * scalar,
            self.geode * scalar,
        )

    def __add__(self, other: Resource) -> Resource:
        return Resource(
            self.ore + other.ore,
            self.clay + other.clay,
            self.obsidian + other.obsidian,
            self.geode + other.geode,
        )

    def __sub__(self, other: Resource) -> Resource:
        return Resource(
            self.ore - other.ore,
            self.clay - other.clay,
            self.obsidian - other.obsidian,
            self.geode - other.geode,
        )

    def __gt__(self, other: Resource) -> bool:
        return (
            self.ore > other.ore
            and self.clay > other.clay
            and self.obsidian > other.obsidian
        )

    def __ge__(self, other: Resource) -> bool:
        return (
            self.ore >= other.ore
            and self.clay >= other.clay
            and self.obsidian >= other.obsidian
        )


@dataclass(frozen=True, slots=True)
class State:
    ticks_left: int = MAX_TICKS
    inventory: Resource = Resource()
    production: Resource = Resource(ore=1)


@dataclass(frozen=True, slots=True)
class Blueprint:
    number: int
    ore: Resource
    clay: Resource
    obsidian: Resource
    geode: Resource

    @classmethod
    def from_input(cls, blueprint: str) -> Blueprint:
        numbers = map(int, re.findall(r"\d+", blueprint))
        return cls(
            number=next(numbers),
            ore=Resource(ore=next(numbers)),
            clay=Resource(ore=next(numbers)),
            obsidian=Resource(ore=next(numbers), clay=next(numbers)),
            geode=Resource(ore=next(numbers), obsidian=next(numbers)),
        )


def main(input_: list[str]) -> Solution:
    blueprints = [Blueprint.from_input(line) for line in input_]
    # sequence = Sequence(
    #         (
    #             Build.CLAY_BOT,
    #             Build.CLAY_BOT,
    #             Build.CLAY_BOT,
    #             Build.OBSIDIAN_BOT,
    #             Build.CLAY_BOT,
    #             Build.OBSIDIAN_BOT,
    #             Build.GEODE_BOT,
    #             Build.GEODE_BOT,
    #         )
    #     )
    part1 = find_best_sequence(blueprints[0])
    return Solution(part1)


def find_best_sequence(blueprint: Blueprint) -> int:
    states = set()
    skip = False
    sequences = iter_sequences()
    sequences.send(None)
    while True:
        try:
            sequence = sequences.send(skip)
        except StopIteration:
            break
        if state := eval_sequence(blueprint, sequence, State()):
            skip = False
            states.add(state)
        else:
            skip = True
    return max([state.inventory.geode for state in states])


def iter_sequences() -> Generator[Sequence]:
    q = {(build,) for build in Build.__members__.values()}
    i = 0
    while q:
        prefix = q.pop()
        for action in Build.__members__.values():
            seq = (*prefix, action)
            skip = yield seq
            if skip or len(seq) > MAX_TICKS:
                continue
            i += 1
            print(i, len(seq), seq)
            q.add(seq)


@cache
def eval_sequence(
    blueprint: Blueprint, sequence: Sequence, state: State
) -> State | None:
    if len(sequence) <= 0 or state is None:
        return state
    action = sequence[0]
    new_state = tick(blueprint, state, action)
    return eval_sequence(blueprint, sequence[1:], new_state)


def tick(blueprint: Blueprint, state: State, build: Build) -> State | None:
    ticks = ticks_required(blueprint, state, build)
    cost = getattr(blueprint, build, Resource())

    if not can_build(ticks, cost, state):
        return None

    # Gather resources
    inventory = state.inventory + state.production.scalar(ticks)

    # Do build
    inventory = inventory - getattr(blueprint, build, Resource())
    production = state.production + Resource.make(build)

    return State(inventory, production)


def ticks_required(blueprint: Blueprint, state: State, build: Build) -> int:
    # TODO
    pass


def can_build(ticks: int, cost: Resource, state: State) -> bool:
    return ticks <= state.ticks_left and state.inventory - cost >= Resource()
