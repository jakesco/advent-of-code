from __future__ import annotations

import enum
import math
import re
from dataclasses import dataclass
from functools import cache
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


@dataclass(frozen=True, slots=True)
class State:
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
    sequence = Sequence(
        (
            Build.NOP,
            Build.NOP,
            Build.CLAY_BOT,
            Build.NOP,
            Build.CLAY_BOT,
            Build.NOP,
            Build.CLAY_BOT,
            Build.NOP,
            Build.NOP,
            Build.NOP,
            Build.OBSIDIAN_BOT,
            Build.CLAY_BOT,
            Build.NOP,
            Build.NOP,
            Build.OBSIDIAN_BOT,
            Build.NOP,
            Build.NOP,
            Build.GEODE_BOT,
            Build.NOP,
            Build.NOP,
            Build.GEODE_BOT,
            Build.NOP,
            Build.NOP,
            Build.NOP,
        )
    )

    for _ in range(1_000_000):
        eval_sequence(blueprints[0], sequence, State())

    return Solution()


@cache
def eval_sequence(blueprint: Blueprint, sequence: Sequence, state: State) -> State:
    # print(state)
    if len(sequence) <= 0:
        return state
    action = sequence[0]
    new_state = tick(blueprint, state, action)
    return eval_sequence(blueprint, sequence[1:], new_state)


def tick(blueprint: Blueprint, state: State, build: Build) -> State:
    # Gather resources
    inventory = state.inventory + state.production

    # Do build
    inventory = inventory - getattr(blueprint, build, Resource())
    production = state.production + Resource.make(build)

    return State(inventory, production)
