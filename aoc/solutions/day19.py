from __future__ import annotations

import enum
import math
import re
from dataclasses import dataclass
from typing import NewType

from .shared import Solution

MAX_TICKS = 24


class Build(enum.StrEnum):
    ORE_BOT = "ore_robot"
    CLAY_BOT = "clay_robot"
    OBSIDIAN_BOT = "obsidian_robot"
    GEODE_BOT = "geode_robot"
    FINISH = ""


Sequence = NewType("Sequence", list)


@dataclass(frozen=True, slots=True)
class Price:
    ore: int
    clay: int
    obsidian: int


@dataclass(frozen=True, slots=True)
class Blueprint:
    number: int
    ore_robot: Price
    clay_robot: Price
    obsidian_robot: Price
    geode_robot: Price

    @classmethod
    def from_input(cls, blueprint: str) -> Blueprint:
        numbers = map(int, re.findall(r"\d+", blueprint))
        return cls(
            number=next(numbers),
            ore_robot=Price(next(numbers), 0, 0),
            clay_robot=Price(next(numbers), 0, 0),
            obsidian_robot=Price(next(numbers), next(numbers), 0),
            geode_robot=Price(next(numbers), 0, next(numbers)),
        )

    def get_price(self, action: Build) -> Price:
        return self.__getattribute__(action)


@dataclass(frozen=True, slots=True)
class State:
    ticks_left: int = MAX_TICKS
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geode: int = 0
    ore_robot: int = 1
    clay_robot: int = 0
    obsidian_robot: int = 0
    geode_robot: int = 0

    def stock(self) -> Price:
        return Price(self.ore, self.clay, self.obsidian)

    def production(self) -> Price:
        return Price(self.ore_robot, self.clay_robot, self.obsidian_robot)

    def to_dict(self) -> dict[str, int]:
        return {name: self.__getattribute__(name) for name in self.__slots__}


def main(input_: list[str]) -> Solution:
    blueprints = [Blueprint.from_input(line) for line in input_]

    order = Sequence(
        [
            Build.CLAY_BOT,
            Build.CLAY_BOT,
            Build.CLAY_BOT,
            Build.OBSIDIAN_BOT,
            Build.CLAY_BOT,
            Build.OBSIDIAN_BOT,
            Build.GEODE_BOT,
            Build.GEODE_BOT,
            Build.GEODE_BOT,
        ]
    )
    part1 = eval_sequence(blueprints[0], order, State())

    return Solution(part1.geode)


def eval_sequence(blueprint: Blueprint, sequence: Sequence, state: State) -> State:
    print(state)
    if len(sequence) <= 0:
        return state
    action = sequence[0]
    new_state = tick(blueprint, state, action)
    return eval_sequence(blueprint, sequence[1:], new_state)


def tick(blueprint: Blueprint, state: State, build: Build) -> State:
    state_dict = state.to_dict()

    if build == Build.FINISH:
        state_dict["geode"] += state_dict["ticks_left"] * state_dict["geode_robot"]
        state_dict["ticks_left"] = 0
        return State(**state_dict)

    price = blueprint.get_price(build)
    ticks = required_ticks(price, state.stock(), state.production())

    if ticks > state_dict["ticks_left"]:
        return tick(blueprint, state, Build.FINISH)

    # Gather resources
    for res in ("ore", "clay", "obsidian", "geode"):
        state_dict[res] += state_dict[f"{res}_robot"] * ticks

    # Do build
    for res in ("ore", "clay", "obsidian"):
        state_dict[res] -= getattr(price, res)
    state_dict[build] += 1

    state_dict["ticks_left"] -= ticks
    return State(**state_dict)


def required_ticks(price: Price, inventory: Price, production: Price) -> int:
    required = set()
    for res in ("ore", "clay", "obsidian"):
        current = getattr(inventory, res)
        target = getattr(price, res)
        prod = getattr(production, res)
        if target == 0:
            continue
        if target > 0 and prod == 0:
            return MAX_TICKS
        r = int(math.ceil((target - current) / prod))
        required.add(max(r, 0))
    return max(required) + 1
