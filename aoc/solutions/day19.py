from __future__ import annotations

import enum
import re
from dataclasses import dataclass
from functools import cache
from multiprocessing import Pool

from .shared import Solution


class Build(enum.StrEnum):
    ORE_BOT = "ore_robot"
    CLAY_BOT = "clay_robot"
    OBSIDIAN_BOT = "obsidian_robot"
    GEODE_BOT = "geode_robot"
    FINISH = ""


@dataclass(frozen=True, slots=True)
class Price:
    ore: int
    clay: int
    obsidian: int

    def __str__(self) -> str:
        return " + ".join(
            [
                f"{self.__getattribute__(name)} {name}"
                for name in self.__slots__
                if self.__getattribute__(name) > 0
            ]
        )

    def __ge__(self, other: Price) -> bool:
        return all(
            [
                self.ore >= other.ore,
                self.clay >= other.clay,
                self.obsidian >= other.obsidian,
            ]
        )


@dataclass(frozen=True, slots=True)
class Blueprint:
    number: int
    ore_robot: Price
    clay_robot: Price
    obsidian_robot: Price
    geode_robot: Price

    def __str__(self) -> str:
        formatted = [
            f"Blueprint {self.number}",
            f"  Ore Robot: {self.ore_robot}",
            f"  Clay Robot: {self.clay_robot}",
            f"  Obsidian Robot: {self.obsidian_robot}",
            f"  Geode Robot: {self.geode_robot}",
            "",
        ]
        return "\n".join(formatted)

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
class Inventory:
    # TODO: Merge this back into state
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

    def to_dict(self) -> dict[str, int]:
        return {name: self.__getattribute__(name) for name in self.__slots__}


@dataclass
class State:
    time_left: int = 24
    inventory: Inventory = Inventory()


def main(input_: list[str]) -> Solution:
    blueprints = [Blueprint.from_input(line) for line in input_]
    # part1 = find_best_geodes(blueprints[0])
    # with Pool() as p:
    #     part1 = max(p.map(find_best_geodes, blueprints))
    ticks_left = 24
    order = [
        Build.CLAY_BOT,
        Build.CLAY_BOT,
        Build.CLAY_BOT,
        Build.OBSIDIAN_BOT,
        Build.CLAY_BOT,
        Build.OBSIDIAN_BOT,
        Build.GEODE_BOT,
        Build.GEODE_BOT,
    ]
    inv = Inventory()
    for o in order:
        t, inv = tick(blueprints[0], inv, o)
        ticks_left -= t
        print(ticks_left, inv)

    return Solution()


def find_best_geodes(blueprint: Blueprint) -> int:
    start = State()
    q = [start]
    done = set()
    iter = 0
    while q:
        state = q.pop()
        time_left = state.time_left - 1
        print(iter)
        iter += 1

        for action in available_actions(blueprint, state.inventory.stock()):
            inv = tick(blueprint, state.inventory, action)
            if inv.clay > 50 or inv.ore > 50:
                continue
            next_state = State(time_left, inv)

            if next_state.time_left <= 0:
                done.add(inv)
            else:
                q.append(next_state)

    return max([i.geode for i in done])


@cache
def ailable_actions(inventory: Inventory) -> set[Build]:
    actions = set()
    if inventory.ore_robot:
        actions.add(Build.ORE_BOT)
        actions.add(Build.CLAY_BOT)
    if inventory.clay_robot:
        actions.add(Build.OBSIDIAN_BOT)
    if inventory.obsidian_robot:
        actions.add(Build.GEODE_BOT)
    return actions


@cache
def tick(blueprint: Blueprint, inventory: Inventory, build: Build) -> (int, Inventory):
    """Returns time elapsed and new inventory"""
    state = inventory.to_dict()

    if build == Build.FINISH:
        state["geode"] += 3
        return Inventory(**state)

    price = blueprint.get_price(build)

    required = max(
        [
            # TODO: divide by robot collection speed
            (getattr(price, res, 2) - state.get(res, 1)) // state.get(f"{res}_robot", 1)
            for res in ("ore", "clay", "obsidian", "default")
        ]
    )

    # Gather resources
    for res in ("ore", "clay", "obsidian", "geode"):
        state[res] += state[f"{res}_robot"] * required

    # Do build
    for res in ("ore", "clay", "obsidian"):
        state[res] -= getattr(price, res)
    state[build] += 1

    return required + 1, Inventory(**state)
