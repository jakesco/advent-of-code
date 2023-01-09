from __future__ import annotations

import enum
import re
from collections import deque
from collections.abc import Generator
from dataclasses import dataclass
from functools import cache, partial
from pprint import pprint
from typing import NewType

from .shared import Solution

MAX_TICKS = 24

RESOURCES = ("ore", "clay", "obsidian")


class Build(enum.StrEnum):
    ORE_BOT = "ore"
    CLAY_BOT = "clay"
    OBSIDIAN_BOT = "obsidian"
    GEODE_BOT = "geode"


@dataclass(frozen=True, slots=True)
class Cost:
    ore: int = 0
    clay: int = 0
    obsidian: int = 0


@dataclass(frozen=True, slots=True)
class Blueprint:
    number: int
    ore: Cost
    clay: Cost
    obsidian: Cost
    geode: Cost

    @classmethod
    def from_input(cls, blueprint: str) -> Blueprint:
        numbers = map(int, re.findall(r"\d+", blueprint))
        return cls(
            number=next(numbers),
            ore=Cost(ore=next(numbers)),
            clay=Cost(ore=next(numbers)),
            obsidian=Cost(ore=next(numbers), clay=next(numbers)),
            geode=Cost(ore=next(numbers), obsidian=next(numbers)),
        )


@dataclass(frozen=True, slots=True)
class State:
    ticks_left: int = MAX_TICKS
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geodes: int = 0
    ore_bots: int = 1
    clay_bots: int = 0
    obsidian_bots: int = 0
    geode_bots: int = 0

    def projected_geodes(self) -> int:
        return self.geodes + (self.geode_bots * self.ticks_left)

    def finish(self) -> State:
        return State(ticks_left=0, geodes=self.projected_geodes())


def main(input_: list[str]) -> Solution:
    blueprints = [Blueprint.from_input(line) for line in input_]
    part1 = find_best_solution(blueprints[0])
    sequence = [
        Build.CLAY_BOT,
        Build.CLAY_BOT,
        Build.CLAY_BOT,
        Build.OBSIDIAN_BOT,
        Build.CLAY_BOT,
        Build.OBSIDIAN_BOT,
        Build.GEODE_BOT,
        Build.GEODE_BOT,
    ]
    state = State()
    for s in sequence:
        pprint(state)
        state = next_state(state, s, blueprints[0])
    return Solution(part1)


def find_best_solution(blueprint: Blueprint) -> int:
    """DFS to find the best geode count for given blueprint."""
    n = candidates(State(), blueprint)
    return 0


def candidates(state: State, blueprint: Blueprint) -> set[State]:
    """Returns next possible states"""
    states = set()
    for directive in Build:
        if new_state := next_state(state, directive, blueprint):
            states.add(new_state)
    return states


@cache
def next_state(state: State, build: Build, blueprint: Blueprint) -> State | None:
    """Generates next state based on build directive"""
    # Check that next state is possible
    cost = getattr(blueprint, build)
    if any(
        [
            getattr(cost, resource) > 0 and getattr(state, f"{resource}_bots") < 1
            for resource in RESOURCES
        ]
    ):
        return None

    ticks_required = max(
        [
            getattr(cost, r) - getattr(state, r) // getattr(state, f"{r}_bots")
            for r in RESOURCES
            if getattr(cost, r) > 0
        ]
    )

    if ticks_required > state.ticks_left:
        return state.finish()

    return State(
        ticks_left=state.ticks_left - ticks_required,
        ore=state.ore + (state.ore_bots * ticks_required) - cost.ore,
        clay=state.clay + (state.clay_bots * ticks_required) - cost.clay,
        obsidian=state.obsidian
        + (state.obsidian_bots * ticks_required)
        - cost.obsidian,
        geodes=state.geodes + (state.geode_bots * ticks_required),
        ore_bots=state.ore_bots + (1 if build == Build.ORE_BOT else 0),
        clay_bots=state.clay_bots + (1 if build == Build.CLAY_BOT else 0),
        obsidian_bots=state.obsidian_bots + (1 if build == Build.OBSIDIAN_BOT else 0),
        geode_bots=state.geode_bots + (1 if build == Build.GEODE_BOT else 0),
    )
