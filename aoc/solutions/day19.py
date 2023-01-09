from __future__ import annotations

import enum
import math
import re
import time
from collections import deque
from dataclasses import dataclass
from multiprocessing import Pool

from .shared import Solution

MAX_TICKS = 20

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

    def score(self, blueprint_number: int) -> int:
        return blueprint_number * self.projected_geodes()

    def projected_geodes(self) -> int:
        return self.geodes + (self.geode_bots * self.ticks_left)

    def finished(self) -> bool:
        return self.ticks_left <= 0

    def finish(self) -> State:
        return State(
            ticks_left=0,
            ore=self.ore + (self.ore_bots * self.ticks_left),
            clay=self.clay + (self.clay_bots * self.ticks_left),
            obsidian=self.obsidian + (self.obsidian_bots * self.ticks_left),
            geodes=self.geodes + (self.geode_bots * self.ticks_left),
            ore_bots=self.ore_bots,
            clay_bots=self.clay_bots,
            obsidian_bots=self.obsidian_bots,
            geode_bots=self.geode_bots,
        )


def main(input_: list[str]) -> Solution:
    blueprints = [Blueprint.from_input(line) for line in input_]
    blueprints = [blueprints[0]]
    with Pool() as pool:
        scores = pool.map(find_best_solution, blueprints)
    part1 = max(scores)
    return Solution(part1)


def find_best_solution(blueprint: Blueprint) -> int:
    """DFS to find the best geode count for given blueprint."""
    # TODO: Limit search space more
    stack = deque([State()])
    discovered = set()
    max_geodes = 0
    while stack:
        current = stack.pop()
        # print(current)
        if current in discovered:
            continue
        discovered.add(current)
        for candidate in candidates(current, blueprint):
            if candidate.finished():
                max_geodes = max(max_geodes, candidate.geodes)
            else:
                stack.append(candidate)
    return max_geodes * blueprint.number


def candidates(state: State, blueprint: Blueprint) -> set[State]:
    """Returns next possible states"""
    states = set()
    for directive in Build:
        if new_state := next_state(state, directive, blueprint):
            states.add(new_state)
    return states


def next_state(state: State, build: str, blueprint: Blueprint) -> State | None:
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

    ticks_required = 1 + max(
        [
            math.ceil(
                (getattr(cost, r) - getattr(state, r)) / getattr(state, f"{r}_bots")
            )
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
