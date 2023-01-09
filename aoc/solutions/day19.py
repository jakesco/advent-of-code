from __future__ import annotations

import enum
import math
import re
import time
from collections import deque
from dataclasses import dataclass
from multiprocessing import Pool

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
    part1 = part2 = 0
    blueprints = [Blueprint.from_input(line) for line in input_]
    with Pool() as pool:
        scores = pool.map(find_best_solution, [blueprints[0]])
    part1 = sum(scores)
    return Solution(part1, part2)


def find_best_solution(blueprint: Blueprint) -> int:
    """DFS to find the best geode count for given blueprint."""
    start = time.perf_counter()
    checked = removed = 0
    stack = deque([State()])
    discovered = set()
    best_so_far = 0
    while stack:
        current = stack.pop()
        if current in discovered:
            continue
        discovered.add(current)
        if early_abort(current, blueprint, best_so_far):
            removed += 1
            continue
        for candidate in candidates(current, blueprint):
            checked += 1
            if candidate.finished():
                best_so_far = max(best_so_far, candidate.geodes)
            else:
                stack.append(candidate)
    end = time.perf_counter()
    print(f"Search Done {blueprint.number}: {best_so_far} ({checked} - {removed})")
    print(f"Search Time: {end - start}s")
    return best_so_far * blueprint.number


def candidates(state: State, blueprint: Blueprint) -> list[State]:
    """Returns next possible states"""
    states = list()
    for directive in Build:
        if new_state := next_state(state, directive, blueprint):
            states.append(new_state)
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


def early_abort(state: State, blueprint: Blueprint, current_best: int) -> bool:
    """Basic ideas for early abort"""
    # TODO: Better heuristics to limit search space
    if state.obsidian_bots == 0 and blueprint.geode.obsidian > state.ticks_left:
        # Can't get enough obsidian to build geode bot in ticks left
        return True
    if prefect_projected_geodes(state) < current_best:
        # Can't possibly get more geodes than current best
        return True
    return False


def prefect_projected_geodes(state: State) -> int:
    """End number of geodes assuming a geode bot is built every tick"""
    additional = 0
    bots = state.geode_bots
    for _ in range(state.ticks_left):
        additional += bots
        bots += 1
    return state.geodes + additional
