from __future__ import annotations

import math
import re
from dataclasses import dataclass
from functools import reduce
from operator import mul
from typing import Callable

from .shared import Solution

RE_MONKEY = re.compile(r"^Monkey (\d):$")
RE_ITEMS = re.compile(r"^.*Starting items: (.*)$")
RE_OP = re.compile(r"^.*Operation:.*old (?P<op>.) (?P<amount>\d+|old)$")
RE_INT = re.compile(r"\d+")

ROUNDS_1 = 20
ROUNDS_2 = 100
# ROUNDS_2 = 10_000


@dataclass
class Monkey:
    id: int
    op: Callable[[int], int]
    test: Callable[[int], int]
    items: list[int]
    inspections: int = 0

    def __str__(self) -> str:
        return f"Monkey {self.id}: {self.items} {self.inspections}"

    def round(self, monkeys: dict[int, Monkey]):
        while len(self.items) > 0:
            self.inspections += 1
            item = self.items.pop(0)
            item = self.op(item)
            item = item // 3
            target = self.test(item)
            monkeys[target].give(item)

    def give(self, item: int):
        self.items.append(item)


@dataclass
class OptimizedItem:
    pass


@dataclass
class OptimizedMonkey:
    pass


def main(input_: list[str]) -> Solution:
    monkeys = dict()
    for i in range(0, len(input_), 7):
        monkey = parse_monkey(input_[i : i + 7])
        monkeys[monkey.id] = monkey

    # Part 1
    for i in range(ROUNDS_1):
        for monkey in monkeys.values():
            monkey.round(monkeys)
    part1 = monkey_business(monkeys)

    # Part 2
    for i in range(ROUNDS_1, ROUNDS_2):
        for monkey in monkeys.values():
            monkey.round(monkeys)
    part2 = monkey_business(monkeys)

    return Solution(part1, part2)


def parse_monkey(lines: list[str]) -> Monkey:
    id_ = int(RE_MONKEY.match(lines[0]).group(1))
    items = [int(item) for item in RE_ITEMS.match(lines[1]).group(1).split(",")]
    match RE_OP.match(lines[2]).groups():
        case ("*", "old"):
            op = lambda x: x * x
        case ("*", operand):
            op = lambda x: x * int(operand)
        case ("+", operand):
            op = lambda x: x + int(operand)
        case _:
            print("Unknown Operation!")
            op = lambda x: x
    test_params = [int(n) for n in RE_INT.findall("".join(lines[3:]))]
    test = lambda x: test_params[1] if x % test_params[0] == 0 else test_params[2]
    return Monkey(id_, op, test, items)


def monkey_business(monkeys: dict[int, Monkey]) -> int:
    return reduce(
        mul,
        sorted([monkey.inspections for monkey in monkeys.values()], reverse=True)[:2],
        1,
    )
