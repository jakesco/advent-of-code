from __future__ import annotations

import operator
import re
from copy import deepcopy
from dataclasses import dataclass
from functools import reduce
from typing import Callable

from .shared import Solution

RE_OP = re.compile(r"^.*Operation:.*old (?P<op>.) (?P<amount>\d+|old)$")
RE_INT = re.compile(r"\d+")

ROUNDS_1 = 20
ROUNDS_2 = 10_000

lcm = 1


@dataclass
class Monkey:
    id: int
    op: Callable[[int], int]
    test: Callable[[int], int]
    items: list[int]
    inspections: int = 0

    def __str__(self) -> str:
        return f"Monkey {self.id}: {self.items} {self.inspections}"

    def round(self, monkeys: dict[int, Monkey], part: int):
        while len(self.items) > 0:
            self.inspections += 1
            item = self.items.pop(0)
            item = self.op(item)
            if part == 1:
                item //= 3
            else:
                item %= lcm
            target = self.test(item)
            monkeys[target].give(item)

    def give(self, item: int):
        self.items.append(item)


def main(input_: list[str]) -> Solution:
    monkeys = dict()
    for i in range(0, len(input_), 7):
        monkey = parse_monkey(input_[i : i + 7])
        monkeys[monkey.id] = monkey

    monkeys_1 = deepcopy(monkeys)
    part1 = simulate_rounds(monkeys_1, part=1)

    monkeys_2 = deepcopy(monkeys)
    part2 = simulate_rounds(monkeys_2, part=2)

    return Solution(part1, part2)


def parse_monkey(lines: list[str]) -> Monkey:
    id_ = int(RE_INT.findall(lines[0])[0])
    items = [int(item) for item in RE_INT.findall(lines[1])]

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

    global lcm
    lcm *= test_params[0]

    return Monkey(id_, op, test, items)


def simulate_rounds(monkeys: dict[int, Monkey], part: int) -> int:
    rounds = ROUNDS_2 if part == 2 else ROUNDS_1
    for i in range(rounds):
        for monkey in monkeys.values():
            monkey.round(monkeys, part=part)
    return monkey_business(monkeys)


def monkey_business(monkeys: dict[int, Monkey]) -> int:
    return reduce(
        operator.mul,
        sorted([monkey.inspections for monkey in monkeys.values()], reverse=True)[:2],
        1,
    )
