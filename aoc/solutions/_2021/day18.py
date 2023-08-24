import argparse
import os
import re
from abc import ABC, abstractmethod
from collections import deque
from dataclasses import dataclass
from itertools import permutations
from math import ceil, floor


class Element(ABC):
    @abstractmethod
    def _literals(self, acc) -> list["Literal"]:
        pass

    @abstractmethod
    def split(self):
        pass

    @abstractmethod
    def add(self, other: "Element"):
        pass

    @abstractmethod
    def can_explode(self) -> bool:
        pass

    @abstractmethod
    def can_split(self) -> bool:
        pass

    @abstractmethod
    def magnitude(self) -> int:
        pass

    @staticmethod
    def get_literals(s: "Element") -> list["Literal"]:
        acc = []
        s._literals(acc)
        return acc


@dataclass
class SnailFishNumber(Element):
    l: Element
    r: Element

    def __repr__(self):
        return f"[{self.l}, {self.r}]"

    def _literals(self, acc):
        self.l._literals(acc)
        self.r._literals(acc)

    def magnitude(self) -> int:
        return 3 * self.l.magnitude() + 2 * self.r.magnitude()

    def add(self, other: Element) -> "SnailFishNumber":
        SnailFishNumber(self, other)
        # print(f"{self} + {other}")
        # print(f"= {result}")
        return SnailFishNumber(self, other)

    def reduce(self):
        """Reduces Snailfish number."""
        explode = self.explode(0)
        if explode is not None:
            self.apply_explode(explode)

        while self.can_split() or explode is not None:
            # Do all explodes
            while explode is not None:
                explode = self.explode(0)
                if explode is not None:
                    self.apply_explode(explode)
            # split
            if not self.split():
                continue

            explode = self.explode(0)
            if explode is not None:
                self.apply_explode(explode)

    def apply_explode(self, value: tuple[int, int]):
        literals = Element.get_literals(self)
        idx = literals.index(Literal(-1))
        if idx - 1 >= 0:
            literals[idx - 1].add(value[0])
        if idx + 1 < len(literals):
            literals[idx + 1].add(value[1])
        literals[idx].add(1)

    def explode(self, depth) -> tuple[int, int] | None:
        """
        Explodes left most explodeable value, returns (left,right) tuple
        if explode happened else None. (left,right) tuple must be applied
        via self.apply_explode before explode is complete.
        """
        # If a pair is nested in 4 pairs, the left pair explodes
        if depth >= 3 and self.l.can_explode():
            left = self.l.l.value
            right = self.l.r.value
            self.l = Literal(-1)
            return (left, right)
        if depth >= 3 and self.r.can_explode():
            left = self.r.l.value
            right = self.r.r.value
            self.r = Literal(-1)
            return (left, right)
        if isinstance(self.l, SnailFishNumber):
            done = self.l.explode(depth + 1)
            if done is not None:
                return done
        if isinstance(self.r, SnailFishNumber):
            done = self.r.explode(depth + 1)
            if done is not None:
                return done
        return None

    def can_explode(self) -> bool:
        return isinstance(self.l, Literal) and isinstance(self.r, Literal)

    def can_split(self) -> bool:
        literals = Element.get_literals(self)
        return any([l.value >= 10 for l in literals])

    def split(self) -> bool:
        """Splits left most splittable value, returns True if split was made."""
        if isinstance(self.l, SnailFishNumber):
            if done := self.l.split():
                return done
        if isinstance(self.l, Literal) and self.l.value >= 10:
            self.l = self.l.split()
            return True
        if isinstance(self.r, SnailFishNumber):
            if done := self.r.split():
                return done
        if isinstance(self.r, Literal) and self.r.value >= 10:
            self.r = self.r.split()
            return True
        return False


@dataclass
class Literal(Element):
    value: int

    def __repr__(self):
        return str(self.value)

    def _literals(self, acc):
        acc.append(self)

    def magnitude(self) -> int:
        return self.value

    def can_explode(self) -> bool:
        return False

    def can_split(self) -> bool:
        return self.value >= 10

    def add(self, val: int):
        self.value += val

    def split(self) -> Element:
        if not self.can_split():
            return self
        half = self.value / 2
        left = floor(half)
        right = ceil(half)
        return SnailFishNumber(Literal(left), Literal(right))


def parse_helper(input_: deque[str]) -> Element:
    c = input_.popleft()
    if c.isdigit():
        return Literal(int(c))
    left = parse_helper(input_)
    right = parse_helper(input_)
    return SnailFishNumber(left, right)


def parse(input_: str) -> Element:
    """Parses Snailfish Number."""
    chars = re.findall(r"\[|\d", input_.strip())
    snail_string = deque([c for c in chars])
    return parse_helper(snail_string)


def read_input(filepath: str) -> list[str]:
    output = list()
    with open(filepath, "r") as f:
        for line in f.readlines():
            output.append(line.strip())
    return output


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 18 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    snailfish_strings = read_input(path)

    numbers = [parse(n) for n in snailfish_strings]
    base = numbers[0]
    for sn in numbers[1:]:
        base = base.add(sn)
        base.reduce()
    print(f"Part 1: {base.magnitude()}")

    max_magnitude = 0
    for n, m in permutations(snailfish_strings, 2):
        x = parse(n)
        y = parse(m)
        sum_ = x.add(y)
        sum_.reduce()
        max_magnitude = max(sum_.magnitude(), max_magnitude)
    print(f"Part 2: {max_magnitude}")


def main(_):
    raise NotImplementedError
