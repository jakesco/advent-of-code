from collections import deque
from collections.abc import Iterable

from aoc.utils.interfaces import Solution


def main(lines: list[str]) -> Solution:
    lines = [int(x) for x in lines]
    return Solution(
        count_increases(lines, 1),
        count_increases(lines, 3),
    )


def count_increases(lines: Iterable[int], window: int):
    deck = deque(maxlen=window)
    first = True
    count = 0
    prev = 0

    for n in lines:
        deck.append(n)
        if len(deck) < window:
            continue
        total = sum(deck)
        if first:
            prev = total
            first = False
            continue
        if total > prev:
            count += 1
        prev = total
    return count
