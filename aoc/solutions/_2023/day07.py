from collections import Counter
from dataclasses import dataclass
from enum import IntEnum
from functools import cache, cmp_to_key, partial
from typing import Self

from aoc.utils.interfaces import Solution

RANKS = ("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
RANKS_2 = ("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")


class HandType(IntEnum):
    FIVE = 6
    FOUR = 5
    FULL = 4
    THREE = 3
    TWO = 2
    ONE = 1
    HIGH = 0


@dataclass(frozen=True)
class Hand:
    hand: str
    bid: int

    @classmethod
    def from_str(cls, hand: str) -> Self:
        h, b = hand.split()
        return cls(h, int(b))


def main(puzzle_input: list[str]) -> Solution:
    hands = [Hand.from_str(hand) for hand in puzzle_input]
    hands1 = sorted(hands, key=cmp_to_key(cmp))
    cmp2 = partial(cmp, jokers=True, ranks=RANKS_2)
    hands2 = sorted(hands, key=cmp_to_key(cmp2))
    return Solution(winnings(hands1), winnings(hands2))


@cache
def hand_type(hand: str, jokers: bool) -> HandType:
    counts = Counter(hand)
    if jokers:
        nj = counts["J"]
        del counts["J"]

    c = sorted(counts.values(), reverse=True)
    match c:
        case [5, *_rest]:
            return HandType.FIVE
        case [4, *_rest]:
            if jokers and nj == 1:
                return HandType.FIVE
            return HandType.FOUR
        case [3, 2, *_rest]:
            return HandType.FULL
        case [3, *_rest]:
            if jokers:
                if nj == 1:
                    return HandType.FOUR
                if nj == 2:
                    return HandType.FIVE
            return HandType.THREE
        case [2, 2, *_rest]:
            if jokers and nj == 1:
                return HandType.FULL
            return HandType.TWO
        case [2, *_rest]:
            if jokers:
                if nj == 3:
                    return HandType.FIVE
                if nj == 2:
                    return HandType.FOUR
                if nj == 1:
                    return HandType.THREE
            return HandType.ONE
        case [1, *_rest]:
            if jokers:
                if nj == 4:
                    return HandType.FIVE
                if nj == 3:
                    return HandType.FOUR
                if nj == 2:
                    return HandType.THREE
                if nj == 1:
                    return HandType.ONE
            return HandType.HIGH
        case _:
            # Should only get here if JJJJJ
            return HandType.FIVE


def cmp(a: Hand, b: Hand, *, jokers: bool = False, ranks: list[str] = RANKS) -> int:
    h1, h2 = hand_type(a.hand, jokers), hand_type(b.hand, jokers)
    if h1 != h2:
        return -1 if h1 < h2 else 1
    for c1, c2 in zip(a.hand, b.hand):
        if c1 == c2:
            continue
        x, y = ranks.index(c1), ranks.index(c2)
        return -1 if x < y else 1
    return 0


def winnings(hands: list[Hand]) -> int:
    return sum(idx * hand.bid for idx, hand in enumerate(hands, start=1))


if __name__ == "__main__":
    sample = """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""
    s = main(sample.split("\n"))
    print(s)
    assert s.part1 == 6440
    assert s.part2 == 5905
    # 250763402 Too Low
