from collections import deque

from .shared import Solution


def main(input_: list[str]) -> Solution:
    message = input_[0]
    part1 = check_window(message, 4)
    part2 = check_window(message, 14)
    return Solution(part1, part2)


def check_window(message: str, maxlen: int) -> int:
    window = deque(maxlen=maxlen)
    for idx, char in enumerate(message, start=1):
        window.append(char)
        if all_unique(window):
            return idx


def all_unique(window: deque[str]) -> bool:
    if len(window) < window.maxlen:
        return False
    return len(window) == len(set(window))
