from functools import reduce

from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    histories = (list(map(int, line.split())) for line in puzzle_input)
    predictions = map(predict, histories)
    return Solution(*map(sum, zip(*predictions)))


def predict(history: list[int]) -> tuple[int, int]:
    """Given a history, return (next, prev) predictions."""
    next_data = [history[-1]]
    prev_data = [history[0]]
    while not all(h == 0 for h in history):
        history = diff_history(history)
        next_data.append(history[-1])
        prev_data.append(history[0])
    return sum(next_data), reduce(lambda x, y: y - x, reversed(prev_data))


def diff_history(history: list[int]) -> list[int]:
    new_hist = []
    curr = history[0]
    for h in history[1:]:
        new_hist.append(h - curr)
        curr = h
    return new_hist


if __name__ == "__main__":
    sample_1 = """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""
    s = main(sample_1.splitlines())
    print(s)
    assert s.part1 == 114
    assert s.part2 == 2
