from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    cards = {}
    for line in puzzle_input:
        a, b, c = line.replace(":", "|").split("|")
        card = int(a.split()[1])
        winners = set(b.split())
        have = set(c.split())
        cards[card] = len(have.intersection(winners))

    counts = dict.fromkeys(cards, 1)
    for k, v in cards.items():
        for i in range(v):
            counts[k + i + 1] += counts[k]

    return Solution(
        sum(2 ** (wins - 1) for wins in cards.values() if wins > 0),
        sum(count for count in counts.values()),
    )


if __name__ == "__main__":
    sample = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
    s = main(sample.split("\n"))
    print(s)
    assert s.part1 == 13
    assert s.part2 == 30
