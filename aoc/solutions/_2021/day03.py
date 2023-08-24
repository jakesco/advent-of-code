from .shared import Solution


def frequency(input_: list[str]) -> list[int]:
    wordsize = len(input_[0])
    freq = [0] * wordsize

    for measure in input_:
        for i in range(wordsize):
            freq[i] += int(measure[i])

    return freq


def part1(input_) -> tuple[int, int]:
    freq = frequency(input_)

    threshold = len(input_) / 2

    gamma = ["1" if x >= threshold else "0" for x in freq]
    epsilon = ["0" if x >= threshold else "1" for x in freq]

    return (int("".join(gamma), 2), int("".join(epsilon), 2))


def filter(input_: list[str], index: int, most: bool) -> list[str]:
    freq = 0
    for m in input_:
        freq += int(m[index])
    if most:
        filter_ = "1" if freq >= len(input_) / 2 else "0"
    else:
        filter_ = "0" if freq >= len(input_) / 2 else "1"
    return [x for x in input_ if x[index] == filter_]


def part2(input_) -> tuple[int, int]:
    wordsize = len(input_[0])

    o2 = input_.copy()
    for i in range(wordsize):
        o2 = filter(o2, i, True)
        if len(o2) == 1:
            break

    co2 = input_.copy()
    for i in range(wordsize):
        co2 = filter(co2, i, False)
        if len(co2) == 1:
            break

    return (int(o2[0], 2), int(co2[0], 2))


def main(input_: list[str]):
    rates = part1(input_)
    lf = part2(input_)
    return Solution(rates[0] * rates[1], lf[0] * lf[1])
