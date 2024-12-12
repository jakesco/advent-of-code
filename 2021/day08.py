import argparse
import os

MAPPING = {"a": "X", "b": "X", "c": "X", "d": "X", "e": "X", "f": "X", "g": "X"}


def read_input(filepath: str) -> (int, int):
    with open(filepath, "r") as f:
        for line in f.readlines():
            segments, digits = line.split("|")
            s = ["".join(sorted(x)) for x in segments.split()]
            d = ["".join(sorted(x)) for x in digits.split()]
            yield s, d


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 8 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


def easy_digits(segment: str) -> str:
    match len(segment):
        case 2:
            return "1"
        case 3:
            return "7"
        case 4:
            return "4"
        case 7:
            return "8"
        case _:
            return "X"


def hard_digits(segment: str, one: set[str], four: set[str]) -> str:
    set_ = set(segment)

    one_inter = len(one.intersection(set_))
    four_inter = len(four.intersection(set_))
    seg_count = len(set_)

    # 2, 3, 5
    if seg_count == 5:
        if one_inter == 2:
            return "3"
        if four_inter == 2:
            return "2"
        if four_inter == 3:
            return "5"

    # 6, 9, 0
    if seg_count == 6:
        if one_inter == 1:
            return "6"
        if four_inter == 4:
            return "9"
        if four_inter == 3:
            return "0"

    return "X"


def decode_segments(segments: list[str]) -> dict[str, str]:
    key = dict()
    for seg in segments:
        key[seg] = "X"

    four = set()
    one = set()

    for segment, digit in key.items():
        d = easy_digits(segment)
        if d == "1":
            one = set(segment)
        if d == "4":
            four = set(segment)
        key[segment] = d

    for segment, digit in key.items():
        if digit in ("1", "4", "7", "8"):
            continue
        key[segment] = hard_digits(segment, one, four)

    return key


def decode_digits(digits: list[str], key: dict[str, str]) -> list[str]:
    return [key[d] for d in digits]


if __name__ == "__main__":
    path = init_parser()
    input_ = read_input(path)

    total = 0
    for segments, digits in input_:
        key = decode_segments(segments)
        number = "".join(decode_digits(digits, key))
        print(f"Decoded number: {number}")
        total += int(number)
    print(f"Total: {total}")


def main(_):
    raise NotImplementedError
