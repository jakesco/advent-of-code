from functools import cmp_to_key
from itertools import zip_longest

from .shared import Solution, batched


def main(input_: list[str]) -> Solution:
    packets = [eval(line) for line in input_ if line]

    part1 = sum(
        [
            idx
            for idx, [left, right] in enumerate(batched(packets, 2), start=1)
            if compare(left, right) < 0
        ]
    )

    packets.extend([[[2]], [[6]]])
    packets = sorted(packets, key=cmp_to_key(compare))
    part2 = (packets.index([[2]]) + 1) * (packets.index([[6]]) + 1)

    return Solution(part1, part2)


def compare(x, y) -> int:
    for left, right in zip_longest(x, y):
        # print("Compare:", left, right)

        match (left is None, right is None):
            case (True, True):
                return 0
            case (True, False):
                # print("Left ran out")
                return -1
            case (False, True):
                # print("Right ran out")
                return 1

        match (isinstance(left, list), isinstance(right, list)):
            case (True, True):
                if (result := compare(left, right)) != 0:
                    return result
                continue
            case (True, False):
                if (result := compare(left, [right])) != 0:
                    return result
                continue
            case (False, True):
                if (result := compare([left], right)) != 0:
                    return result
                continue

        if left < right:
            # print("Left side smaller")
            return -1
        if left > right:
            # print("Right side smaller")
            return 1

    return 0
