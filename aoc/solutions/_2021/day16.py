import argparse
import os
from collections import deque
from dataclasses import dataclass
from math import prod

HEX_TO_BIN = {
    "0": "0000",
    "1": "0001",
    "2": "0010",
    "3": "0011",
    "4": "0100",
    "5": "0101",
    "6": "0110",
    "7": "0111",
    "8": "1000",
    "9": "1001",
    "A": "1010",
    "B": "1011",
    "C": "1100",
    "D": "1101",
    "E": "1110",
    "F": "1111",
}


@dataclass
class Packet:
    version: int
    type: int

    def version_sum(self):
        return self.version


@dataclass
class Literal(Packet):
    value: int


@dataclass
class Operator(Packet):
    subops: list[Packet]

    def version_sum(self):
        return self.version + sum([o.version_sum() for o in self.subops])


class Stream:
    def __init__(self, hex_input: str):
        binary = "".join([HEX_TO_BIN[c] for c in hex_input])
        self.__data: deque[str] = deque([b for b in binary])

    @property
    def length(self):
        return len(self.__data)

    def pick(self, n: int = 1) -> str:
        """Read next n bits from stream."""
        acc = list()
        for i in range(n):
            acc.append(self.__data.popleft())
        return "".join(acc)

    def pick_int(self, n: int = 1) -> int:
        """Read next n bits from stream as int."""
        binary = self.pick(n)
        return int(binary, 2)


def process_operator(stream: Stream) -> Packet | None:
    version = stream.pick_int(3)
    type_ = stream.pick_int(3)

    # If literal
    if type_ == 4:
        val = list()
        while True:
            group = stream.pick(5)
            val.append(group[1:])
            if group.startswith("0"):
                break

        value = int("".join(val), 2)
        return Literal(version, type_, value)

    # otherwise operator
    else:
        length_type = stream.pick()
        sub_ops = list()
        if length_type == "0":
            total_length = stream.pick_int(15)
            while total_length > 0:
                startlen = stream.length
                sub_ops.append(process_operator(stream))
                endlen = stream.length
                total_length -= startlen - endlen
        else:
            num_subops = stream.pick_int(11)
            while num_subops > 0:
                sub_ops.append(process_operator(stream))
                num_subops -= 1

        return Operator(version, type_, sub_ops)


def eval_(op: Packet) -> int:
    if isinstance(op, Literal):
        return op.value

    elif isinstance(op, Operator):
        match op.type:
            case 0:
                return sum([eval_(o) for o in op.subops])
            case 1:
                return prod([eval_(o) for o in op.subops])
            case 2:
                return min([eval_(o) for o in op.subops])
            case 3:
                return max([eval_(o) for o in op.subops])
            case 5:
                return 1 if eval_(op.subops[0]) > eval_(op.subops[1]) else 0
            case 6:
                return 1 if eval_(op.subops[0]) < eval_(op.subops[1]) else 0
            case 7:
                return 1 if eval_(op.subops[0]) == eval_(op.subops[1]) else 0


def read_input(filepath: str) -> Stream:
    with open(filepath, "r") as f:
        hex_input = f.read().strip()
    return Stream(hex_input)


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 16 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    stream = read_input(path)

    packet = process_operator(stream)
    print(f"Part 1: {packet.version_sum()}")
    print(f"Part 2: {eval_(packet)}")


def main(_):
    raise NotImplementedError
