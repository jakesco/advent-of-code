from collections import defaultdict
from dataclasses import dataclass

from aoc.utils.interfaces import Solution

hashmap = defaultdict(list)


@dataclass
class Lens:
    lab: str
    foc: int

    @property
    def hash(self) -> int:
        return hash_label(self.lab)

    def __repr__(self) -> str:
        return f"{self.lab} {self.foc}"


def main(puzzle_input: list[str]) -> Solution:
    steps = puzzle_input[0].split(",")
    for step in steps:
        if "=" in step:
            label, focus = step.split("=")
            h = hashmap[hash_label(label)]
            found = False
            for lens in h:
                if lens.lab == label:
                    lens.foc = int(focus)
                    found = True
                    break
            if not found:
                h.append(Lens(label, int(focus)))
        else:
            label = step[:-1]
            h = hashmap[hash_label(label)]
            for lens in h:
                if lens.lab == label:
                    h.remove(lens)
                    break

    return Solution(
        sum(hash_label(x) for x in puzzle_input[0].split(",")),
        sum(
            sum((box_id + 1) * idx * lens.foc for idx, lens in enumerate(box, start=1))
            for box_id, box in hashmap.items()
        ),
    )


def hash_label(string: str) -> int:
    hash_ = 0
    for c in string:
        hash_ += ord(c)
        hash_ *= 17
        hash_ %= 256
    return hash_


if __name__ == "__main__":
    assert hash_label("HASH") == 52
    sample = """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""
    test = main(sample.splitlines())
    print(test)
    assert test.part1 == 1320
    assert test.part2 == 145
