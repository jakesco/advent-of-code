from dataclasses import dataclass

from aoc.utils.interfaces import Solution


@dataclass
class Move:
    direction: str
    distance: int

    @staticmethod
    def from_input(input: str):
        dir_, dist = input.split(" ")
        return Move(dir_, int(dist))


@dataclass
class Position:
    distance: int = 0
    depth: int = 0
    aim: int = 0

    def product(self):
        return self.distance * self.depth

    def move(self, movement: Move):
        match movement:
            case Move("forward", distance):
                self.distance += distance
                self.depth += self.aim * distance
            case Move("up", distance):
                self.aim -= distance
            case Move("down", distance):
                self.aim += distance


def main(input_: list[str]) -> Solution:
    movements = (Move.from_input(x) for x in input_)

    position = Position()
    for m in movements:
        position.move(m)

    print(f"Position: {position.distance}")
    print(f"Depth: {position.depth}")
    print(f"Answer: {position.product()}")

    return Solution()
