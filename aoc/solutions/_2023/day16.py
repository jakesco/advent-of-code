from collections import defaultdict
from pprint import pprint
from dataclasses import dataclass, field
import enum

from aoc.utils.interfaces import Solution

# TODO: Probably don't need to simulate the beams

class D(enum.Enum):
    UP = enum.auto()
    DOWN = enum.auto()
    LEFT = enum.auto()
    RIGHT = enum.auto()


@dataclass(frozen=True)
class P:
    x: int
    y: int


@dataclass
class Beam:
    current: P = P(0, 0)
    direction: D = D.RIGHT
    path: list[P] = field(default_factory=list)
    stopped: bool = False

    def advance(self):
        self.path.append(self.current)
        x, y = self.current.x, self.current.y
        match self.direction:
            case D.UP:
                self.current = P(x, y - 1)
            case D.DOWN:
                self.current = P(x, y + 1)
            case D.LEFT:
                self.current = P(x - 1, y)
            case D.RIGHT:
                self.current = P(x + 1, y)

    def process(self, c: str):
        print(c)

@dataclass
class Grid:
    _grid: list[str]

    def get(self, point: P) -> str:
        return self._grid[point.y][point.x]

    def print(self) -> None:
        for line in self._grid:
            for c in line:
                print(c, end='')
            print()


def main(puzzle_input: list[str]) -> Solution:
    grid = Grid(puzzle_input)
    beams = simulate_beam(grid)
    pprint(beams)
    return Solution(
        sum(len(beam.path) for beam in beams),
    )


def simulate_beam(grid: Grid) -> list[Beam]:
    beams = [Beam()]
    while any(not beam.stopped for beam in beams):
        for beam in beams:
            print(beam)
            if beam.stopped:
                continue
            try:
                c = grid.get(beam.current)
                if new_beam := process(beam, c):
                    beams.append(new_beam)
                beam.advance()
            except IndexError:
                beam.stopped = True

    return beams

def process(beam: Beam, c: str) -> Beam | None:
    if c == ".":
        return None

    match (c, beam.direction):
        case ("|", D.LEFT):
            beam.direction = D.UP
            return Beam(beam.current, D.DOWN, beam.path.copy())
        case ("|", D.RIGHT):
            beam.direction = D.UP
            return Beam(beam.current, D.DOWN, beam.path.copy())
        case ("-", D.UP):
            beam.direction = D.LEFT
            return Beam(beam.current, D.RIGHT, beam.path.copy())
        case ("-", D.DOWN):
            beam.direction = D.LEFT
            return Beam(beam.current, D.RIGHT, beam.path.copy())
        case ("\\", D.UP):
            beam.direction = D.LEFT
        case ("\\", D.DOWN):
            beam.direction = D.RIGHT
        case ("\\", D.LEFT):
            beam.direction = D.UP
        case ("\\", D.RIGHT):
            beam.direction = D.DOWN
        case ("/", D.UP):
            beam.direction = D.RIGHT
        case ("/", D.DOWN):
            beam.direction = D.LEFT
        case ("/", D.LEFT):
            beam.direction = D.DOWN
        case ("/", D.RIGHT):
            beam.direction = D.UP
        case _:
            return None


if __name__ == "__main__":
    sample = r""".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."""
    test = main(sample.splitlines())
    print(test)
    assert test.part1 == 46
    assert test.part2 == 0
