from collections import Counter, deque
from collections.abc import Iterator
from dataclasses import dataclass

from .shared import Grid, P, Solution

DIRECTIONS = deque(
    [
        (P(0, -1), P(1, -1), P(-1, -1)),  # N, NW, NE
        (P(0, 1), P(-1, 1), P(1, 1)),  # S, SW, SE
        (P(-1, 0), P(-1, 1), P(-1, -1)),  # W, NW, SW
        (P(1, 0), P(1, 1), P(1, -1)),  # E, NE, SE
    ],
    maxlen=4,
)


@dataclass(frozen=True, slots=True)
class Move:
    elf: P
    dest: P


def main(input_: list[str]) -> Solution:
    grid = Grid.from_lines(input_, convert=str, skip=".")
    grid.render_graph(range(-5, 10), range(-5, 10))

    for _ in range(10):
        run_round(grid)
        grid.render_graph(range(-5, 10), range(-5, 10))

    print(score(grid))
    return Solution()


def run_round(grid: Grid):
    moves = [propose_move(elf, grid) for elf in grid]
    duplicates = {p for p, c in Counter([move.dest for move in moves]).items() if c > 1}
    for move in moves:
        if move.dest in duplicates:
            continue
        execute_move(move, grid)
    DIRECTIONS.rotate(-1)


def propose_move(elf: P, grid: Grid) -> Move:
    for n in neighbors(elf):
        if any([p in grid for p in n]):
            continue
        return Move(elf, n[0])
    return Move(elf, elf)


def neighbors(point: P) -> Iterator[tuple[P, P, P]]:
    for direction in DIRECTIONS:
        yield tuple(map(lambda x: x + point, direction))


def execute_move(move: Move, grid: Grid):
    del grid[move.elf]
    grid[move.dest] = "#"


def score(grid: Grid) -> int:
    min_x = min(grid, key=lambda p: p.x).x
    max_x = max(grid, key=lambda p: p.x).x
    min_y = min(grid, key=lambda p: p.y).y
    max_y = max(grid, key=lambda p: p.y).y
    size = (max_x - min_x) * (max_y - min_y)
    return size - len(grid)
