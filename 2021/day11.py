import argparse
import os
import uuid
from collections import deque
from dataclasses import dataclass, field


@dataclass
class Octopus:
    energy: int
    flashed: bool = False
    __id: str = field(default_factory=uuid.uuid4)

    def __hash__(self):
        return hash(self.__id)

    def charge(self) -> int:
        self.energy += 1
        return self.energy

    def flash(self):
        self.energy = 0
        self.flashed = True

        global flashes
        flashes += 1


class Graph:
    def __init__(self):
        self.nodes: dict[Octopus, set[Octopus]] = dict()
        self.to_flash: deque[Octopus] = deque()

    def add_node(self, node: Octopus, neighbors: set[Octopus]):
        self.nodes[node] = neighbors

    def _charge_all(self):
        # Increment all energy levels
        for o in self.nodes.keys():
            charge = o.charge()
            if charge > 9:
                self.to_flash.append(o)

    def _flash(self):
        # Flash all nodes with energy > 9
        while self.to_flash:
            o = self.to_flash.popleft()
            if o.flashed:
                continue
            o.flash()
            for n in self.nodes[o]:
                if n.flashed:
                    continue
                charge = n.charge()
                if charge > 9:
                    self.to_flash.append(n)

    def _cleanup(self):
        # reset to_flash queue and flash state
        self.to_flash.clear()
        for o in self.nodes.keys():
            o.flashed = False

    def _check_sync(self):
        return all([o.flashed for o in self.nodes.keys()])

    def step(self) -> bool:
        self._charge_all()
        self._flash()
        if self._check_sync():
            return True
        self._cleanup()
        return False

    def __str__(self):
        length = 10
        output = [str(o.energy) for o in self.nodes.keys()]
        for i in range(length, len(output) + 1, length + 1):
            output.insert(i, "\n")
        return "".join(output)


def find_neighbors(center: (int, int), grid: list[list[Octopus]]) -> set[Octopus]:
    neighbors = set()
    max_row = len(grid) - 1
    max_col = len(grid[0]) - 1
    for i in range(-1, 2):
        for j in range(-1, 2):
            # Constrain row and column within grid
            row = min(max(center[0] + i, 0), max_row)
            col = min(max(center[1] + j, 0), max_col)
            if (row, col) == center:
                continue
            o = grid[row][col]
            neighbors.add(o)
    return neighbors


def read_input(filepath: str) -> Graph:
    grid = list()
    with open(filepath, "r") as f:
        for line in f.readlines():
            grid.append([Octopus(int(n)) for n in line.rstrip()])

    # Add Octopi + Neighbors to Graph
    graph = Graph()
    rows = len(grid)
    cols = len(grid[0])
    for i in range(rows):
        for j in range(cols):
            o = grid[i][j]
            neighbors = find_neighbors((i, j), grid)
            graph.add_node(o, neighbors)

    return graph


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 11 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    flashes = 0
    steps = 1000

    path = init_parser()
    graph = read_input(path)

    for i in range(steps):
        synced = graph.step()
        if synced:
            print(f"Part 2: Synced at step {i + 1}!")
            exit()

    print(f"Part 1 flashes: {flashes}")


def main(_):
    raise NotImplementedError
