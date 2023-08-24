import argparse
import os
from dataclasses import dataclass
from functools import reduce
from queue import Queue


class Map:
    def __init__(self, data: list[list[int]]):
        self.rows = len(data)
        self.cols = len(data[0])
        self.data = data

    def elm(self, row: int, col: int) -> int:
        try:
            if row < 0 or col < 0:
                raise IndexError
            return self.data[row][col]
        except IndexError:
            return 10

    def is_local_min(self, row: int, col: int) -> bool:
        val = self.elm(row, col)
        return (
            val < self.elm(row + 1, col)
            and val < self.elm(row - 1, col)
            and val < self.elm(row, col + 1)
            and val < self.elm(row, col - 1)
        )

    def get_local_mins(self) -> list[(int, int)]:
        local_mins = []
        for i in range(self.rows):
            for j in range(self.cols):
                if self.is_local_min(i, j):
                    local_mins.append((i, j))
        return local_mins

    def is_boundry(self, row: int, col: int):
        return self.elm(row, col) >= 9


@dataclass
class Node:
    x: int
    y: int
    val: int
    visited: bool = False

    def __hash__(self):
        return hash((self.x, self.y, self.val))


class Graph:
    def __init__(self, map_):
        self.nodes = dict()

        # Add node for each element in matrix
        for i in range(map_.rows):
            for j in range(map_.cols):
                n = Node(i, j, map_.elm(i, j))
                self.nodes[n] = set()

        # Connect nodes if they are adjacent and value < 9
        for node, neighbors in self.nodes.items():
            if node.val >= 9:
                continue
            candidates = (
                self.find_node(node.x + 1, node.y),
                self.find_node(node.x - 1, node.y),
                self.find_node(node.x, node.y + 1),
                self.find_node(node.x, node.y - 1),
            )
            for candidate in candidates:
                if candidate and candidate.val < 9:
                    neighbors.add(candidate)

    def find_node(self, x: int, y: int) -> Node | None:
        for n in self.nodes.keys():
            if n.x == x and n.y == y:
                return n
        return None

    def calculate_basin_size(self, start_node: Node) -> int:
        # Traverse the graph and count nodes connected nodes
        q = Queue()
        q.put_nowait(start_node)

        count = 0
        while not q.empty():

            n = q.get_nowait()
            if not n.visited:
                n.visited = True
                count += 1

            for neighbor in self.nodes[n]:
                if neighbor.visited:
                    continue
                q.put_nowait(neighbor)

        return count

    def __repr__(self):
        return str(self.nodes)


def calculate_risk(map_: Map) -> int:
    local_mins = [map_.elm(c[0], c[1]) for c in map_.get_local_mins()]
    return sum(local_mins) + len(local_mins)


def read_input(filepath: str):
    map_ = []
    with open(filepath, "r") as f:
        for line in f.readlines():
            map_.append([int(n) for n in line.rstrip()])
    return Map(map_)


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 9 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    map_ = read_input(path)

    print(f"Part 1: risk = {calculate_risk(map_)}")

    g = Graph(map_)
    local_mins = [g.find_node(i, j) for i, j in map_.get_local_mins()]

    basins = [g.calculate_basin_size(node) for node in local_mins]
    basins.sort(reverse=True)
    product = reduce(lambda a, b: a * b, basins[:3])
    print(f"Part 2: size = {product}")


def main(_):
    raise NotImplementedError
