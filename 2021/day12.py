import argparse
import os
from collections import Counter, deque
from dataclasses import dataclass


@dataclass(frozen=True)
class Node:
    name: str
    big: bool

    def __repr__(self):
        return self.name


@dataclass
class Path:
    path: list[Node]
    visited: Counter[Node]
    max_visits: int

    @property
    def length(self):
        return len(self.path)

    def can_visit(self, node: Node) -> bool:
        allow_more_visits = all(
            [c < self.max_visits for n, c in self.visited.items() if not n.big]
        )
        return allow_more_visits or node.big or node not in self.visited

    def current_node(self) -> Node | None:
        if self.path:
            return self.path[-1]
        return None

    def add_to_path(self, node: Node) -> bool:
        if self.can_visit(node) and node != self.current_node():
            self.path.append(node)
            self.visited[node] += 1
            return True
        return False


class Graph:
    def __init__(self):
        self.__lookup: dict[str, Node] = dict()
        self.__nodes: dict[Node, set[Node]] = dict()

    def __str__(self):
        output = []
        for k, v in self.__nodes.items():
            output.append(f"{k} <-> {[n for n in v]}")
        return "\n".join(output)

    def __new_node(self, name) -> Node:
        node = Node(name, name.isupper())
        self.__lookup[name] = node
        self.__nodes[node] = set()
        return node

    def add_node(self, name: str) -> Node:
        if node := self.__lookup.get(name, None):
            return node
        return self.__new_node(name)

    def add_connection(self, name1: str, name2: str):
        node1 = self.__lookup.get(name1, None)
        if not node1:
            node1 = self.add_node(name1)

        node2 = self.__lookup.get(name2, None)
        if not node2:
            node2 = self.add_node(name2)

        self.__nodes[node1].add(node2)
        self.__nodes[node2].add(node1)

    def find_paths(self, max_visits: int = 1) -> list[Path]:
        start = self.__lookup["start"]
        end = self.__lookup["end"]
        seed = Path([start], Counter([start]), max_visits)

        active = deque([seed])

        paths = list()
        while active:
            path = active.popleft()
            next_ = self.__nodes[path.current_node()]

            for n in next_:
                p = Path(path.path.copy(), path.visited.copy(), max_visits)
                if n == start or not p.add_to_path(n):
                    continue
                if end in p.visited:
                    paths.append(p)
                else:
                    active.append(p)

        return paths


def read_input(filepath: str) -> Graph:
    graph = Graph()
    with open(filepath, "r") as f:
        for line in f.readlines():
            nodes = line.rstrip().split("-")
            graph.add_connection(nodes[0], nodes[1])
    return graph


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 12 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    graph = read_input(path)
    paths_part1 = graph.find_paths(1)
    print(f"Part 1: {len(paths_part1)}(10) distinct paths")
    paths_part2 = graph.find_paths(2)
    print(f"Part 2: {len(paths_part2)}(36) distinct paths")


def main(_):
    raise NotImplementedError
