from __future__ import annotations

from copy import deepcopy
import math
import random
import re
from dataclasses import dataclass
from functools import partial
from multiprocessing import Pool, SimpleQueue

from typing import Iterator
from .shared import Solution

from itertools import permutations


RE_VALVE = re.compile(
    r"Valve (?P<name>[A-Z]{2}).*rate=(?P<rate>\d+);.*valves? (?P<connections>.*)"
)

START = "AA"
STEPS = 30
CHUNK = 100_000


@dataclass(frozen=True, eq=True)
class Node:
    name: str
    rate: int


@dataclass
class Graph:
    nodes: dict[str, Node]
    edges: dict[str, set[Node]]
    _adj_matrix: dict[str, dict[str, int]] = None

    @classmethod
    def from_input(cls, lines: list[str]) -> Graph:
        nodes = dict()
        connections = dict()
        for line in lines:
            valve = RE_VALVE.match(line)
            name = valve.group("name")
            rate = int(valve.group("rate"))
            connection = [v.strip() for v in valve.group("connections").split(",")]
            nodes[name] = Node(name, rate)
            connections[name] = connection
        edges = dict()
        for name, conn in connections.items():
            edges[name] = {nodes[n] for n in conn}
        graph = cls(nodes, edges)
        graph._build_adj_matrix()
        return graph

    def _build_adj_matrix(self):
        self._adj_matrix = dict()
        for node in self.nodes.keys():
            self._adj_matrix[node] = self._shortest_path(node)

    def _shortest_path(self, node: str):
        distances = {node.name: math.inf for node in self.nodes.values()}
        distances[node] = 0

        q = list(distances.keys())
        visited = set()
        while q:
            q.sort(key=lambda node: distances[node], reverse=True)
            current = q.pop()
            for adj in self.edges[current]:
                if adj.name in visited:
                    continue
                new_dist = distances[current] + 1
                if new_dist < distances[adj.name]:
                    distances[adj.name] = new_dist
                visited.add(adj.name)
        return distances

    def distance(self, n1: str, n2: str) -> int:
        return self._adj_matrix[n1][n2]

    def rate(self, node: str) -> int:
        return self.nodes[node].rate

    def important_nodes(self) -> list[str]:
        return [node.name for node in self.nodes.values() if node.rate > 0]


def main(input_: list[str]) -> Solution:
    graph = Graph.from_input(input_)
    part1 = part2 = 0
    eval_ = partial(eval_scenario, graph=graph)
    with Pool() as p:
        results = p.imap_unordered(eval_, permutations(graph.important_nodes(), r=9), chunksize=CHUNK)
        best = max([result for result in results])
    print(best)

    return Solution(part1, part2)


def eval_scenario(scenario: str, *, graph: Graph, max_steps: int = STEPS) -> int:
    graph = deepcopy(graph)
    pressure_released = 0
    steps = 0
    current = START
    open_valves = []
    checked = 0
    for node in scenario:
        dist = graph.distance(current, node) + 1
        steps += dist
        if steps >= max_steps:
            steps -= dist
            break
        checked += 1
        pressure_released += sum(open_valves) * dist
        current = node
        open_valves.append(graph.rate(current))

    if steps < max_steps:
        pressure_released += sum(open_valves) * (max_steps - steps)

    return pressure_released
