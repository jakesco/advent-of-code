from __future__ import annotations
import re
import math
from pprint import pprint
from dataclasses import dataclass

from functools import cached_property

from .shared import Solution

RE_VALVE = re.compile(
    r"Valve (?P<name>[A-Z]{2}).*rate=(?P<rate>\d+);.*valves? (?P<connections>.*)"
)

START = "AA"
STEPS = 30


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

    def important_nodes(self) -> list[Node]:
        return [node for node in self.nodes.values() if node.rate > 0]



def main(input_: list[str]) -> Solution:
    graph = Graph.from_input(input_)
    scenario = ["DD", "BB", "JJ", "HH", "EE", "CC"]
    sol = eval_scenario(graph, scenario)
    print(sol)

    return Solution()


def eval_scenario(graph: Graph, scenario: list[str]) -> int:
    pressure_released = 0
    steps = 0
    current = START
    open_valves = []
    while steps <= STEPS and scenario:
        node = scenario.pop(0)
        print(current, steps, pressure_released, open_valves)
        dist = graph.distance(current, node) + 1
        pressure_released += sum(open_valves) * dist
        steps += dist
        current = node
        open_valves.append(graph.rate(current))
    pressure_released += sum(open_valves) * (STEPS - steps)
    return pressure_released








