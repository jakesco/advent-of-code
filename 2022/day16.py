from __future__ import annotations

import math
import re
from collections import deque
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
    edges: dict[str, list[Node]]
    adj_matrix: dict[Node, dict[Node, int]] = None

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
            edges[name] = [nodes[n] for n in conn]
        graph = cls(nodes, edges)
        graph._build_adj_matrix()
        return graph

    def _build_adj_matrix(self):
        self.adj_matrix = dict()
        for node in self.nodes.values():
            self.adj_matrix[node] = self._shortest_path(node)

    def _shortest_path(self, node: Node):
        distances = {node: math.inf for node in self.nodes.values()}
        distances[node] = 0

        q = list(distances.keys())
        visited = set()
        while q:
            q.sort(key=lambda node: distances[node], reverse=True)
            current = q.pop()
            for adj in self.edges[current.name]:
                if adj in visited:
                    continue
                new_dist = distances[current] + 1
                if new_dist < distances[adj]:
                    distances[adj] = new_dist
                visited.add(adj)
        return distances

    def distance(self, n1: Node, n2: Node) -> int:
        return self.adj_matrix[n1][n2]

    def rate(self, node: str) -> int:
        return self.nodes[node].rate

    @cached_property
    def important_nodes(self) -> set[Node]:
        return {node for node in self.nodes.values() if node.rate > 0}


@dataclass
class Scenario:
    current: Node | None
    open_valves: set[Node]
    steps_left: int
    steam_released: int = 0


def main(input_: list[str]) -> Solution:
    graph = Graph.from_input(input_)
    part1 = max([s.steam_released for s in enumerate_scenarios(graph)])
    part2 = best_disjoint_scenarios(enumerate_scenarios(graph, STEPS - 4))
    return Solution(part1, part2)


def enumerate_scenarios(graph: Graph, start_steps: int = STEPS) -> list[Scenario]:
    initial = Scenario(graph.nodes[START], set(), start_steps)
    q = deque([initial])
    scenarios = list()
    while q:
        s = q.popleft()

        adjacent = adjacent_nodes(graph, s)
        if len(adjacent) == 0:
            steam = s.steam_released + sum(
                [node.rate * s.steps_left for node in s.open_valves]
            )
            scenarios.append(Scenario(None, s.open_valves, 0, steam))
            continue

        for adj, dist in adjacent:
            steps = s.steps_left - dist
            steam = s.steam_released + sum([node.rate * dist for node in s.open_valves])
            open_valves = {adj, *s.open_valves}
            new_scenario = Scenario(adj, open_valves, steps, steam)
            q.append(new_scenario)
    return scenarios


def adjacent_nodes(graph: Graph, scenario: Scenario) -> set[(Node, int)]:
    adjacent = set()
    for node in graph.important_nodes:
        if node in scenario.open_valves:
            continue
        distance = graph.distance(scenario.current, node) + 1
        if scenario.steps_left < distance:
            continue
        adjacent.add((node, distance))
    return adjacent


def best_disjoint_scenarios(scenarios: list[Scenario]) -> int:
    scenarios = sorted(scenarios, key=lambda x: x.steam_released, reverse=True)
    best = scenarios[0]
    for s in scenarios[1:]:
        if best.open_valves.isdisjoint(s.open_valves):
            return best.steam_released + s.steam_released
    return 0
