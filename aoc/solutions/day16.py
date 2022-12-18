from __future__ import annotations

import math
import random
import re
from dataclasses import dataclass
from functools import partial

from .shared import Solution

# from itertools import permutations


RE_VALVE = re.compile(
    r"Valve (?P<name>[A-Z]{2}).*rate=(?P<rate>\d+);.*valves? (?P<connections>.*)"
)

START = "AA"
STEPS = 30
ITERATIONS = 100_000
# ITERATIONS = 10

# 2442 too low


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
        nodes = [node.name for node in self.nodes.values() if node.rate > 0]
        random.shuffle(nodes)
        return nodes


def main(input_: list[str]) -> Solution:
    graph = Graph.from_input(input_)
    part1 = simulated_annealing(graph)
    part2 = simulated_annealing_part2(graph)
    return Solution(part1, part2)


def simulated_annealing(graph: Graph) -> int:
    calc_score = partial(eval_scenario, graph=graph)
    scenario = graph.important_nodes()
    current = calc_score(scenario)
    for iter in range(ITERATIONS):
        new_scenario = neighbor(scenario)
        new_score = calc_score(new_scenario)
        if accept(current, new_score, iter):
            scenario = new_scenario
            current = new_score
    return current


def simulated_annealing_part2(graph: Graph) -> int:
    calc_score = partial(eval_scenario, graph=graph, max_steps=STEPS - 4)
    scenario = graph.important_nodes()
    half = len(scenario) // 2
    current = calc_score(scenario[:half]) + calc_score(scenario[half:])
    for iter in range(ITERATIONS):
        new_scenario = neighbor(scenario)
        half = len(new_scenario) // 2
        new_score = calc_score(new_scenario[:half]) + calc_score(new_scenario[half:])
        if accept(current, new_score, iter):
            scenario = new_scenario
            current = new_score
    return current


def neighbor(scenario: list[str]) -> list[str]:
    sample = random.sample(scenario, 2)
    i, j = scenario.index(sample[0]), scenario.index(sample[1])
    scenario[i], scenario[j] = scenario[j], scenario[i]
    return scenario


def accept(old_score: int, new_score: int, iteration) -> bool:
    if old_score < new_score:
        p = 1
    else:
        t = temperature(iteration) + 0.0001
        p = math.exp(-(old_score - new_score) / t)
    return p >= random.random()


def temperature(iteration: int) -> float:
    return 1 - (iteration + 1) / ITERATIONS


def eval_scenario(scenario: list[str], *, graph: Graph, max_steps: int = STEPS) -> int:
    pressure_released = 0
    steps = 0
    current = START
    open_valves = []
    for node in scenario:
        dist = graph.distance(current, node) + 1
        steps += dist
        if steps >= max_steps:
            steps -= dist
            break
        pressure_released += sum(open_valves) * dist
        current = node
        open_valves.append(graph.rate(current))

    if steps < max_steps:
        pressure_released += sum(open_valves) * (max_steps - steps)

    return pressure_released
