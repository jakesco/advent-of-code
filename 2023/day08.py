import re
from itertools import cycle
from math import lcm
from typing import TypeAlias

from aoc.utils.interfaces import Solution

Graph: TypeAlias = dict[str, tuple[str, str]]

NODE = re.compile(r"([A-Z0-9]+) = \(([A-Z0-9]+), ([A-Z0-9]+)\)")


def main(puzzle_input: list[str]) -> Solution:
    moves, graph = parse_input(puzzle_input)
    return Solution(
        count_steps("AAA", moves, graph),
        lcm(*(count_steps(node, moves, graph) for node in graph if node.endswith("A"))),
    )


def parse_input(puzzle_input: list[str]) -> tuple[str, Graph]:
    moves = puzzle_input[0].strip()
    graph = {}
    for line in puzzle_input[2:]:
        m = NODE.match(line)
        a, b, c = m.groups()
        graph[a] = (b, c)
    return moves, graph


def count_steps(start_node: str, moves: str, graph: Graph) -> int:
    curr = start_node
    steps = 0
    for move in cycle(moves):
        side = 0 if move == "L" else 1
        curr = graph[curr][side]
        steps += 1
        if curr.endswith("Z"):
            break
    return steps


if __name__ == "__main__":
    sample_1 = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""
    a = count_steps("AAA", *parse_input(sample_1.split("\n")))
    assert a == 2

    sample_2 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""
    moves, graph = parse_input(sample_2.split("\n"))
    b = lcm(*[count_steps(node, moves, graph) for node in graph if node.endswith("A")])
    assert b == 6

    print(Solution(a, b))
