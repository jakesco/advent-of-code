import re
from collections import defaultdict
from dataclasses import dataclass
from pprint import pprint

from aoc.utils.interfaces import Solution

RE_CONNECTION = re.compile(r"(.*) to (.*) = (\d+)")


@dataclass
class Connection:
    node: str
    dist: int


def main(puzzle_input: list[str]) -> Solution:
    graph: dict[str, list[Connection]] = defaultdict(list)
    for conn in puzzle_input:
        m = RE_CONNECTION.match(conn)
        a, b, dist = m.group(1), m.group(2), int(m.group(3))
        graph[a].append(Connection(b, dist))
        graph[b].append(Connection(a, dist))
    pprint(graph)
    return Solution()
