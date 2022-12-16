import re
from pprint import pprint

from .shared import Solution

RE_VALVE = re.compile(
    r"Valve (?P<name>[A-Z]{2}).*rate=(?P<rate>\d+);.*valves? (?P<connections>.*)"
)

START = "AA"


def main(input_: list[str]) -> Solution:
    graph, rates = parse_valves(input_)
    pprint(graph)
    pprint(rates)
    return Solution()


def parse_valves(lines: list[str]) -> (dict[str, list[str]], dict[str, int]):
    graph = dict()
    rates = dict()
    for line in lines:
        valve = RE_VALVE.match(line)
        name = valve.group("name")
        rate = int(valve.group("rate"))
        connections = [v.strip() for v in valve.group("connections").split(",")]
        graph[name] = connections
        rates[name] = rate
    return graph, rates
