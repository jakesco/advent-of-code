import enum
import re
from dataclasses import dataclass

from aoc.utils.interfaces import Solution

ASSIGN_GATE = re.compile(r"^(?P<in>\w+) -> (?P<out>\w+)$")
NOT_GATE = re.compile(r"^NOT (?P<in>\w+) -> (?P<out>\w+)$")
AND_GATE = re.compile(r"^(?P<in1>\w+) AND (?P<in2>\w+) -> (?P<out>\w+)$")
OR_GATE = re.compile(r"^(?P<in1>\w+) OR (?P<in2>\w+) -> (?P<out>\w+)$")
LSHIFT_GATE = re.compile(r"^(?P<in1>\w+) LSHIFT (?P<in2>\d+) -> (?P<out>\w+)$")
RSHIFT_GATE = re.compile(r"^(?P<in1>\w+) RSHIFT (?P<in2>\d+) -> (?P<out>\w+)$")


class GateType(enum.Enum):
    AND = enum.auto()
    OR = enum.auto()
    LSHIFT = enum.auto()
    RSHIFT = enum.auto()
    NOT = enum.auto()
    ASSIGN = enum.auto()


@dataclass
class Connection:
    gate_type: GateType
    in1: str | int
    in2: str | int
    out: str


def main(puzzle_input: list[str]) -> Solution:
    connections = [parse_connection(line) for line in puzzle_input]
    result = run_simulation(connections)
    part1 = "a"
    while isinstance(part1, str):
        part1 = result[part1]

    return Solution(part1)


def run_simulation(connections: list[Connection]) -> dict[str, int]:
    results: dict[str, int] = {"_": 0}

    while connections:
        conn = connections.pop(0)
        print(f"Processing {conn}")
        print(f"State {results}", len(connections))

        if conn.gate_type == GateType.ASSIGN:
            results[conn.out] = conn.in1
            continue

        if isinstance(conn.in1, str) and results.get(conn.in1) is None:
            connections.append(conn)
            continue

        if isinstance(conn.in2, str) and results.get(conn.in2) is None:
            connections.append(conn)
            continue

        operand1 = conn.in1 if isinstance(conn.in1, int) else results[conn.in1]
        operand2 = conn.in2 if isinstance(conn.in2, int) else results[conn.in2]

        match conn.gate_type:
            case GateType.NOT:
                results[conn.out] = ~operand1 & 0xFFFF  # convert to unsigned 16 bit
            case GateType.AND:
                results[conn.out] = operand1 & operand2
            case GateType.OR:
                results[conn.out] = operand1 | operand2
            case GateType.LSHIFT:
                results[conn.out] = operand1 << operand2
            case GateType.RSHIFT:
                results[conn.out] = operand1 >> operand2

    return results


def parse_connection(string: str) -> Connection:
    if match := ASSIGN_GATE.match(string):
        return Connection(
            GateType.ASSIGN, try_int(match.group("in")), "_", match.group("out")
        )

    if match := NOT_GATE.match(string):
        return Connection(
            GateType.NOT, try_int(match.group("in")), "_", match.group("out")
        )

    if match := AND_GATE.match(string):
        return Connection(
            GateType.AND,
            try_int(match.group("in1")),
            try_int(match.group("in2")),
            match.group("out"),
        )

    if match := OR_GATE.match(string):
        return Connection(
            GateType.OR,
            try_int(match.group("in1")),
            try_int(match.group("in2")),
            match.group("out"),
        )

    if match := LSHIFT_GATE.match(string):
        return Connection(
            GateType.LSHIFT,
            try_int(match.group("in1")),
            try_int(match.group("in2")),
            match.group("out"),
        )

    if match := RSHIFT_GATE.match(string):
        return Connection(
            GateType.RSHIFT,
            try_int(match.group("in1")),
            try_int(match.group("in2")),
            match.group("out"),
        )

    raise ValueError(f"Unknown connection type {string}")


def try_int(string: str) -> int | str:
    try:
        return int(string)
    except ValueError:
        return string
