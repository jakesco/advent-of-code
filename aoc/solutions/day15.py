from __future__ import annotations

import re
from collections import deque
from dataclasses import dataclass

from .shared import P, Solution

# ROW = 10
# MAX_ROW = 20
ROW = 2_000_000
MAX_ROW = 4_000_000


@dataclass(frozen=True, eq=True, order=True)
class HorizontalLine:
    x_min: int
    x_max: int

    def overlaps(self, other: HorizontalLine) -> bool:
        return not (self.x_min > other.x_max + 1 or self.x_max + 1 < other.x_min)

    def merge(self, other: HorizontalLine) -> HorizontalLine:
        new_min = min(self.x_min, other.x_min)
        new_max = max(self.x_max, other.x_max)
        return HorizontalLine(new_min, new_max)

    def length(self) -> int:
        return self.x_max - self.x_min


@dataclass(frozen=True)
class SensorData:
    sensor: P
    beacon: P
    distance: int

    @property
    def rows(self) -> tuple[int, int]:
        return self.sensor.y, self.beacon.y

    def exclude(self, row: int) -> int:
        return sum([1 for r in self.rows if r == row])

    def lines(self, row: int) -> list[HorizontalLine]:
        return [HorizontalLine(r, r) for r in self.rows if r == row]


def main(input_: list[str]) -> Solution:
    sensor_data = parse_sensor_data(input_)

    part1 = analyze_row(sensor_data, ROW)[0].length()
    part2 = tuning_frequency(analyze_rows(sensor_data))
    return Solution(part1, part2)


def parse_sensor_data(sensor_locations: list[str]) -> list[SensorData]:
    sensor_pairs: list[SensorData] = []
    for location in sensor_locations:
        a, b, c, d = map(int, re.findall(r"-?\d+", location))
        sensor = P(a, b)
        beacon = P(c, d)
        distance = sensor.m_dist(beacon)
        sensor_pairs.append(SensorData(sensor, beacon, distance))
    return sensor_pairs


def analyze_rows(sensor_data: list[SensorData]) -> P:
    x, y = 0, 0
    lines = []
    for row in range(MAX_ROW + 1):
        y = row
        lines = analyze_row(sensor_data, row)
        if len(lines) > 1:
            break

    if len(lines) < 2:
        return P(x, y)

    lines.sort()
    a, b = lines
    x = (b.x_min + a.x_max) // 2
    return P(x, y)


def analyze_row(sensor_data: list[SensorData], row: int) -> list[HorizontalLine]:
    scanner_lines = []
    for pair in sensor_data:
        scanner_lines.extend(pair.lines(row))
        dist = pair.distance
        center = P(pair.sensor.x, row)
        if (row_dist := center.m_dist(pair.sensor)) <= dist:
            r = (1 + (dist - row_dist) * 2) // 2
            left = center.x - r
            right = center.x + r
            scanner_lines.append(HorizontalLine(left, right))
    scanner_lines = dedup_intervals(scanner_lines)
    return scanner_lines


def dedup_intervals(lines: list[HorizontalLine]) -> list[HorizontalLine]:
    lines.sort()
    stack = deque([lines.pop(0)])
    for line in lines:
        top = stack[0]
        if not line.overlaps(top):
            stack.appendleft(line)
        else:
            stack[0] = top.merge(line)
    return list(stack)


def tuning_frequency(p: P) -> int:
    return p.x * 4_000_000 + p.y
