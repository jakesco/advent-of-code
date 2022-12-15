from __future__ import annotations

import re
from collections import deque
from dataclasses import dataclass

from .shared import P, Solution

ROW = 10
MAX_ROW = 20
# ROW = 2_000_000
# MAX_ROW = 4_000_000


@dataclass(frozen=True, eq=True, order=True)
class HorizontalLine:
    x_min: int
    x_max: int
    row: int = ROW

    def overlaps(self, other: HorizontalLine) -> bool:
        return not (self.x_min > other.x_max + 1 or self.x_max + 1 < other.x_min)

    def merge(self, other: HorizontalLine) -> HorizontalLine:
        new_min = min(self.x_min, other.x_min)
        new_max = max(self.x_max, other.x_max)
        return HorizontalLine(new_min, new_max, self.row)

    def length(self) -> int:
        return self.x_max - self.x_min


@dataclass(frozen=True)
class SensorData:
    sensor: P
    beacon: P
    distance: int

    def exclude(self, row: int) -> int:
        n = 0
        if self.sensor.y == row:
            n += 1
        if self.beacon.y == row:
            n += 1
        return n

    def lines(self, row: int) -> list[HorizontalLine]:
        l = []
        if self.sensor.y == row:
            l.append(HorizontalLine(self.sensor.x, self.sensor.x, row))
        if self.beacon.y == row:
            l.append(HorizontalLine(self.beacon.x, self.beacon.x, row))
        return l


def main(input_: list[str]) -> Solution:
    sensor_data = parse_sensor_data(input_)
    for row in range(MAX_ROW + 1):
        scanned: list[HorizontalLine] = []
        for pair in sensor_data:
            scanned.extend(pair.lines(row))
            dist = pair.distance
            center = P(pair.sensor.x, row)
            if (row_dist := center.m_dist(pair.sensor)) <= dist:
                r = (1 + (dist - row_dist) * 2) // 2
                left = center.x - r
                right = center.x + r
                scanned.append(HorizontalLine(left, right, row))

        scanned = dedup_intervals(scanned)
        for s in scanned:
            print(s)
        if row == ROW:
            part1 = sum([s.length() for s in scanned])

    part2 = tuning_frequency(P(14, 11))
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
