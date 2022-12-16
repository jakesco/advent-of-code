from __future__ import annotations

import os
import re
from collections import deque
from dataclasses import dataclass
from multiprocessing import Process, SimpleQueue

from .shared import P, Solution

# ROW = 10
# MAX_ROW = 21
ROW = 2_000_000
MAX_ROW = 4_000_001


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

    part1 = analyze_row(ROW, sensor_data)[0].length()

    # Part 2, distribute work among all cores
    cores = os.cpu_count()
    interval = MAX_ROW // cores
    ranges = [range(p * interval, p * interval + interval) for p in range(cores - 1)]
    ranges.append(range((cores - 1) * interval, MAX_ROW))

    q = SimpleQueue()
    workers = [
        Process(target=analyze_rows, args=(r, sensor_data.copy(), q)) for r in ranges
    ]

    for worker in workers:
        worker.start()

    result = q.get()

    for worker in workers:
        worker.terminate()

    part2 = tuning_frequency(result)
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


def analyze_row(row: int, sensor_data: list[SensorData]) -> list[HorizontalLine]:
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


def analyze_rows(rows: range, sensor_data: list[SensorData], q: SimpleQueue):
    for row in rows:
        lines = analyze_row(row, sensor_data)
        if len(lines) > 1:
            q.put(get_missing_point(lines, row))


def get_missing_point(lines: list[HorizontalLine], row: int) -> P:
    lines.sort()
    a, b = lines
    x = (b.x_min + a.x_max) // 2
    return P(x, row)


def tuning_frequency(p: P) -> int:
    return p.x * 4_000_000 + p.y
