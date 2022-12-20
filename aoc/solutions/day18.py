from .shared import Solution, P3

NEIGHBORS = {
    P3(1, 0, 0),
    P3(0, 1, 0),
    P3(0, 0, 1),
    P3(-1, 0, 0),
    P3(0, -1, 0),
    P3(0, 0, -1),
}

def main(input_: list[str]) -> Solution:
    points = {P3(*map(int, p.split(","))) for p in input_}

    part1 = sum([exposed_faces(p, points) for p in points])

    part2 = find_outside_points(points)
    part2 = len(part2)
    return Solution(part1, part2)


def exposed_faces(point: P3, points: set[P3]) -> int:
    neighbors = {point.add(n) for n in NEIGHBORS}
    overlap = neighbors.intersection(points)
    return 6 - len(overlap)

def find_outside_points(points: set[P3]) -> set[P3]:
    start = P3(0, 0, 0)
    q = [start]
    visited = set()
    outside_points = set()
    while q:
        c = q.pop(0)
        if c == end:
            break
        for adj in {c.add(n) for n in NEIGHBORS}:
    print(start, end)
    return set()


