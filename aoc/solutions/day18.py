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

    all_faces = [exposed_faces(p, points) for p in points]
    part1 = sum(all_faces)
    print(islands(points))
    part2 = 0
    return Solution(part1, part2)


def exposed_faces(point: P3, points: set[P3]) -> int:
    neighbors = {point.add(n) for n in NEIGHBORS}
    overlap = neighbors.intersection(points)
    return 6 - len(overlap)

def islands(points: set[P3]) -> set[P3]:
    air_pockets = set()
    for point in points:
        neighbors = {point.add(n) for n in NEIGHBORS}
        if neighbors.isdisjoint(points):
            air_pockets.add(point)
    return air_pockets


