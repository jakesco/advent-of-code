from itertools import chain, product

from .shared import P, Solution

MOVE_MAP = {
    "R": P(1, 0),
    "L": P(-1, 0),
    "U": P(0, 1),
    "D": P(0, -1),
}


def main(input_: list[str]) -> Solution:
    visited1 = {P(0, 0)}
    visited2 = {P(0, 0)}
    knots = 10 * [P(0, 0)]
    moves = chain(*map(lambda x: int(x[1]) * x[0], map(lambda x: x.split(" "), input_)))

    for move in moves:
        move = MOVE_MAP[move]
        knots[0] = knots[0].add(move)
        for idx, knot in enumerate(knots[1:], start=1):
            knots[idx] = update_knot(knots[idx - 1], knot)
        visited1.add(knots[1])
        visited2.add(knots[-1])

    return Solution(len(visited1), len(visited2))


def update_knot(h: P, t: P) -> P:
    if adjacent(h, t):
        return t

    diff = h.diff(t)
    if diff.mag() > 1:
        vec = unit(diff)
        return t.add(vec)

    return t


def adjacent(h: P, t: P) -> bool:
    neighbors = (
        h.add(P(x, y)) for x, y in product((-1, 0, 1), (-1, 0, 1)) if (x, y) != (0, 0)
    )
    return t in neighbors


def unit(p: P) -> P:
    """Make an integer unit vector"""
    if p.x == 0:
        return P(0, p.y // abs(p.y))
    if p.y == 0:
        return P(p.x // abs(p.x), 0)
    return P(p.x // abs(p.x), p.y // abs(p.y))
