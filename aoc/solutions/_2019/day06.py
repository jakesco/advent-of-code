import aoc.utils.tree as t
from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    tree = build_tree(puzzle_input)
    orbits = t.process(tree["COM"], count_orbits)
    moves = move_orbits(tree["YOU"], tree["SAN"])

    return Solution(sum(orbits), moves)


def build_tree(orbits: list[str]) -> dict[str, t.Node]:
    nodes = {}
    for orbit in orbits:
        parent, child = orbit.split(")")
        if parent not in nodes:
            nodes[parent] = t.Node(parent)
        if child not in nodes:
            nodes[child] = t.Node(child)
        nodes[parent].add(nodes[child])
    return nodes


def count_orbits(n: t.Node) -> int:
    orbits = 0
    while n.parent is not None:
        orbits += 1
        n = n.parent
    return orbits


def move_orbits(you: t.Node, santa: t.Node) -> int:
    shared_parents = you.parents().intersection(santa.parents())
    return count_moves(you, shared_parents) + count_moves(santa, shared_parents)


def count_moves(n: t.Node, stop_nodes: set[str]) -> int:
    current = n.parent
    moves = 0
    while current and current.name not in stop_nodes:
        moves += 1
        current = current.parent
    return moves


if __name__ == "__main__":
    print(
        main(
            [
                "COM)B",
                "B)C",
                "C)D",
                "D)E",
                "E)F",
                "B)G",
                "G)H",
                "D)I",
                "E)J",
                "J)K",
                "K)L",
                "K)YOU",
                "I)SAN",
            ]
        )
    )
