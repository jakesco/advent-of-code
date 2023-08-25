import aoc.utils.tree as t
from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    root = build_tree(puzzle_input)
    orbits = t.process(root, count_orbits)
    return Solution(sum(orbits))


def build_tree(orbits: list[str]) -> t.Node:
    nodes = {}
    for orbit in orbits:
        parent, child = orbit.split(")")
        if parent not in nodes:
            nodes[parent] = t.Node(parent)
        if child not in nodes:
            nodes[child] = t.Node(child)
        nodes[parent].add(nodes[child])
    return nodes["COM"]


def count_orbits(n: t.Node) -> int:
    orbits = 0
    while n.parent is not None:
        orbits += 1
        n = n.parent
    return orbits


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
