from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Callable

from .shared import Solution

RE_OP = re.compile(
    r"^(?P<name>[a-z]+): (?P<left>[a-z]+) (?P<op>[+|\-|*|/]) (?P<right>[a-z]+)$"
)
RE_NUMBER = re.compile(r"^(?P<name>[a-z]+): (?P<num>\d+)$")

# Ops also hold their inverse, makes part 2 easier.
OPS = {
    "+": (lambda x, y: x + y, lambda z, x: z - x, lambda z, y: z - y),
    "-": (lambda x, y: x - y, lambda z, x: x - z, lambda z, y: z + y),
    "*": (lambda x, y: x * y, lambda z, x: z // x, lambda z, y: z // y),
    "/": (lambda x, y: x // y, lambda z, x: x // z, lambda z, y: z * y),
}

SIDE = {
    (True, False): "left",
    (False, True): "right",
}


@dataclass
class Leaf:
    name: str
    val: int


@dataclass
class Node:
    name: str
    op: tuple[Callable[[int, int], int], ...]
    left: Node | Leaf | str = None
    right: Node | Leaf | str = None


def main(input_: list[str]) -> Solution:
    root = parse_tree(input_)
    part1 = eval_tree(root)
    start, target = eval_opposite_side(root)
    part2 = eval_tree_2(getattr(root, start), target)
    return Solution(part1, part2)


def parse_tree(lines: list[str]) -> Node:
    nodes = dict()
    leaves = dict()
    for line in lines:
        if m := RE_NUMBER.fullmatch(line):
            leaves[m.group("name")] = Leaf(m.group("name"), int(m.group("num")))
        if m := RE_OP.fullmatch(line):
            nodes[m.group("name")] = m

    def build_tree(name: str) -> Node | Leaf:
        if leaf := leaves.get(name):
            return leaf
        m = nodes[name]
        return Node(
            name=m.group("name"),
            op=OPS[m.group("op")],
            left=build_tree(m.group("left")),
            right=build_tree(m.group("right")),
        )

    return build_tree("root")


def eval_tree(node: Node | Leaf) -> int:
    if isinstance(node, Leaf):
        return node.val
    left = eval_tree(node.left)
    right = eval_tree(node.right)
    return node.op[0](left, right)


def eval_tree_2(node, target: int) -> int:
    if node.name == "humn":
        return target
    side, result = eval_opposite_side(node)
    if side == "right":
        target = node.op[1](target, result)
    else:
        target = node.op[2](target, result)
    return eval_tree_2(getattr(node, side), target)


def eval_opposite_side(node: Node) -> (str, int):
    """Finds 'humn' and evals opposite side. (e.g. (left, eval(right)))"""
    side = SIDE[humn_in_branch(node.left), humn_in_branch(node.right)]
    return side, eval_tree(getattr(node, opposite_side(side)))


def opposite_side(side: str) -> str:
    return "left" if side == "right" else "right"


def humn_in_branch(node: Node) -> bool:
    if node.name == "humn":
        return True
    if isinstance(node, Leaf):
        return False
    return humn_in_branch(node.left) or humn_in_branch(node.right)
