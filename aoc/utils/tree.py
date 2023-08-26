"""
Some generic tree utilities.

usage:
    import aoc.utils.tree as t

    root = t.Node("root")
    root.add(t.Node("child1"))
    root.add(t.Node("child2"))
    t.print_tree(root)
"""
from __future__ import annotations

from collections.abc import Callable, Iterator
from dataclasses import dataclass, field
from typing import Any


@dataclass
class Node:
    name: str
    value: Any | None = None
    parent: Node | None = None
    children: list[Node] = field(default_factory=list)

    def add(self, child: Node):
        child.parent = self
        self.children.append(child)

    def parents(self) -> set[str]:
        current = self
        nodes = set()
        while current.parent:
            nodes.add(current.parent.name)
            current = current.parent
        return nodes

    def __str__(self) -> str:
        return f"{self.name}({self.value})"


def traverse(root: Node, func: Callable[[Node], None]):
    func(root)
    for child in root.children:
        traverse(child, func)


def process(root: Node, func: Callable[[Node], Any]) -> Iterator[Any]:
    yield func(root)
    for child in root.children:
        yield from process(child, func)


def smallest_subtree(root: Node, *args: Node) -> Node:
    """Searches for the smallest subtree that contains all nodes in args."""
    # TODO
    pass


def print_tree(root: Node, depth: int = 0):
    print("  " * depth + str(root))
    for child in root.children:
        print_tree(child, depth + 1)
