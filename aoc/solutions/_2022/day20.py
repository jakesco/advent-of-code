from __future__ import annotations

from dataclasses import dataclass

from .shared import Solution

DECRYPTION_KEY = 811589153


@dataclass
class Node:
    val: int
    prev: Node | None = None
    next: Node | None = None


def main(input_: list[str]) -> Solution:
    head, node_list = build_linked_list(input_)
    for node in node_list:
        mix(node, len(node_list))
    part1 = score(head)

    head, node_list = build_linked_list(input_, apply_key=True)
    for node in node_list * 10:
        mix(node, len(node_list))
    part2 = score(head)
    return Solution(part1, part2)


def build_linked_list(lines: list[str], apply_key: bool = False) -> (Node, list[Node]):
    nodes = []
    for val in map(int, lines):
        if apply_key:
            val = val * DECRYPTION_KEY
        nodes.append(Node(val))
    for idx, node in enumerate(nodes):
        prev_idx = (idx - 1) % len(nodes)
        next_idx = (idx + 1) % len(nodes)
        node.prev = nodes[prev_idx]
        node.next = nodes[next_idx]
    return find_zero(nodes[0]), nodes


def find_zero(head: Node) -> Node:
    if head.val == 0:
        return head
    current = head.next
    while current != head:
        if current.val == 0:
            return current
        current = current.next


def mix(node: Node, node_count: int):
    steps = node.val % (node_count - 1)
    if node.val == 0 or steps == 0:
        return

    # Remove node
    node.prev.next = node.next
    node.next.prev = node.prev

    # Find insert position
    target = node.prev
    for _ in range(steps):
        target = target.next

    # Insert node
    node.next = target.next
    node.prev = target
    target.next.prev = node
    target.next = node


def score(head: Node) -> int:
    total = 0
    current = head.next
    for i in range(1, 3_001):
        if i % 1000 == 0:
            total += current.val
        current = current.next
    return total
