from __future__ import annotations

import re
from dataclasses import dataclass

from .shared import Solution

SIZE_CAP = 100_000
DISK_SPACE = 70_000_000
SPACE_NEEDED = 30_000_000

RE_CMD = re.compile(r"^\$ (?P<command>[a-z]+) ?(?P<arg>.*)?$")
RE_DIR = re.compile(r"^dir (?P<dirname>.*)$")
RE_FILE = re.compile(r"^(?P<size>\d+) (?P<name>.*)$")


@dataclass
class File:
    parent: Directory
    name: str
    size: int

    def __str__(self) -> str:
        return f"{self.name} (file, size={self.size})"


@dataclass
class Directory:
    parent: Directory | None
    name: str
    children: dict[str, File | Directory]

    @property
    def size(self) -> int:
        return sum([entity.size for entity in self.children.values()])

    def add_child(self, child: File | Directory):
        self.children[child.name] = child

    def __str__(self) -> str:
        return f"{self.name} (dir)"


def main(input_: list[str]) -> Solution:
    current = root = Directory(None, "/", {})
    dirs = [root]
    for line in input_[1:]:
        if m := RE_CMD.match(line):
            match (m.group("command"), m.group("arg")):
                case ("cd", ".."):
                    current = current.parent
                case ("cd", target):
                    current = current.children[target]
                case _:
                    continue
        elif m := RE_DIR.match(line):
            new_dir = Directory(current, m.group("dirname"), {})
            dirs.append(new_dir)
            current.add_child(new_dir)
        elif m := RE_FILE.match(line):
            new_file = File(current, m.group("name"), int(m.group("size")))
            current.add_child(new_file)

    # print_tree(root)
    sizes = [d.size for d in dirs]
    unused = DISK_SPACE - root.size

    part1 = sum([s for s in sizes if s <= SIZE_CAP])
    part2 = min([s for s in sizes if s + unused > SPACE_NEEDED])
    return Solution(part1, part2)


def print_tree(entity: Directory | File, indent: int = 0):
    print(f"{indent * '  '}-", entity)
    if isinstance(entity, Directory):
        for child in entity.children.values():
            print_tree(child, indent=indent + 1)
