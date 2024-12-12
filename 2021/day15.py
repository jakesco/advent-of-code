import argparse
import os
from dataclasses import dataclass


@dataclass(frozen=True)
class Node:
    x: int
    y: int
    risk: int
    end: bool = False

    def __repr__(self) -> str:
        return str(self.risk)


class Path:
    def __init__(self, current: Node, previous: list[Node], heuristic: float = 0):
        self.current = current
        self.path = previous
        self.heuristic = heuristic

    def __repr__(self):
        return (
            f"Search(({self.current.x}, {self.current.y}), "
            f"{len(self.path)}, {self.heuristic})"
        )

    @property
    def risk(self):
        return self.current.risk + sum(
            [n.risk for n in self.path if not n.x == n.y == 0]
        )


class Graph:
    def __init__(self, grid: list[list[int]]):
        self.rows = len(grid)
        self.cols = len(grid[0])
        self.__nodes = []
        for r in range(self.rows):
            for c in range(self.cols):
                self.__nodes.append(
                    Node(r, c, grid[r][c], r == self.rows - 1 and c == self.cols - 1)
                )

    def __str__(self):
        output = [str(n) for n in self.__nodes]
        for r in range(self.cols, len(self.__nodes), self.cols + 1):
            output.insert(r, "\n")
        return "".join(output)

    def render_path(self, path: Path):
        output = []
        for n in self.__nodes:
            if n in path.path:
                output.append(".")
            elif n.end:
                output.append(".")
            else:
                output.append(str(n))
        for r in range(self.cols, len(self.__nodes), self.cols + 1):
            output.insert(r, "\n")
        print("".join(output))

    def get(self, row: int, col: int) -> Node | None:
        if (0 <= row < self.rows) and (0 <= col < self.cols):
            return self.__nodes[row * self.cols + col]
        return None

    def neighbors(self, node: Node) -> list[Node]:
        adj = [
            self.get(node.x + i, node.y + j)
            for i, j in [(1, 0), (0, 1), (-1, 0), (0, -1)]
        ]
        return [n for n in adj if n is not None]

    def dijkstra(self) -> Path | None:
        """Search for shortest path using Dijkstra's shortest path."""
        init = Path(self.__nodes[0], list())
        visited = set()
        q = [init]

        while q:
            candidate = q.pop()

            if candidate.current in visited:
                continue

            if candidate.current.end:
                return candidate

            for node in self.neighbors(candidate.current):
                if node in visited:
                    continue

                heuristic = candidate.heuristic + node.risk
                previous = candidate.path.copy()
                previous.append(candidate.current)
                new_path = Path(node, previous, heuristic)
                q.append(new_path)

            visited.add(candidate.current)
            q.sort(key=lambda p: p.heuristic, reverse=True)

        return None


def read_input(filepath: str, *, expand: bool = False) -> Graph:
    map_ = list()
    with open(filepath, "r") as f:
        if expand:
            for line in f.readlines():
                l = [int(n) for n in line.strip()]
                out = l
                for i in range(4):
                    l = [1 if n == 9 else (n + 1) for n in l]
                    out = out + l
                map_.append(out)
            rows = len(map_)
            for i in range(4):
                tmp = list()
                for j in range(-1 * rows, 0):
                    tmp.append([1 if n == 9 else (n + 1) for n in map_[j]])
                for t in tmp:
                    map_.append(t)
        else:
            for line in f.readlines():
                map_.append([int(n) for n in line.strip()])
    return Graph(map_)


def init_parser() -> str:
    parser = argparse.ArgumentParser(description="Advent of Code day 15 solution.")
    parser.add_argument(
        "input", metavar="FILE", type=str, nargs=1, help="Path to input data."
    )
    args = parser.parse_args()
    return os.path.realpath(args.input[0])


if __name__ == "__main__":
    path = init_parser()
    g_p1 = read_input(path)
    g_p2 = read_input(path, expand=True)

    path1 = g_p1.dijkstra()
    print(f"Part 1: {path1.risk}")

    path2 = g_p2.dijkstra()
    print(f"Part 2: {path2.risk}")


def main(_):
    raise NotImplementedError
