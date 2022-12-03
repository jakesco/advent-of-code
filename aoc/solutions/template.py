from pathlib import Path

from .shared import Solution


def main(filename: Path) -> Solution:
    solution = Solution()
    with filename.open() as f:
        print(f.read())
    return solution
