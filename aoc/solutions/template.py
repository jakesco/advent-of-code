from pathlib import Path

from .shared import Solution


def main(filename: Path) -> Solution:
    with filename.open() as f:
        print(f.read())
    return Solution()
