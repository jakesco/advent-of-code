from pathlib import Path

from .shared import Solution, verbose


def main(filename: Path) -> Solution:
    if verbose.get():
        print("Running with verbose output.")
    with filename.open() as f:
        print(f.read())
    return Solution()
