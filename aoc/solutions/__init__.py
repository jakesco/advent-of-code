from pathlib import Path

from . import day01, day02, shared

SOLUTION_REGISTRY = {
    1: day01.main,
    2: day02.main,
}


def run(day: int, filename: Path, verbose: bool = False):
    if day not in SOLUTION_REGISTRY.keys():
        raise IndexError("day not implemented yet: '%s'" % day)
    if not filename.exists():
        raise FileNotFoundError("input file does not exist: '%s'" % filename)

    shared.verbose.set(verbose)
    if shared.verbose.get():
        print(f"Running day {day} solution on input {filename}...")

    solution: shared.Solution = SOLUTION_REGISTRY[day](filename)
    print(solution)
