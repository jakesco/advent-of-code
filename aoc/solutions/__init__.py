import pkgutil
from importlib import import_module
from importlib.util import find_spec
from pathlib import Path
from typing import Callable

from aoc.utils.interfaces import Solution


def solve(year: int, day: int, filename: Path) -> Solution:
    """Run solution for a given day."""

    if not filename.exists():
        raise FileNotFoundError("input file does not exist: '%s'" % filename)

    solution = _find_solution(year, day)

    with filename.open() as f:
        puzzle_input = f.read().splitlines()

    return solution(puzzle_input)


def _find_solution(year: int, day: int) -> Callable[[list[str]], Solution]:
    """Walks `solutions` package directory finding all modules with a name like _year.dayXX.
    Will then find the `main` function for the specified year and day.
    """
    solution_module = _find_solution_year(year)
    for pkg in pkgutil.iter_modules(
        find_spec(f"{__name__}.{solution_module.name}").submodule_search_locations
    ):
        if f"{day:02}" in pkg.name:
            module = import_module(
                f".{solution_module.name}.{pkg.name}", package=__name__
            )
            return getattr(module, "main")
    raise IndexError("solution not implemented for day %s" % day)


def _find_solution_year(year: int) -> pkgutil.ModuleInfo:
    """Walks `solutions` package to find implemented year module"""
    for pkg in pkgutil.iter_modules(find_spec(__name__).submodule_search_locations):
        if str(year) in pkg.name:
            return pkg
    raise IndexError("no solutions implemented for %s" % year)
