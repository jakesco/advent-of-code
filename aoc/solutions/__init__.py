import pkgutil
import re
from importlib import import_module
from importlib.util import find_spec
from pathlib import Path
from typing import Callable

SOLUTION_REGEX = re.compile(r"day(?P<day>[0-2][1-9])")


def find_solutions() -> dict[int, Callable]:
    """Walks `solutions` package directory finding all modules with a name like dayXX.
    Will then register the `main` function for each day in a dictionary.
    """
    solution_map = dict()
    for pkg in pkgutil.iter_modules(find_spec(__name__).submodule_search_locations):
        if match := SOLUTION_REGEX.fullmatch(pkg.name):
            module = import_module(f".{pkg.name}", package=__name__)
            day = int(match.group("day"))
            solution_map[day] = getattr(module, "main")
    return solution_map


def run(day: int, filename: Path):
    """Run solution for a given day."""
    solution_registry = find_solutions()

    if day not in solution_registry.keys():
        raise IndexError("day not implemented yet: '%s'" % day)
    if not filename.exists():
        raise FileNotFoundError("input file does not exist: '%s'" % filename)

    solution = solution_registry[day](filename)
    print(solution)
