from functools import reduce
from itertools import islice
from typing import Callable, TypeVar

T = TypeVar("T")


def batched(iterable, n):
    """
    Batch data into lists of length n. The last batch may be shorter.
    Recipe from https://docs.python.org/3/library/itertools.html#itertools-recipes
    """
    # batched('ABCDEFG', 3) --> ABC DEF G
    if n < 1:
        raise ValueError("n must be at least one")
    it = iter(iterable)
    while batch := list(islice(it, n)):
        yield batch


def apply(initial: T, funcs: list[Callable[[T], T]]) -> T:
    """Apply all functions to initial argument."""
    return reduce(lambda x, y: y(x), funcs[1:], funcs[0](initial))
