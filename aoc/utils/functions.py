from itertools import islice


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
