from hashlib import md5

from aoc.utils.interfaces import Solution

MAX_RUNS = 10_000_000


def main(puzzle_input: list[str]) -> Solution:
    secret_key = puzzle_input[0]

    part1 = search_for_key(secret_key, 0, 5)
    part2 = search_for_key(secret_key, part1, 6)

    return Solution(part1, part2)


def search_for_key(secret_key: str, start: int, n_zeros: int) -> int:
    for i in range(start, MAX_RUNS):
        hash_ = md5_hash(f"{secret_key}{i}")
        if check_hash(hash_, n_zeros):
            return i


def check_hash(hash_: str, n_zeros: int) -> bool:
    return hash_.startswith("0" * n_zeros)


def md5_hash(data: str) -> str:
    return md5(data.encode()).hexdigest()
