from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    search_space = range(*map(int, puzzle_input[0].split("-")))
    part1 = sum([validate(str(candidate)) for candidate in search_space])
    part2 = sum([validate(str(candidate), strict=True) for candidate in search_space])
    return Solution(part1, part2)


def validate(candidate: str, *, strict: bool = False) -> bool:
    doubles = []
    current = candidate[0]
    count = 1
    for next_ in candidate[1:]:
        if current > next_:
            return False

        if current != next_:
            doubles.append(count)
            count = 0

        count += 1
        current = next_
    doubles.append(count)

    if strict:
        return 2 in doubles
    else:
        return max(doubles) > 1


if __name__ == "__main__":
    test1 = {
        "111111": True,
        "223450": False,
        "123789": False,
    }
    test2 = {
        "112233": True,
        "123444": False,
        "111122": True,
    }

    for k, v in test1.items():
        print(f"Running: {k} | Expect: {v} | Result: {validate(k)}")
        assert validate(k) == v

    for k, v in test2.items():
        print(f"Running: {k} | Expect: {v} | Result: {validate(k, strict=True)}")
        assert validate(k, strict=True) == v
