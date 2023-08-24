from aoc.utils.interfaces import Solution


def main(puzzle_input: list[str]) -> Solution:
    for line in puzzle_input:
        print(line)
    return Solution()


if __name__ == "__main__":
    print(main(["Hello,", "World!"]))
