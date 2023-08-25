from aoc.solutions._2019.day06 import main


def test_201906_1():
    test_input = [
        "COM)B",
        "B)C",
        "C)D",
        "D)E",
        "E)F",
        "B)G",
        "G)H",
        "D)I",
        "E)J",
        "J)K",
        "K)L",
    ]
    sol = main(test_input)
    assert sol.part1 == 42


def test_201906_2():
    test_input = [
        "COM)B",
        "B)C",
        "C)D",
        "D)E",
        "E)F",
        "B)G",
        "G)H",
        "D)I",
        "E)J",
        "J)K",
        "K)L",
        "K)YOU",
        "I)SAN",
    ]
    sol = main(test_input)
    assert sol.part2 == 4
