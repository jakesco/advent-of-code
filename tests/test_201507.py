from aoc.solutions._2015.day07 import parse_connection, run_simulation


def test_solution():
    input_ = [
        "123 -> x",
        "456 -> y",
        "x AND y -> d",
        "x OR y -> e",
        "x LSHIFT 2 -> f",
        "y RSHIFT 2 -> g",
        "NOT x -> h",
        "NOT y -> i",
    ]
    connections = [parse_connection(line) for line in input_]
    result = run_simulation(connections)
    expected = {
        "d": 72,
        "e": 507,
        "f": 492,
        "g": 114,
        "h": 65412,
        "i": 65079,
        "x": 123,
        "y": 456,
    }
    for k, v in expected.items():
        assert result[k] == v
