from aoc.utils.intcode import IntcodeInterpreter

# fmt: off
LARGE_TEST_DAY_5 = [
    3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20,
    31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46,
    104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99,
]
# fmt: on


def test_add_1():
    interpreter = IntcodeInterpreter([1, 0, 0, 0, 99])
    interpreter.execute()
    assert interpreter._memory == [2, 0, 0, 0, 99]


def test_add_2():
    interpreter = IntcodeInterpreter([1, 1, 1, 4, 99, 5, 6, 0, 99])
    interpreter.execute()
    assert interpreter._memory == [30, 1, 1, 4, 2, 5, 6, 0, 99]


def test_mul_1():
    interpreter = IntcodeInterpreter([2, 3, 0, 3, 99])
    interpreter.execute()
    assert interpreter._memory == [2, 3, 0, 6, 99]


def test_mul_2():
    interpreter = IntcodeInterpreter([2, 4, 4, 5, 99, 0])
    interpreter.execute()
    assert interpreter._memory == [2, 4, 4, 5, 99, 9801]


def test_input():
    interpreter = IntcodeInterpreter([3, 0, 99])
    interpreter.input = [42]
    interpreter.execute()
    assert interpreter._memory == [42, 0, 99]


def test_output():
    interpreter = IntcodeInterpreter([4, 0, 99])
    interpreter.execute()
    assert interpreter.output == [4]


def test_mode_setting():
    interpreter = IntcodeInterpreter([1002, 4, 3, 4, 33])
    interpreter.execute()
    assert interpreter._memory == [1002, 4, 3, 4, 99]


def test_mode_setting_2():
    interpreter = IntcodeInterpreter([1101, 100, -1, 4, 0])
    interpreter.execute()
    assert interpreter._memory == [1101, 100, -1, 4, 99]


def test_jump_if_true():
    interpreter = IntcodeInterpreter([1105, 1, 4, 99, 99])
    interpreter.execute()
    assert interpreter._p == 4


def test_jump_if_true_2():
    interpreter = IntcodeInterpreter([1105, 0, 4, 99, 0])
    interpreter.execute()
    assert interpreter._p == 3


def test_jump_if_false():
    interpreter = IntcodeInterpreter([1106, 0, 4, 99, 99])
    interpreter.execute()
    assert interpreter._p == 4


def test_jump_if_false_2():
    interpreter = IntcodeInterpreter([1106, 1, 4, 99, 0])
    interpreter.execute()
    assert interpreter._p == 3


def test_less_than():
    interpreter = IntcodeInterpreter([1107, 1, 2, 5, 99, 0])
    interpreter.execute()
    assert interpreter._memory == [1107, 1, 2, 5, 99, 1]


def test_less_than_2():
    interpreter = IntcodeInterpreter([1107, 2, 1, 5, 99, 1])
    interpreter.execute()
    assert interpreter._memory == [1107, 2, 1, 5, 99, 0]


def test_less_than_position_mode():
    interpreter = IntcodeInterpreter([7, 3, 4, 5, 99, 0])
    interpreter.execute()
    assert interpreter._memory == [7, 3, 4, 5, 99, 1]


def test_equals():
    interpreter = IntcodeInterpreter([1108, 1, 1, 5, 99, 0])
    interpreter.execute()
    assert interpreter._memory == [1108, 1, 1, 5, 99, 1]


def test_compare_functional_1():
    """Using position mode, consider whether the input is equal to 8;
    output 1 (if it is) or 0 (if it is not).
    """
    interpreter = IntcodeInterpreter([3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8])
    interpreter.input = [8]
    interpreter.execute()
    assert interpreter.output == [1]
    interpreter.reset()
    interpreter.input = [7]
    interpreter.execute()
    assert interpreter.output == [0]


def test_compare_functional_2():
    """Using position mode, consider whether the input is less than 8;
    output 1 (if it is) or 0 (if it is not).
    """
    interpreter = IntcodeInterpreter([3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8])
    interpreter.input = [7]
    interpreter.execute()
    assert interpreter.output == [1]
    interpreter.reset()
    interpreter.input = [8]
    interpreter.execute()
    assert interpreter.output == [0]


def test_compare_functional_3():
    """Using immediate mode, consider whether the input is equal to 8;
    output 1 (if it is) or 0 (if it is not).
    """
    interpreter = IntcodeInterpreter([3, 3, 1108, -1, 8, 3, 4, 3, 99])
    interpreter.input = [8]
    interpreter.execute()
    assert interpreter.output == [1]
    interpreter.reset()
    interpreter.input = [7]
    interpreter.execute()
    assert interpreter.output == [0]


def test_compare_functional_4():
    """Using immediate mode, consider whether the input is less than 8;
    output 1 (if it is) or 0 (if it is not).
    """
    interpreter = IntcodeInterpreter([3, 3, 1107, -1, 8, 3, 4, 3, 99])
    interpreter.input = [7]
    interpreter.execute()
    assert interpreter.output == [1]
    interpreter.reset()
    interpreter.input = [8]
    interpreter.execute()
    assert interpreter.output == [0]


def test_jump_position_1():
    interpreter = IntcodeInterpreter(
        [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]
    )
    interpreter.input = [0]
    interpreter.execute()
    assert interpreter.output == [0]
    interpreter.reset()
    interpreter.input = [1]
    interpreter.execute()
    assert interpreter.output == [1]


def test_jump_immediate_2():
    interpreter = IntcodeInterpreter([3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1])
    interpreter.input = [0]
    interpreter.execute()
    assert interpreter.output == [0]
    interpreter.reset()
    interpreter.input = [1]
    interpreter.execute()
    assert interpreter.output == [1]


def test_large_example_1():
    interpreter = IntcodeInterpreter(LARGE_TEST_DAY_5)
    interpreter.input = [7]
    interpreter.execute()
    assert interpreter.output == [999]


def test_large_example_2():
    interpreter = IntcodeInterpreter(LARGE_TEST_DAY_5)
    interpreter.input = [8]
    interpreter.execute()
    assert interpreter.output == [1000]


def test_large_example_3():
    interpreter = IntcodeInterpreter(LARGE_TEST_DAY_5)
    interpreter.input = [9]
    interpreter.execute()
    assert interpreter.output == [1001]
