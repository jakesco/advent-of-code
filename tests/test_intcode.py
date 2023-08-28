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


def simple_amp(tape: str, sequence: list[int]) -> int:
    interpreter = IntcodeInterpreter.loads(tape)
    output = 0
    for s in sequence:
        interpreter.reset()
        interpreter.input = [s, output]
        interpreter.execute()
        output = interpreter.output.pop(0)
    return output


def test_simple_amp_1():
    sequence = [4, 3, 2, 1, 0]
    tape = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    assert simple_amp(tape, sequence) == 43210


def test_simple_amp_2():
    sequence = [0, 1, 2, 3, 4]
    tape = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    assert simple_amp(tape, sequence) == 54321


def test_simple_amp_3():
    sequence = [1, 0, 4, 3, 2]
    tape = (
        "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,"
        "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    )
    assert simple_amp(tape, sequence) == 65210


def advanced_amp(tape: str, sequence: list[int]) -> int:
    interpreters = [
        IntcodeInterpreter.loads(tape, debug=True, block_on_output=True)
        for _ in range(5)
    ]
    for idx, s in enumerate(sequence):
        interpreters[idx].input = [s]
    output = 0
    while not interpreters[-1]._halted:
        for i in interpreters:
            i.input.append(output)
            i.execute()
            if i.output:
                output = i.output.pop(0)
    return output


def test_advanced_amp_1():
    sequence = [9, 8, 7, 6, 5]
    tape = (
        "3,26,1001,26,-4,26,3,27,1002,27,2,27,"
        "1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,"
        "0,5"
    )
    assert advanced_amp(tape, sequence) == 139629729


def test_advanced_amp_2():
    sequence = [9, 7, 8, 5, 6]
    tape = (
        "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,"
        "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,"
        "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
    )
    assert advanced_amp(tape, sequence) == 18216
