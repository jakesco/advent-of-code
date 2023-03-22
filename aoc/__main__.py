import argparse
import sys
import traceback
from pathlib import Path

from solutions import solve

parser = argparse.ArgumentParser(
    prog="aoc",
    description="Solutions for Advent of Code 2022",
)

parser.add_argument(
    "year", type=int, help="Solution year to run, between 2015 and 2022."
)
parser.add_argument("day", type=int, help="Solution day to run, between 1 and 25.")
parser.add_argument("filename", type=Path, help="Path to puzzle input file.")

args = parser.parse_args()

try:
    solution = solve(args.year, args.day, args.filename)
    print(solution)
except FileNotFoundError as e:
    sys.stderr.write("aoc: error: %s\n" % e)
    sys.exit(1)
except IndexError as e:
    sys.stderr.write("aoc: error: %s\n" % e)
    sys.exit(2)
except Exception as e:
    sys.stderr.write("aoc: error: %s\n" % e)
    traceback.print_exc()
    sys.exit(3)
