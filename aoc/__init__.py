import argparse
import sys
import traceback
from pathlib import Path

from aoc.solutions import solve
from aoc.utils.downloader import cache_puzzle_input

__version__ = "2024.12.1"


def _init_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="aoc",
        description="Solutions for Advent of Code",
    )
    parser.add_argument(
        "year", type=int, help="solution year to run, between 2015 and 2022."
    )
    parser.add_argument("day", type=int, help="solution day to run, between 1 and 25.")
    parser.add_argument(
        "filename",
        type=Path,
        help=(
            "path to puzzle input file. "
            "(If no path given, puzzle input will be downloaded.)"
        ),
        nargs="?",
        default=None,
    )
    parser.add_argument(
        "-d",
        "--download",
        action="store_true",
        default=False,
        help="download puzzle input only, without running puzzle solution.",
    )
    return parser


def main():
    args = _init_parser().parse_args()
    try:
        if args.download:
            cache_puzzle_input(args.year, args.day)
            sys.exit(0)

        filename = args.filename
        if filename is None:
            filename = cache_puzzle_input(args.year, args.day)

        solution = solve(args.year, args.day, filename)
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
