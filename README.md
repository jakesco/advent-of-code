# AOC2022

My solutions for Advent of Code 2022.

Run solutions by passing the day and puzzle input like so:
```
$ python aoc <day> <input>
```

## Solutions

* Day 01: [Calorie Counting](https://adventofcode.com/2022/day/1) | [solution](./aoc/solutions/day01.py)
* Day 02: [Rock Paper Scissors](https://adventofcode.com/2022/day/2) | [solution](./aoc/solutions/day02.py)
* Day 03: [Rucksack Reorganization](https://adventofcode.com/2022/day/3) | [solution](./aoc/solutions/day03.py)
* Day 04: [Camp Cleanup](https://adventofcode.com/2022/day/4) | [solution](./aoc/solutions/day04.py)
* Day 05: [Supply Stacks](https://adventofcode.com/2022/day/5) | [solution](./aoc/solutions/day05.py)
* Day 06: [Tuning Trouble](https://adventofcode.com/2022/day/6) | [solution](./aoc/solutions/day06.py)
* Day 07: [No Space Left On Device](https://adventofcode.com/2022/day/7) | [solution](./aoc/solutions/day07.py)
* Day 08: [Treetop Tree House](https://adventofcode.com/2022/day/8) | [solution](./aoc/solutions/day08.py)
* Day 09: [Rope Bridge](https://adventofcode.com/2022/day/9) | [solution](./aoc/solutions/day09.py)
* Day 10: [Cathode-Ray Tube](https://adventofcode.com/2022/day/10) | [solution](./aoc/solutions/day10.py)
* Day 11: [Monkey in the Middle](https://adventofcode.com/2022/day/11) | [solution](./aoc/solutions/day11.py)
* Day 12: [Hill Climbing Algorithm](https://adventofcode.com/2022/day/12) | [solution](./aoc/solutions/day12.py)
* Day 13: [Distress Signal](https://adventofcode.com/2022/day/13) | [solution](./aoc/solutions/day13.py)
* Day 14: [Regolith Reservoir](https://adventofcode.com/2022/day/14) | [solution](./aoc/solutions/day14.py)
* Day 15: [Beacon Exclusion Zone](https://adventofcode.com/2022/day/15) | [solution](./aoc/solutions/day15.py)
* Day 16: [Proboscidea Volcanium](https://adventofcode.com/2022/day/16) | [solution](./aoc/solutions/day16.py)
* Day 17: [Pyroclastic Flow](https://adventofcode.com/2022/day/17) | [solution](./aoc/solutions/day17.py)
* Day 18: [Boiling Boulders](https://adventofcode.com/2022/day/18) | [solution](./aoc/solutions/day18.py)
* Day 19: [Not Enough Minerals](https://adventofcode.com/2022/day/19) | solution
* Day 20: [Grove Positioning System](https://adventofcode.com/2022/day/20) | [solution](./aoc/solutions/day20.py)
* Day 21: [Monkey Math](https://adventofcode.com/2022/day/21) | solution


## Puzzle Input Tip

If you have your Advent of Code session token saved to a file like `.token`, you can download your test input with curl:
```
$ curl "https://adventofcode.com/2022/day/1/input" \
       --header "Cookie: $(cat .token)"
```
You can find your session token by looking at the `Cookie` header in the network tab
in your browser tools when navigating to https://adventofcode.com/2022/day/1/input.
Save the value of this `Cookie` header in `.token`. e.g.
```
session=XXX
```
I've added a `make` command for convenience:
```
$ make get-input day=<day>
```