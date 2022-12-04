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
