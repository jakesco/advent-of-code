# Advent of Code

My solutions for [Advent of Code](https://adventofcode.com).

Run solutions by passing the year, day and puzzle input like so:
```
$ python aoc <year> <day> [input]
```

If no input is provided, and you have a `.token` file in the root 
of the project, the script will attempt to automatically download 
your puzzle input from the Advent of Code website. Steps to get 
your session token are in the [Puzzle Input Tip](#puzzle-input-tip) 
section below.

## Solutions

<details>
<summary>2015</summary>

* Day 01: [Not Quite Lisp](https://adventofcode.com/2015/day/1) | [solution](./aoc/solutions/_2015/day01.py)
* Day 02: [I Was Told There Would Be No Math](https://adventofcode.com/2015/day/2) | [solution](./aoc/solutions/_2015/day02.py)
* Day 03: [Perfectly Spherical Houses in a Vacuum](https://adventofcode.com/2015/day/3) | [solution](./aoc/solutions/_2015/day03.py)
* Day 04: [The Ideal Stocking Stuffer](https://adventofcode.com/2015/day/4) | [solution](./aoc/solutions/_2015/day04.py)
* Day 05: [Doesn't He Have Intern-Elves For This?](https://adventofcode.com/2015/day/5) | [solution](./aoc/solutions/_2015/day05.py)
* Day 06: [Probably a Fire Hazard](https://adventofcode.com/2015/day/6) | [solution](./aoc/solutions/_2015/day06.py)

</details>

<details open>
<summary>2022</summary>

* Day 01: [Calorie Counting](https://adventofcode.com/2022/day/1) | [solution](./aoc/solutions/_2022/day01.py)
* Day 02: [Rock Paper Scissors](https://adventofcode.com/2022/day/2) | [solution](./aoc/solutions/_2022/day02.py)
* Day 03: [Rucksack Reorganization](https://adventofcode.com/2022/day/3) | [solution](./aoc/solutions/_2022/day03.py)
* Day 04: [Camp Cleanup](https://adventofcode.com/2022/day/4) | [solution](./aoc/solutions/_2022/day04.py)
* Day 05: [Supply Stacks](https://adventofcode.com/2022/day/5) | [solution](./aoc/solutions/_2022/day05.py)
* Day 06: [Tuning Trouble](https://adventofcode.com/2022/day/6) | [solution](./aoc/solutions/_2022/day06.py)
* Day 07: [No Space Left On Device](https://adventofcode.com/2022/day/7) | [solution](./aoc/solutions/_2022/day07.py)
* Day 08: [Treetop Tree House](https://adventofcode.com/2022/day/8) | [solution](./aoc/solutions/_2022/day08.py)
* Day 09: [Rope Bridge](https://adventofcode.com/2022/day/9) | [solution](./aoc/solutions/_2022/day09.py)
* Day 10: [Cathode-Ray Tube](https://adventofcode.com/2022/day/10) | [solution](./aoc/solutions/_2022/day10.py)
* Day 11: [Monkey in the Middle](https://adventofcode.com/2022/day/11) | [solution](./aoc/solutions/_2022/day11.py)
* Day 12: [Hill Climbing Algorithm](https://adventofcode.com/2022/day/12) | [solution](./aoc/solutions/_2022/day12.py)
* Day 13: [Distress Signal](https://adventofcode.com/2022/day/13) | [solution](./aoc/solutions/_2022/day13.py)
* Day 14: [Regolith Reservoir](https://adventofcode.com/2022/day/14) | [solution](./aoc/solutions/_2022/day14.py)
* Day 15: [Beacon Exclusion Zone](https://adventofcode.com/2022/day/15) | [solution](./aoc/solutions/_2022/day15.py)
* Day 16: [Proboscidea Volcanium](https://adventofcode.com/2022/day/16) | [solution](./aoc/solutions/_2022/day16.py)
* Day 17: [Pyroclastic Flow](https://adventofcode.com/2022/day/17) | [solution](./aoc/solutions/_2022/day17.py)
* Day 18: [Boiling Boulders](https://adventofcode.com/2022/day/18) | [solution](./aoc/solutions/_2022/day18.py)
* Day 19: [Not Enough Minerals](https://adventofcode.com/2022/day/19) | [solution](./aoc/solutions/_2022/day19.py)
* Day 20: [Grove Positioning System](https://adventofcode.com/2022/day/20) | [solution](./aoc/solutions/_2022/day20.py)
* Day 21: [Monkey Math](https://adventofcode.com/2022/day/21) | [solution](./aoc/solutions/_2022/day21.py)
* Day 22: [Monkey Map](https://adventofcode.com/2022/day/22) | [solution](./aoc/solutions/_2022/day22.py)
* Day 23: [Unstable Diffusion](https://adventofcode.com/2022/day/23) | [solution](./aoc/solutions/_2022/day23.py)

</details>

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
session=XXXX
```
