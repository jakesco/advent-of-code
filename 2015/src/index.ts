#!/usr/bin/env node

import { program } from "commander";

import { day01 } from "./day01";
import { day02 } from "./day02";
import { day03 } from "./day03";
import { day04 } from "./day04";
import { day05 } from "./day05";
import { day06 } from "./day06";
import { day07 } from "./day07";

const solutions = [
  () => undefined, // This array starts at 1...
  day01,
  day02,
  day03,
  day04,
  day05,
  day06,
  day07,
];

program
  .name("aoc")
  .description("Advent of code 2015 solutions")
  .version("1.0.0")
  .argument("day", "solution day")
  .argument("input", "input file")
  .action((day, input) => {
    if (day >= 1 && day <= 25) {
      solutions[day](input);
    } else {
      console.error("Day must be between 1 and 25.");
    }
  });

program.parse();
