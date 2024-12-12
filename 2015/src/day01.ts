import { readFileSync } from "fs";

function readInput(input: string): string {
  try {
    const data = readFileSync(input, "utf8");
    return data;
  } catch (err) {
    console.log(err);
    return "";
  }
}

function part1(directions: string): number {
  let floor = 0;
  for (let i = 0; i < directions.length; i++) {
    switch (directions[i]) {
      case "(":
        floor += 1;
        break;
      case ")":
        floor -= 1;
        break;
    }
  }

  return floor;
}

function part2(directions: string): number {
  let floor = 0;
  for (let i = 0; i < directions.length; i++) {
    switch (directions[i]) {
      case "(":
        floor += 1;
        break;
      case ")":
        floor -= 1;
        break;
    }

    if (floor < 0) {
      return i + 1;
    }
  }
  return directions.length;
}

export function day01(input: string, verbose: boolean = false) {
  const directions = readInput(input);
  console.log("Part 1: " + part1(directions));
  console.log("Part 2: " + part2(directions));
}
