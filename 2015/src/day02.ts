import { readFileSync } from "fs";

class Box {
  h: number;
  w: number;
  l: number;

  constructor(input: string) {
    const values = input.split("x").map(Number);
    this.h = values[0];
    this.w = values[1];
    this.l = values[2];
  }
}

function requiredPaper(b: Box): number {
  const side1 = b.l * b.w;
  const side2 = b.w * b.h;
  const side3 = b.h * b.l;
  const extra = Math.min(side1, side2, side3);

  return 2 * side1 + 2 * side2 + 2 * side3 + extra;
}

function volume(b: Box): number {
  return b.h * b.w * b.l;
}

function requiredRibbon(b: Box): number {
  /* Takes 2 shortest sides to calculate wrap length, then adds volume */
  const sides = Object.values(b).sort((a, b) => a - b);
  return 2 * sides[0] + 2 * sides[1] + volume(b);
}

function readInput(input: string): Box[] {
  try {
    const data = readFileSync(input, "utf8");
    return data.split("\n").map((value) => {
      return new Box(value);
    });
  } catch (err) {
    console.log(err);
    return [];
  }
}

export function day02(input: string, verbose: boolean = false): void {
  const boxes = readInput(input);
  console.log("Part 1: " + boxes.map(requiredPaper).reduce((a, b) => a + b, 0));
  console.log(
    "Part 2: " + boxes.map(requiredRibbon).reduce((a, b) => a + b, 0)
  );
}
