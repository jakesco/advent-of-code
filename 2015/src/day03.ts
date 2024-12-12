import { readFileSync } from "fs";

interface Point {
  x: number;
  y: number;
}

function part1(directions: string): number {
  let location: Point = { x: 0, y: 0 };
  let visited: Set<string> = new Set();
  // Using stringify to compare objects by value in Set.
  visited.add(JSON.stringify({ x: 0, y: 0 }));
  for (let i = 0; i < directions.length; i++) {
    switch (directions[i]) {
      case ">":
        location.x += 1;
        break;
      case "<":
        location.x -= 1;
        break;
      case "^":
        location.y += 1;
        break;
      case "v":
        location.y -= 1;
        break;
    }
    visited.add(JSON.stringify({ x: location.x, y: location.y }));
  }
  return visited.size;
}

function part2(directions: string): number {
  let location: Point[] = [
    { x: 0, y: 0 }, // santa
    { x: 0, y: 0 }, // roboSanta
  ];

  let visited: Set<string> = new Set();
  visited.add(JSON.stringify({ x: 0, y: 0 }));

  for (let i = 0; i < directions.length; i++) {
    switch (directions[i]) {
      case ">":
        location[i % 2].x += 1;
        break;
      case "<":
        location[i % 2].x -= 1;
        break;
      case "^":
        location[i % 2].y += 1;
        break;
      case "v":
        location[i % 2].y -= 1;
        break;
    }
    visited.add(JSON.stringify({ x: location[i % 2].x, y: location[i % 2].y }));
  }
  return visited.size;
}

function readInput(input: string): string {
  try {
    const data = readFileSync(input, "utf8");
    return data;
  } catch (err) {
    console.log(err);
    return "";
  }
}

export function day03(input: string): void {
  const directions = readInput(input);
  console.log("Part 1: " + part1(directions));
  console.log("Part 2: " + part2(directions));
}
