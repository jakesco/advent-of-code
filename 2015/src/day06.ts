import { readFileSync } from "fs";

class LightGrid {
  h: number;
  w: number;
  state: number[][];

  constructor(height: number, width: number) {
    this.h = height;
    this.w = width;
    this.state = [...Array(height)].map(e => Array(width).fill(0));
  }

  apply1(instruction: Instruction): void {
    for(let i = instruction.region.x0; i <= instruction.region.x1; i++) {
      for(let j = instruction.region.y0; j <= instruction.region.y1; j++) {
        switch (instruction.action) {
          case Action.On:
            this.state[i][j] = 1;
            break;
          case Action.Off:
            this.state[i][j] = 0;
            break;
          case Action.Toggle:
            const current = this.state[i][j]
            if (current === 1) {
              this.state[i][j] = 0
            } else {
              this.state[i][j] = 1
            }
        }
      }
    }
    // console.log(`Applied ${instruction.action} to region (${instruction.region.x0},${instruction.region.y0}) (${instruction.region.x1},${instruction.region.y1}). ${this.lightsLit()} lights lit.`)
  }

  apply2(instruction: Instruction): void {
    for(let i = instruction.region.x0; i <= instruction.region.x1; i++) {
      for(let j = instruction.region.y0; j <= instruction.region.y1; j++) {
        switch (instruction.action) {
          case Action.On:
            this.state[i][j] += 1;
            break;
          case Action.Off:
            if (this.state[i][j] > 0) {
              this.state[i][j] -= 1;
            }
            break;
          case Action.Toggle:
            this.state[i][j] += 2;
        }
      }
    }
  }

  lightsLit(): number {
    return sumArray(this.state.map(sumArray));
  }
}

function sumArray(array: number[]): number {
  return array.reduce((a, b) => a + b, 0);
}

enum Action {
  Toggle = "toggle",
  On = "on",
  Off = "off",
};

class Region {
  readonly x0: number = 0;
  readonly y0: number = 0;
  readonly x1: number = 0;
  readonly y1: number = 0;

  constructor(input: string) {
    const matches = input.match(/[0-9]+,[0-9]+/g);

    if (matches?.length !== 2) {
      console.error("Failed to parse region.");
      return;
    }

    const topLeft = matches[0].split(',').map(Number);
    const bottomRight = matches[1].split(',').map(Number);

    this.x0 = Math.min(topLeft[0], bottomRight[0]);
    this.x1 = Math.max(topLeft[0], bottomRight[0]);
    this.y0 = Math.min(topLeft[1], bottomRight[1]);
    this.y1 = Math.max(topLeft[1], bottomRight[1]);
  }
}

class Instruction {
  readonly action: Action;
  readonly region: Region;

  constructor(input: string) {
    this.action = parseAction(input);
    this.region = new Region(input);
  }
}

function parseAction(input: string): Action {
  // ? is called Optional chaining
  switch (input.match(/on|off|toggle/g)?.pop()) {
    case "on":
      return Action.On;
    case "off":
      return Action.Off;
    case "toggle":
    default:
      return Action.Toggle;
  }
}

function readInput(input: string): string[] {
  try {
    const data = readFileSync(input, "utf8");
    return data.split("\n");
  } catch (err) {
    console.log(err);
    return [""];
  }
}

export function day06(input: string): void {
  const insturctions = readInput(input).map(i => new Instruction(i));

  let grid1 = new LightGrid(1000, 1000);
  let grid2 = new LightGrid(1000, 1000);
  insturctions.forEach(i => {
    grid1.apply1(i);
    grid2.apply2(i);
  });
  console.log("Part 1: " + grid1.lightsLit());
  console.log("Part 2: " + grid2.lightsLit());
}
