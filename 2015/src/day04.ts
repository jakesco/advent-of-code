import { readFileSync } from "fs";
// TODO: remove this dep way be a fun exercise in the future.
const md5 = require("blueimp-md5/js/md5");

const MAX_RUNS = 1_000_000_000;

function md5StartsWith(key: string, match: string = "00000"): number {
  let i = 1;
  while (i < MAX_RUNS) {
    let hash = md5(key + i);
    if (hash.startsWith(match)) {
      return i;
    }
    i++;
  }
  return i;
}

function readInput(input: string): string {
  try {
    return readFileSync(input, "utf8").trim();
  } catch (err) {
    console.log(err);
    return "";
  }
}

export function day04(input: string): void {
  const key = readInput(input);
  console.log("Part 1: " + md5StartsWith(key));
  console.log("Part 2: " + md5StartsWith(key, "000000"));
}
