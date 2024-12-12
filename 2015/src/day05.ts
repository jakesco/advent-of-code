import { readFileSync } from "fs";

type Predicate = (input: string) => boolean;

function threeVowels(input: string): boolean {
  const re = /a|e|i|o|u/g;
  const result = [...input.matchAll(re)];
  return result.length >= 3;
}

function twiceInARow(input: string): boolean {
  // back reference to match to characters in a row
  return input.match(/(.)\1/g) !== null;
}

function noForbiddenStrings(input: string): boolean {
  return input.match(/ab|cd|pq|xy/g) === null;
}

function pairOfTwo(input: string): boolean {
  return input.match(/(..).*\1/g) !== null;
}

function repeatSurround(input: string): boolean {
  return input.match(/(.).\1/g) !== null;
}

function checkString(input: string, criteria: Predicate[]): boolean {
  return criteria.map((p) => p(input)).reduce((a, b) => a && b, true);
}

function countNiceWords(words: string[], criteria: Predicate[]): number {
  return words
    .map((a) => (checkString(a, criteria) ? 1 : 0))
    .reduce((x: number, y: number) => x + y, 0);
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

export function day05(input: string): void {
  const words = readInput(input);

  const conditionsPart1 = [threeVowels, twiceInARow, noForbiddenStrings];
  console.log("Part 1: " + countNiceWords(words, conditionsPart1));

  const conditionsPart2 = [pairOfTwo, repeatSurround];
  console.log("Part 2: " + countNiceWords(words, conditionsPart2));
}
