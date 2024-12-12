/*
  For example, here is a simple circuit:

  123 -> x
  456 -> y
  x AND y -> d
  x OR y -> e
  x LSHIFT 2 -> f
  y RSHIFT 2 -> g
  NOT x -> h
  NOT y -> i

  After it is run, these are the signals on the wires:

  d: 72
  e: 507
  f: 492
  g: 114
  h: 65412
  i: 65079
  x: 123
  y: 456
*/

import { readFileSync } from "fs";

const AssignExpr = /^(\d+) -> ([a-z]+)$/
const AndExpr = /^([a-z]+) AND ([a-z]+) -> ([a-z]+)$/
const OrExpr = /^([a-z]+) OR ([a-z]+) -> ([a-z]+)$/
const LShiftExpr = /^([a-z]+) LSHIFT (\d+) -> ([a-z]+)$/
const RShiftExpr = /^([a-z]+) RSHIFT (\d+) -> ([a-z]+)$/
const NotExpr = /^NOT ([a-z]+) -> ([a-z]+)$/

enum Gate {
  AND,
  OR,
  LSHIFT,
  RSHIFT,
  NOT,
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

export function day07(input: string): void {
  const insturctions = readInput(input);


  console.log("Part 1: ");
  console.log("Part 2: ");
}
