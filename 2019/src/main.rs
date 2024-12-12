use std::env;
use std::process;

use solutions::Puzzle;

mod grid;
mod intcode;
mod solutions;

fn main() {
    let args: Vec<String> = env::args().collect();

    let puzzle = Puzzle::new(&args).unwrap_or_else(|err| {
        eprintln!("Error parsing arguments: {err}");
        usage();
        process::exit(1);
    });

    match solutions::solve(puzzle) {
        Ok(s) => println!("{}", s),
        Err(e) => {
            eprintln!("Solution error: {e}");
            process::exit(2);
        }
    }
}

fn usage() {
    let usage_string = "\
usage: aoc2019 DAY FILEPATH

Solutions for Advent of Code 2019

positional arguments:
  DAY         Solution day to run, between 1 and 25.
  FILENAME    Path to puzzle input file.";

    println!("{usage_string}");
}
