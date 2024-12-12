use std::error::Error;
use std::fmt;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::Path;

mod day01;
mod day02;
mod day03;

pub struct Puzzle {
    pub day: u8,
    pub filepath: String,
}

impl Puzzle {
    pub fn new(args: &[String]) -> Result<Puzzle, &'static str> {
        if args.len() < 3 {
            return Err("not enough arguments");
        }

        let day = args[1].parse::<u8>().unwrap();
        if day < 1 || day > 25 {
            return Err("invalid day, must be between 1 and 25");
        }

        let filepath = args[2].clone();
        if !fs::metadata(&filepath).is_ok() {
            return Err("input file does not exist");
        }

        Ok(Puzzle { day, filepath })
    }
}

pub struct Solution {
    part1: i32,
    part2: i32,
}

impl fmt::Display for Solution {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Part 1: {}\nPart 2: {}", self.part1, self.part2)
    }
}

pub fn solve(config: Puzzle) -> Result<Solution, Box<dyn Error>> {
    let lines: Vec<String> = collect_lines(config.filepath);

    let solution = match config.day {
        1 => day01::run(lines),
        2 => day02::run(lines),
        3 => day03::run(lines),
        _ => panic!("solution not implemented"),
    };

    Ok(solution)
}

fn collect_lines(filepath: impl AsRef<Path>) -> Vec<String> {
    let f = fs::File::open(filepath).expect("Could not parse file");
    let reader = BufReader::new(f);
    reader
        .lines()
        .map(|line| line.expect("Could not parse input"))
        .collect()
}
