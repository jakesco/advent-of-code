use std::str::FromStr;

use std::collections::HashSet;
use crate::solutions::Solution;
use crate::grid::P;

#[derive(Debug, PartialEq)]
enum Direction {
    Left,
    Right,
    Down,
    Up,
}

#[derive(Debug, PartialEq)]
struct Move {
    distance: u32,
    direction: Direction,
}

#[derive(Debug)]
struct ParseMoveError;

impl FromStr for Move {
    type Err = ParseMoveError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let direction = match s.chars().nth(0) {
            Some('L') => Direction::Left,
            Some('R') => Direction::Right,
            Some('D') => Direction::Down,
            Some('U') => Direction::Up,
            _ => return Err(ParseMoveError),
        };

        let distance: u32 = match s[1..].parse() {
            Err(_) => return Err(ParseMoveError),
            Ok(n) => n,
        };

        Ok(Move {
            distance,
            direction,
        })
    }
}

pub fn run(puzzle_input: Vec<String>) -> Solution {
    for line in puzzle_input.iter() {
        println!("{:?}", parse_path(line));
    }

    Solution { part1: 0, part2: 0 }
}

fn parse_path(line: &String) -> HashSet<P> {
    let moves: Vec<Move> = line
        .split(",")
        .map(|m| m.parse::<Move>().expect("fail to parse move"))
        .collect();

    let mut start = P::zero();
    let mut set = HashSet::new();
    for move_ in moves {
        add_path(move_, &mut start, &mut set);
    }
    set
}

fn add_path(move_: Move, start: &mut P, set: &mut HashSet<P>) {
    let dist = move_.distance as i32;
    match move_.direction {
        Direction::Left => set.extend((start.x..dist).map(|n| P::new(start.x - (n + 1), start.y))),
        Direction::Right => set.extend((start.x..dist).map(|n| P::new(start.x + (n + 1), start.y))),
        Direction::Up => set.extend((start.y..dist).map(|n| P::new(start.x, start.y + (n + 1)))),
        Direction::Down => set.extend((start.y..dist).map(|n| P::new(start.x, start.y - (n + 1)))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn move_string_parse() {
        let input = String::from("U123");
        let expected = Move {
            direction: Direction::Up,
            distance: 123,
        };
        let actual: Move = input[..].parse().unwrap();
        assert_eq!(expected, actual);

        let input = String::from("R8");
        let expected = Move {
            direction: Direction::Right,
            distance: 8,
        };
        let actual: Move = input[..].parse().unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn move_string_fail_parse() {
        let input = String::from("UD123");
        assert!(input[..].parse::<Move>().is_err());

        let input = String::from("123");
        assert!(input[..].parse::<Move>().is_err());
    }

    #[test]
    fn parse_path_test() {
        let input = String::from("R2,U1");
        let path = parse_path(&input);
        let mut expected = HashSet::new();
        expected.insert(P::new(1, 0));
        expected.insert(P::new(2, 0));
        expected.insert(P::new(0, 1));
        println!("{:?}", path);
        assert!(expected.is_subset(&path) && expected.is_superset(&path))
    }
}
