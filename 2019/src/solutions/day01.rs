use crate::solutions::Solution;

pub fn run(puzzle_input: Vec<String>) -> Solution {
    let numbers: Vec<i32> = puzzle_input
        .into_iter()
        .map(|n| n.parse().unwrap())
        .collect();

    let part1: i32 = numbers.iter().map(|n| fuel_required(*n)).sum();
    let part2: i32 = numbers.into_iter().map(|n| recursive_fuel(n, 0)).sum();

    Solution { part1, part2 }
}

fn fuel_required(mass: i32) -> i32 {
    mass / 3 - 2
}

fn recursive_fuel(mass: i32, acc: i32) -> i32 {
    let fuel = fuel_required(mass);
    if fuel <= 0 {
        return acc;
    }
    recursive_fuel(fuel, acc + fuel)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fuel_part1() {
        assert_eq!(2, fuel_required(12));
        assert_eq!(2, fuel_required(14));
        assert_eq!(654, fuel_required(1969));
        assert_eq!(33583, fuel_required(100756));
    }

    #[test]
    fn fuel_part2() {
        assert_eq!(2, recursive_fuel(14, 0));
        assert_eq!(966, recursive_fuel(1969, 0));
        assert_eq!(50346, recursive_fuel(100756, 0));
    }
}
