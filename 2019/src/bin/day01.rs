use advent::get_input;

type Input = Vec<i32>;

fn fuel(mass: i32) -> i32 {
    mass / 3 - 2
}

fn recursive_fuel(mass: i32) -> i32 {
    let m = fuel(mass);
    if m <= 0 {
        0
    } else {
        m + recursive_fuel(m)
    }
}

fn sum_fold(input: &Input, f: fn(i32) -> i32) -> i32 {
    input.iter().copied().fold(0, |acc, x| acc + f(x))
}

fn solve(input: &Input) -> (i32, i32) {
    let part1 = sum_fold(input, fuel);
    let part2 = sum_fold(input, recursive_fuel);
    (part1, part2)
}

fn prepare(lines: Vec<String>) -> Input {
    lines
        .into_iter()
        .filter_map(|line| line.parse().ok())
        .collect()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input: Input = get_input(prepare)?;
    let (part1, part2) = solve(&input);

    println!("({}, {})", part1, part2);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuel_calc() {
        assert_eq!(fuel(12), 2);
        assert_eq!(fuel(14), 2);
        assert_eq!(fuel(1969), 654);
        assert_eq!(fuel(100756), 33583);
    }

    #[test]
    fn test_recursive_fuel_calc() {
        assert_eq!(recursive_fuel(14), 2);
        assert_eq!(recursive_fuel(1969), 966);
        assert_eq!(recursive_fuel(100756), 50346);
    }
}
