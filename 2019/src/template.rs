use advent::get_input;

type Input = Vec<String>;

fn solve(_input: &Input) -> (i32, i32) {
    let part1 = 0;
    let part2 = 0;
    (part1, part2)
}

fn prepare(lines: Vec<String>) -> Input {
    lines
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

    macro_rules! input {
        ($raw:expr) => {
            $raw.lines().map(String::from).collect::<Vec<_>>()
        };
    }

    const EXAMPLE: &str = "\
first line
second line";

    #[test]
    fn test_example() {
        let input = prepare(input!(EXAMPLE));
        let (part1, part2) = solve(&input);
        assert_eq!(part1, 0);
        assert_eq!(part2, 0);
    }
}
