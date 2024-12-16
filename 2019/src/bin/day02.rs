use advent::get_input;

use advent::intcode::*;

type Input = IntCode;

fn search(vm: &mut IntCode) -> Option<(usize, usize)> {
    for n in 0..=99 {
        for v in 0..=99 {
            vm.reset();
            vm.swap(n, v);
            vm.run();
            if vm.result() == 19690720 {
                return Some((n, v));
            }
        }
    }
    None
}

fn solve(input: &mut Input) -> (usize, usize) {
    input.swap(12, 2);
    input.run();
    let part1 = input.result();

    let (n, v) = search(input).unwrap();

    let part2 = 100 * n + v;
    (part1, part2)
}

fn prepare(lines: Vec<String>) -> Input {
    lines
        .first()
        .expect("No IntCode program supplied")
        .parse()
        .expect("Failed to parse IntCode program.")
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut input: Input = get_input(prepare)?;
    let (part1, part2) = solve(&mut input);

    println!("({}, {})", part1, part2);
    Ok(())
}
