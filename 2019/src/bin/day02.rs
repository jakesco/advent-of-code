use advent::get_input;

use advent::intcode::*;

type Input = IntCode;

fn run_vm(vm: &mut Input, noun: usize, verb: usize) -> usize {
    vm.reset();
    vm.swap(noun, verb);
    vm.run();
    vm.result()
}

fn search(vm: &mut IntCode) -> Option<usize> {
    for n in 0..=99 {
        for v in 0..=99 {
            if run_vm(vm, n, v) == 19690720 {
                return Some(100 * n + v);
            }
        }
    }
    None
}

fn solve(input: &mut Input) -> (usize, usize) {
    let part1 = run_vm(input, 12, 2);
    let part2 = search(input).expect("Failed to find noun/verb pair");
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
