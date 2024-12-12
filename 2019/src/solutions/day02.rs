use crate::intcode::Intcode;
use crate::solutions::Solution;

pub fn run(puzzle_input: Vec<String>) -> Solution {
    let mut machine = Intcode::new(puzzle_input.first().unwrap()).unwrap();
    let part1 = machine.run(12, 2).unwrap() as i32;
    let part2 = search(&mut machine).unwrap();
    Solution { part1, part2 }
}

fn search(machine: &mut Intcode) -> Option<i32> {
    for noun in 0..100 {
        for verb in 0..100 {
            let result = machine.run(noun, verb).unwrap();
            if result == 19690720 {
                return Some((100 * noun + verb) as i32);
            }
        }
    }
    None
}
