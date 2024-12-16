#[derive(Debug)]
pub struct IntCode {
    tape: Vec<usize>,
    memory: Vec<usize>,
    ptr: usize,
    halt: bool,
}

impl IntCode {
    pub fn new(tape: Vec<usize>) -> Self {
        let memory = tape.clone();
        IntCode {
            tape,
            memory,
            ptr: 0,
            halt: false,
        }
    }

    pub fn step(&mut self) {
        match self.memory[self.ptr] {
            1 => {
                let a = self.memory[self.ptr + 1];
                let b = self.memory[self.ptr + 2];
                let c = self.memory[self.ptr + 3];
                self.memory[c] = self.memory[a] + self.memory[b];
                self.ptr += 4;
            }
            2 => {
                let a = self.memory[self.ptr + 1];
                let b = self.memory[self.ptr + 2];
                let c = self.memory[self.ptr + 3];
                self.memory[c] = self.memory[a] * self.memory[b];
                self.ptr += 4;
            }
            99 => self.halt = true,
            _ => panic!("Unknown optcode."),
        }
    }

    pub fn run(&mut self) {
        while !self.halt {
            self.step()
        }
    }

    /// Swaps positon 1 and 2 in memory with given values
    pub fn swap(&mut self, noun: usize, verb: usize) {
        self.memory[1] = noun;
        self.memory[2] = verb;
    }

    pub fn reset(&mut self) {
        self.memory = self.tape.clone();
        self.ptr = 0;
        self.halt = false;
    }

    pub fn result(&self) -> usize {
        self.memory[0]
    }
}

impl std::str::FromStr for IntCode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tape: Vec<usize> = s.split(",").filter_map(|s| s.parse().ok()).collect();
        if tape.len() == 0 {
            return Err(String::from("Faild to parse input."));
        }
        let memory = tape.clone();
        Ok(IntCode {
            tape,
            memory,
            ptr: 0,
            halt: false,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intcode_parsing() {
        let program = "1,0,0,3,99";
        let ic: IntCode = program.parse().expect("Failed to Parse IntCode tape.");
        assert_eq!(ic.tape, [1, 0, 0, 3, 99])
    }

    #[test]
    fn test_add() {
        let mut ic = IntCode::new(vec![1, 0, 0, 3, 99]);
        ic.step();
        assert_eq!(ic.memory[3], 2);
        assert_eq!(ic.ptr, 4);
    }

    #[test]
    fn test_mul() {
        let mut ic = IntCode::new(vec![2, 1, 2, 3, 99]);
        ic.step();
        assert_eq!(ic.memory[3], 2);
        assert_eq!(ic.ptr, 4);
    }

    #[test]
    fn test_halt() {
        let mut ic = IntCode::new(vec![99]);
        ic.step();
        assert!(ic.halt);
    }

    #[test]
    fn test_reset() {
        let mut ic = IntCode::new(vec![1, 0, 0, 3, 99]);
        ic.run();
        ic.reset();
        assert_eq!(ic.memory, vec![1, 0, 0, 3, 99]);
        assert_eq!(ic.ptr, 0);
        assert!(!ic.halt)
    }

    #[test]
    fn test_program_1() {
        let mut ic = IntCode::new(vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]);
        ic.run();
        assert_eq!(
            ic.memory,
            vec![3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );
    }

    #[test]
    fn test_program_2() {
        let mut ic = IntCode::new(vec![1, 0, 0, 0, 99]);
        ic.run();
        assert_eq!(ic.memory, vec![2, 0, 0, 0, 99]);
    }

    #[test]
    fn test_program_3() {
        let mut ic = IntCode::new(vec![2, 3, 0, 3, 99]);
        ic.run();
        assert_eq!(ic.memory, vec![2, 3, 0, 6, 99]);
    }

    #[test]
    fn test_program_4() {
        let mut ic = IntCode::new(vec![2, 4, 4, 5, 99, 0]);
        ic.run();
        assert_eq!(ic.memory, vec![2, 4, 4, 5, 99, 9801]);
    }

    #[test]
    fn test_program_5() {
        let mut ic = IntCode::new(vec![1, 1, 1, 4, 99, 5, 6, 0, 99]);
        ic.run();
        assert_eq!(ic.memory, vec![30, 1, 1, 4, 2, 5, 6, 0, 99]);
    }
}
