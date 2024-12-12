pub struct Intcode {
    tape: Vec<usize>,
    memory: Vec<usize>,
    size: usize,
}

impl Intcode {
    pub fn new(tape: &String) -> Result<Intcode, &'static str> {
        let tape: Vec<usize> = tape
            .split(",")
            .map(|n| n.parse::<usize>().unwrap())
            .collect();
        let memory = tape.clone();
        let size = tape.len();

        Ok(Intcode { tape, memory, size })
    }

    pub fn run(&mut self, a: usize, b: usize) -> Result<usize, &'static str> {
        self.reset();
        let mut ptr = 0;

        self.memory[1] = a;
        self.memory[2] = b;

        while ptr < self.size {
            match self.memory[ptr] {
                1 => {
                    if let [x, y, z] = self.memory[ptr + 1..=ptr + 3] {
                        self.memory[z] = self.memory[x] + self.memory[y];
                        ptr += 4;
                    } else {
                        return Err("intcode error: op failed");
                    }
                }
                2 => {
                    if let [x, y, z] = self.memory[ptr + 1..=ptr + 3] {
                        self.memory[z] = self.memory[x] * self.memory[y];
                        ptr += 4;
                    } else {
                        return Err("intcode error: op failed");
                    }
                }
                99 => return Ok(self.memory[0]),
                _ => return Err("intcode error: unknown opcode"),
            }
        }

        Err("intcode error: out of memory")
    }

    fn reset(&mut self) {
        self.memory = self.tape.clone();
    }

    #[allow(dead_code)]
    pub fn peek(&self, idx: usize) -> Option<usize> {
        self.memory.get(idx).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_intcode() {
        let program = String::from("1,9,10,3,2,3,11,0,99,30,40,50");
        let mut machine = Intcode::new(&program).unwrap();
        let result = machine.run(9, 10).unwrap();
        assert_eq!(3500, result);
    }

    #[test]
    fn intcode_example_1() {
        let program = String::from("1,0,0,0,99");
        let mut machine = Intcode::new(&program).unwrap();
        let result = machine.run(0, 0).unwrap();
        assert_eq!(2, result);
    }

    #[test]
    fn intcode_example_2() {
        let program = String::from("2,3,0,3,99");
        let mut machine = Intcode::new(&program).unwrap();
        machine.run(3, 0).unwrap();
        assert_eq!(6, machine.peek(3).unwrap());
    }

    #[test]
    fn intcode_example_3() {
        let program = String::from("2,4,4,5,99,0");
        let mut machine = Intcode::new(&program).unwrap();
        machine.run(4, 4).unwrap();
        assert_eq!(9801, machine.peek(5).unwrap());
    }

    #[test]
    fn intcode_example_4() {
        let program = String::from("1,1,1,4,99,5,6,0,99");
        let mut machine = Intcode::new(&program).unwrap();
        let result = machine.run(1, 1).unwrap();
        assert_eq!(30, result);
    }
}
