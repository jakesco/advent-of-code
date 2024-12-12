use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct P {
    pub x: i32,
    pub y: i32,
}

impl P {
    pub fn new(x: i32, y: i32) -> P {
        P { x , y }
    }

    pub fn zero() -> P {
        P::new(0, 0)
    }

    pub fn manhattan_distance(&self, other: &P) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

pub struct Grid(HashSet<P>);

impl Grid {
    pub fn new() -> Grid {
        Grid { 0: HashSet::new() }
    }

    pub fn add(&mut self, point: P) {
        self.0.insert(point);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manhattan_distance() {
        let a = P { x: 0, y: 0 };
        let b = P { x: 1, y: 1 };
        assert_eq!(2, a.manhattan_distance(&b));
    }

    #[test]
    fn make_grid() {
        let mut g = Grid::new();
        g.add(P { x: 1, y: 1 });
    }
}
