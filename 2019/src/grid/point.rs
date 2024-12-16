use std::ops::{Add, Div, Mul, Sub};

/// Point in the grid represented as (x, y) coordinates
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct P {
    pub x: i32,
    pub y: i32,
}

impl P {
    pub fn new(x: i32, y: i32) -> Self {
        P { x, y }
    }

    /// Calculate Manhattan distance to another position
    pub fn manhattan_distance(&self, other: P) -> i32 {
        todo!()
    }

    /// Calculate Chebyshev distance to another position
    pub fn chebyshev_distance(&self, other: P) -> i32 {
        todo!()
    }

    /// Rotate position 90 degrees clockwise around origin
    pub fn rotate_clockwise(&self) -> P {
        todo!()
    }

    /// Rotate position 90 degrees counter-clockwise around origin
    pub fn rotate_counter_clockwise(&self) -> P {
        todo!()
    }

    /// Get the unit vector in this direction (normalized to -1, 0, or 1 for each component)
    pub fn normalize(&self) -> P {
        todo!()
    }
}

// Basic arithmetic operations
impl Add for P {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        todo!()
    }
}

impl Sub for P {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        todo!()
    }
}

// Scalar multiplication
impl Mul<i32> for P {
    type Output = Self;

    fn mul(self, scalar: i32) -> Self {
        todo!()
    }
}

// Scalar division
impl Div<i32> for P {
    type Output = Self;

    fn div(self, scalar: i32) -> Self {
        todo!()
    }
}

// Common direction vectors
impl P {
    pub const ZERO: P = P { x: 0, y: 0 };
    pub const UP: P = P { x: 0, y: -1 };
    pub const DOWN: P = P { x: 0, y: 1 };
    pub const LEFT: P = P { x: -1, y: 0 };
    pub const RIGHT: P = P { x: 1, y: 0 };

    pub const DIRECTIONS_4: [P; 4] = [P::UP, P::RIGHT, P::DOWN, P::LEFT];

    pub const DIRECTIONS_8: [P; 8] = [
        P { x: -1, y: -1 },
        P { x: 0, y: -1 },
        P { x: 1, y: -1 },
        P { x: -1, y: 0 },
        P { x: 1, y: 0 },
        P { x: -1, y: 1 },
        P { x: 0, y: 1 },
        P { x: 1, y: 1 },
    ];
}
