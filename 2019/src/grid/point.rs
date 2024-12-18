use std::cmp::max;
use std::ops::{Add, AddAssign, Div, Mul, MulAssign, Sub};

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
        (other.x - self.x).abs() + (other.y - self.y).abs()
    }

    /// Calculate Chebyshev distance to another position
    pub fn chebyshev_distance(&self, other: P) -> i32 {
        max((other.x - self.x).abs(), (other.y - self.y).abs())
    }

    /// Rotate position 90 degrees clockwise around origin
    pub fn rotate_clockwise(&self) -> P {
        P {
            x: -self.y,
            y: self.x,
        }
    }

    /// Rotate position 90 degrees counter-clockwise around origin
    pub fn rotate_counter_clockwise(&self) -> P {
        P {
            x: self.y,
            y: -self.x,
        }
    }
}

// Basic arithmetic operations
impl Add for P {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl AddAssign for P {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for P {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

// Scalar multiplication
impl Mul<i32> for P {
    type Output = Self;

    fn mul(self, scalar: i32) -> Self {
        Self {
            x: self.x * scalar,
            y: self.y * scalar,
        }
    }
}

impl MulAssign<i32> for P {
    fn mul_assign(&mut self, scalar: i32) {
        *self = Self {
            x: self.x * scalar,
            y: self.y * scalar,
        }
    }
}

// Scalar division
impl Div<i32> for P {
    type Output = Self;

    fn div(self, scalar: i32) -> Self {
        Self {
            x: self.x / scalar,
            y: self.y / scalar,
        }
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
