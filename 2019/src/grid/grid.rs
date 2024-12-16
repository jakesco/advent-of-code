use crate::P;
use std::collections::HashMap;

/// Grid represented as width, height, and a map from positions to values
#[derive(Debug, Clone, PartialEq)]
pub struct Grid<T> {
    width: i32,
    height: i32,
    cells: HashMap<P, T>,
}

impl<T> Grid<T> {
    /// Create an empty grid of given dimensions
    pub fn empty(width: i32, height: i32) -> Self {
        todo!()
    }

    /// Create a grid from a vector of vectors
    pub fn from_vec(data: Vec<Vec<T>>) -> Self
    where
        T: Clone,
    {
        todo!()
    }

    /// Convert grid to vector of vectors, with Options for missing values
    pub fn to_vec(&self) -> Vec<Vec<Option<T>>>
    where
        T: Clone,
    {
        todo!()
    }

    /// Get a vector of all cell values
    pub fn get_all_vals(&self) -> Vec<&T> {
        todo!()
    }

    /// Check if position is within grid bounds
    pub fn in_bounds(&self, pos: P) -> bool {
        todo!()
    }

    /// Get grid dimensions
    pub fn dimensions(&self) -> (i32, i32) {
        todo!()
    }

    /// Get value at position
    pub fn get(&self, pos: P) -> Option<&T> {
        todo!()
    }

    /// Get value at position, return default if not found
    pub fn get_with_default(&self, default: T, pos: P) -> T
    where
        T: Clone,
    {
        todo!()
    }

    /// Set value at position
    pub fn set(&mut self, pos: P, value: T) -> bool {
        todo!()
    }

    /// Update value at position using function
    pub fn update<F>(&mut self, pos: P, f: F) -> bool
    where
        F: FnOnce(&T) -> T,
    {
        todo!()
    }

    /// Get all eight surrounding positions
    pub fn neighbors(&self, pos: P) -> Vec<P> {
        todo!()
    }

    /// Get only cardinal (non-diagonal) neighbors
    pub fn cardinal_neighbors(&self, pos: P) -> Vec<P> {
        todo!()
    }

    /// Get only diagonal neighbors
    pub fn diagonal_neighbors(&self, pos: P) -> Vec<P> {
        todo!()
    }

    /// Get all eight surrounding values
    pub fn neighbor_vals(&self, pos: P) -> Vec<&T> {
        todo!()
    }

    /// Get only cardinal (non-diagonal) neighbor values
    pub fn cardinal_neighbor_vals(&self, pos: P) -> Vec<&T> {
        todo!()
    }

    /// Get only diagonal neighbor values
    pub fn diagonal_neighbor_vals(&self, pos: P) -> Vec<&T> {
        todo!()
    }

    /// Map a function over all points in Grid
    pub fn map<F, U>(&self, f: F) -> Grid<U>
    where
        F: Fn(&T) -> U,
    {
        todo!()
    }

    /// Map a function that also takes position as input
    pub fn map_with_position<F, U>(&self, f: F) -> Grid<U>
    where
        F: Fn(P, &T) -> U,
    {
        todo!()
    }

    /// Find first position of a value satisfying predicate
    pub fn find_position<F>(&self, pred: F) -> Option<P>
    where
        F: Fn(&T) -> bool,
    {
        todo!()
    }

    /// Find all positions of values satisfying predicate
    pub fn find_all<F>(&self, pred: F) -> Vec<P>
    where
        F: Fn(&T) -> bool,
    {
        todo!()
    }
}
