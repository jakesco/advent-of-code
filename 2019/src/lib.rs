use std::env;
use std::fs;

pub mod intcode;

/// Read puzzle input from given cli argument (defaults to input.txt).
/// Applies given prepare function to file contents.
pub fn get_input<T>(prepare: fn(String) -> T) -> Result<T, Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let filepath = args.get(2).map_or("input.txt", |s| s.as_str());
    let contents = fs::read_to_string(filepath)?;
    let result = prepare(contents);
    Ok(result)
}
