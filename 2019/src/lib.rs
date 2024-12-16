use std::env;
use std::error::Error;
use std::fs;

pub mod intcode;

pub fn get_input<T>(prepare: fn(Vec<String>) -> T) -> Result<T, Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let filepath = args.get(2).map_or("input.txt", |s| s.as_str());

    let contents = fs::read_to_string(filepath)?;

    let lines = contents.lines().map(String::from).collect();

    Ok(prepare(lines))
}
