use std::collections::HashSet;
use std::fs;
use std::path::Path;

fn find_marker(data: &str, window_size: usize) -> usize {
    let bytes = data.as_bytes();
    for i in window_size..=bytes.len() {
        let window = &bytes[i - window_size..i];
        let unique: HashSet<&u8> = window.iter().collect();
        if unique.len() == window_size {
            return i;
        }
    }
    0
}

fn part1(data: &str) -> usize {
    find_marker(data, 4)
}

fn part2(data: &str) -> usize {
    find_marker(data, 14)
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    let data = fs::read_to_string(&input_path)
        .or_else(|_| fs::read_to_string(Path::new("../input.txt")))
        .expect("Failed to read input file");
    let data = data.trim();

    println!("Part 1: {}", part1(data));
    println!("Part 2: {}", part2(data));
}
