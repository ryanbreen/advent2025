use std::fs;
use std::path::Path;

fn parse_input(filename: &Path) -> Vec<(u32, u32, u32, u32)> {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    content
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let parts: Vec<&str> = line.split(',').collect();
            let left: Vec<u32> = parts[0].split('-').map(|x| x.parse().unwrap()).collect();
            let right: Vec<u32> = parts[1].split('-').map(|x| x.parse().unwrap()).collect();
            (left[0], left[1], right[0], right[1])
        })
        .collect()
}

fn fully_contains(a1: u32, b1: u32, a2: u32, b2: u32) -> bool {
    // Check if one range fully contains the other
    (a1 <= a2 && b1 >= b2) || (a2 <= a1 && b2 >= b1)
}

fn overlaps(a1: u32, b1: u32, a2: u32, b2: u32) -> bool {
    // Check if ranges overlap at all
    a1 <= b2 && a2 <= b1
}

fn part1(pairs: &[(u32, u32, u32, u32)]) -> usize {
    pairs
        .iter()
        .filter(|&&(a1, b1, a2, b2)| fully_contains(a1, b1, a2, b2))
        .count()
}

fn part2(pairs: &[(u32, u32, u32, u32)]) -> usize {
    pairs
        .iter()
        .filter(|&&(a1, b1, a2, b2)| overlaps(a1, b1, a2, b2))
        .count()
}

fn main() {
    let exe_path = std::env::current_exe().expect("Failed to get executable path");
    let exe_dir = exe_path.parent().expect("Failed to get executable directory");
    let input_path = exe_dir.join("../input.txt");

    // Try executable-relative path first, fallback to cwd-relative
    let input_file = if input_path.exists() {
        input_path
    } else {
        Path::new("../input.txt").to_path_buf()
    };

    let pairs = parse_input(&input_file);

    println!("Part 1: {}", part1(&pairs));
    println!("Part 2: {}", part2(&pairs));
}
