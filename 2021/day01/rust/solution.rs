use std::fs;
use std::path::Path;

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let input = fs::read_to_string(&input_path)
        .expect("Failed to read input file");

    let depths: Vec<i32> = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.parse().expect("Failed to parse number"))
        .collect();

    println!("Part 1: {}", part1(&depths));
    println!("Part 2: {}", part2(&depths));
}

/// Count the number of times a depth measurement increases from the previous.
fn part1(depths: &[i32]) -> usize {
    depths.windows(2)
        .filter(|w| w[1] > w[0])
        .count()
}

/// Count increases in 3-measurement sliding window sums.
fn part2(depths: &[i32]) -> usize {
    // Create sliding window sums of 3 consecutive measurements
    let window_sums: Vec<i32> = depths.windows(3)
        .map(|w| w[0] + w[1] + w[2])
        .collect();

    // Count how many times the sum increases
    window_sums.windows(2)
        .filter(|w| w[1] > w[0])
        .count()
}
