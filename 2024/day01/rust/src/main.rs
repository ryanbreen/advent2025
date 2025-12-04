use std::fs;
use std::collections::HashMap;

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input.txt");

    let mut left = Vec::new();
    let mut right = Vec::new();

    // Parse input
    for line in input.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() == 2 {
            left.push(parts[0].parse::<i32>().unwrap());
            right.push(parts[1].parse::<i32>().unwrap());
        }
    }

    // Part 1: Sort both lists and calculate total distance
    let mut left_sorted = left.clone();
    let mut right_sorted = right.clone();

    left_sorted.sort_unstable();
    right_sorted.sort_unstable();

    let part1_sum: i64 = left_sorted.iter()
        .zip(right_sorted.iter())
        .map(|(l, r)| (l - r).abs() as i64)
        .sum();

    // Part 2: Calculate similarity score
    // Build frequency map for right list
    let mut right_freq = HashMap::new();
    for &num in &right {
        *right_freq.entry(num).or_insert(0) += 1;
    }

    let part2_sum: i64 = left.iter()
        .map(|&num| num as i64 * *right_freq.get(&num).unwrap_or(&0) as i64)
        .sum();

    println!("Part 1: {}", part1_sum);
    println!("Part 2: {}", part2_sum);
}
