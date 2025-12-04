use std::fs;

fn is_safe_report(levels: &[i32]) -> bool {
    if levels.len() < 2 {
        return true;
    }

    let increasing = levels[1] > levels[0];

    for i in 0..levels.len() - 1 {
        let diff = levels[i + 1] - levels[i];
        let abs_diff = diff.abs();

        // Check if difference is in valid range [1, 3]
        if abs_diff < 1 || abs_diff > 3 {
            return false;
        }

        // Check if direction is consistent
        if (increasing && diff <= 0) || (!increasing && diff >= 0) {
            return false;
        }
    }

    true
}

fn is_safe_with_dampener(levels: &[i32]) -> bool {
    // First check if already safe
    if is_safe_report(levels) {
        return true;
    }

    // Try removing each level one at a time
    for skip in 0..levels.len() {
        let temp: Vec<i32> = levels
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != skip)
            .map(|(_, &val)| val)
            .collect();

        if is_safe_report(&temp) {
            return true;
        }
    }

    false
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let reports: Vec<Vec<i32>> = input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num| num.parse().unwrap())
                .collect()
        })
        .collect();

    // Part 1: Count safe reports
    let safe_count = reports
        .iter()
        .filter(|report| is_safe_report(report))
        .count();

    println!("Part 1: {}", safe_count);

    // Part 2: Count safe reports with dampener
    let safe_with_dampener = reports
        .iter()
        .filter(|report| is_safe_with_dampener(report))
        .count();

    println!("Part 2: {}", safe_with_dampener);
}
