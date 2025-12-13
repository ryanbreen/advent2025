use std::collections::HashMap;
use std::fs;

/// Count how many stones result from a single stone after N blinks using memoization
fn count_stones(value: u64, blinks: u32, memo: &mut HashMap<(u64, u32), u64>) -> u64 {
    // Base case: no more blinks
    if blinks == 0 {
        return 1;
    }

    // Check memo
    let key = (value, blinks);
    if let Some(&result) = memo.get(&key) {
        return result;
    }

    let result = if value == 0 {
        // Rule 1: 0 becomes 1
        count_stones(1, blinks - 1, memo)
    } else {
        // Check if number has even number of digits using mathematical approach
        // Number of digits = floor(log10(value)) + 1
        let num_digits = value.checked_ilog10().unwrap_or(0) + 1;

        if num_digits % 2 == 0 {
            // Rule 2: Even number of digits -> split
            // Calculate divisor: 10^(num_digits/2)
            let divisor = 10_u64.pow(num_digits / 2);
            let left = value / divisor;
            let right = value % divisor;
            count_stones(left, blinks - 1, memo) + count_stones(right, blinks - 1, memo)
        } else {
            // Rule 3: Multiply by 2024
            count_stones(value * 2024, blinks - 1, memo)
        }
    };

    // Store in memo
    memo.insert(key, result);
    result
}

fn part1(stones: &[u64], memo: &mut HashMap<(u64, u32), u64>) -> u64 {
    stones.iter()
        .map(|&stone| count_stones(stone, 25, memo))
        .sum()
}

fn part2(stones: &[u64], memo: &mut HashMap<(u64, u32), u64>) -> u64 {
    stones.iter()
        .map(|&stone| count_stones(stone, 75, memo))
        .sum()
}

fn main() {
    // Read input file
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    // Parse space-separated numbers
    let stones: Vec<u64> = input
        .trim()
        .split_whitespace()
        .map(|s| s.parse().expect("Failed to parse number"))
        .collect();

    // Share memo between both parts for maximum memoization efficiency
    let mut memo = HashMap::new();
    println!("Part 1: {}", part1(&stones, &mut memo));
    println!("Part 2: {}", part2(&stones, &mut memo));
}
