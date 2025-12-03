use std::fs;

fn part1(lines: &[String]) -> i32 {
    let mut total = 0;

    for line in lines {
        let n = line.len();

        // Precompute max suffix: max_suffix[i] = max digit from position i to end
        let mut max_suffix = vec![0u8; n];
        max_suffix[n - 1] = line.chars().nth(n - 1).unwrap().to_digit(10).unwrap() as u8;

        for i in (0..n - 1).rev() {
            let digit = line.chars().nth(i).unwrap().to_digit(10).unwrap() as u8;
            max_suffix[i] = max_suffix[i + 1].max(digit);
        }

        let mut max_joltage = 0;

        // For each possible first battery position
        for i in 0..n - 1 {
            let first_digit = line.chars().nth(i).unwrap().to_digit(10).unwrap() as i32;
            // The maximum second digit is the max from position i+1 onwards
            let max_second = max_suffix[i + 1] as i32;
            let joltage = first_digit * 10 + max_second;
            max_joltage = max_joltage.max(joltage);
        }

        total += max_joltage;
    }

    total
}

fn part2(lines: &[String]) -> u64 {
    let mut total: u64 = 0;

    for line in lines {
        let n = line.len();
        let k = 12; // Select exactly 12 batteries
        let chars: Vec<char> = line.chars().collect();

        // Greedy algorithm to select k digits that form the maximum number
        let mut result = Vec::new();
        let mut current_pos = 0;

        for i in 0..k {
            // How many digits we still need to select after this one
            let remaining_needed = k - i - 1;
            // Latest position we can start searching from
            let search_end = n - remaining_needed;

            // Find the maximum digit in the valid range
            let mut max_digit = -1i32;
            let mut max_pos = current_pos;

            for j in current_pos..search_end {
                let digit = chars[j].to_digit(10).unwrap() as i32;
                if digit > max_digit {
                    max_digit = digit;
                    max_pos = j;
                }
            }

            result.push(max_digit as u8);
            current_pos = max_pos + 1;
        }

        // Convert digits to u64
        let mut joltage: u64 = 0;
        for digit in result {
            joltage = joltage * 10 + digit as u64;
        }

        total += joltage;
    }

    total
}

fn main() {
    // Read input from ../input.txt (relative to the rust directory)
    let input_text = fs::read_to_string("../input.txt")
        .expect("Failed to read input.txt");

    let lines: Vec<String> = input_text
        .trim()
        .lines()
        .map(|s| s.to_string())
        .collect();

    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
