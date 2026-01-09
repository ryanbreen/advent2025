use std::fs;
use std::path::Path;

/// Convert a SNAFU digit character to its decimal value.
fn snafu_digit_to_value(c: char) -> i64 {
    match c {
        '2' => 2,
        '1' => 1,
        '0' => 0,
        '-' => -1,
        '=' => -2,
        _ => panic!("Invalid SNAFU digit: {}", c),
    }
}

/// Convert a SNAFU string to decimal.
fn snafu_to_decimal(s: &str) -> i64 {
    s.chars().fold(0, |acc, c| acc * 5 + snafu_digit_to_value(c))
}

/// Convert a decimal number to SNAFU string.
fn decimal_to_snafu(mut n: i64) -> String {
    if n == 0 {
        return "0".to_string();
    }

    let mut digits = Vec::new();
    while n != 0 {
        let remainder = n % 5;
        match remainder {
            0 | 1 | 2 => {
                digits.push(char::from_digit(remainder as u32, 10).unwrap());
                n /= 5;
            }
            3 => {
                digits.push('=');
                n = n / 5 + 1;
            }
            4 => {
                digits.push('-');
                n = n / 5 + 1;
            }
            _ => unreachable!(),
        }
    }

    digits.iter().rev().collect()
}

/// Part 1: Sum all SNAFU numbers and return result as SNAFU.
fn part1(text: &str) -> String {
    let total: i64 = text
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| snafu_to_decimal(line.trim()))
        .sum();
    decimal_to_snafu(total)
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    // Try relative to executable first, then try relative to current directory
    let text = if input_path.exists() {
        fs::read_to_string(&input_path).expect("Failed to read input file")
    } else {
        let alt_path = Path::new("../input.txt");
        fs::read_to_string(alt_path).expect("Failed to read input file")
    };

    println!("Part 1: {}", part1(&text));
    println!("Part 2: No Part 2 on Day 25!");
}
