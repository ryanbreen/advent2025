use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");
    let input = input.trim();

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn part1(input: &str) -> u32 {
    let lines: Vec<&str> = input.lines().collect();

    // Find all symbol positions
    let symbols = find_symbols(&lines);

    // Find all numbers and check if adjacent to symbols
    let mut total = 0;

    for (row, line) in lines.iter().enumerate() {
        let mut col = 0;
        let chars: Vec<char> = line.chars().collect();

        while col < chars.len() {
            if chars[col].is_ascii_digit() {
                // Found start of a number
                let start_col = col;
                let mut num_str = String::new();

                while col < chars.len() && chars[col].is_ascii_digit() {
                    num_str.push(chars[col]);
                    col += 1;
                }

                let end_col = col - 1;
                let value: u32 = num_str.parse().unwrap();

                // Check if this number is adjacent to any symbol
                if is_adjacent_to_symbol(row, start_col, end_col, &symbols) {
                    total += value;
                }
            } else {
                col += 1;
            }
        }
    }

    total
}

fn part2(input: &str) -> u32 {
    let lines: Vec<&str> = input.lines().collect();

    // Find all gear positions (*)
    let gears = find_gears(&lines);

    // Map each gear to its adjacent numbers
    let mut gear_numbers: HashMap<(usize, usize), Vec<u32>> = HashMap::new();

    for (row, line) in lines.iter().enumerate() {
        let mut col = 0;
        let chars: Vec<char> = line.chars().collect();

        while col < chars.len() {
            if chars[col].is_ascii_digit() {
                // Found start of a number
                let start_col = col;
                let mut num_str = String::new();

                while col < chars.len() && chars[col].is_ascii_digit() {
                    num_str.push(chars[col]);
                    col += 1;
                }

                let end_col = col - 1;
                let value: u32 = num_str.parse().unwrap();

                // Find all adjacent gears
                let adjacent_gears = find_adjacent_gears(row, start_col, end_col, &gears);
                for gear_pos in adjacent_gears {
                    gear_numbers.entry(gear_pos).or_insert_with(Vec::new).push(value);
                }
            } else {
                col += 1;
            }
        }
    }

    // Sum gear ratios for gears with exactly 2 adjacent numbers
    let mut total = 0;
    for nums in gear_numbers.values() {
        if nums.len() == 2 {
            total += nums[0] * nums[1];
        }
    }

    total
}

fn find_symbols(lines: &[&str]) -> HashSet<(usize, usize)> {
    let mut symbols = HashSet::new();

    for (row, line) in lines.iter().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            if ch != '.' && !ch.is_ascii_digit() {
                symbols.insert((row, col));
            }
        }
    }

    symbols
}

fn find_gears(lines: &[&str]) -> HashSet<(usize, usize)> {
    let mut gears = HashSet::new();

    for (row, line) in lines.iter().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            if ch == '*' {
                gears.insert((row, col));
            }
        }
    }

    gears
}

fn is_adjacent_to_symbol(row: usize, start_col: usize, end_col: usize, symbols: &HashSet<(usize, usize)>) -> bool {
    // Check all adjacent positions around the number
    for pos in get_adjacent_positions(row, start_col, end_col) {
        if symbols.contains(&pos) {
            return true;
        }
    }
    false
}

fn find_adjacent_gears(row: usize, start_col: usize, end_col: usize, gears: &HashSet<(usize, usize)>) -> HashSet<(usize, usize)> {
    let mut adjacent_gears = HashSet::new();

    for pos in get_adjacent_positions(row, start_col, end_col) {
        if gears.contains(&pos) {
            adjacent_gears.insert(pos);
        }
    }

    adjacent_gears
}

fn get_adjacent_positions(row: usize, start_col: usize, end_col: usize) -> Vec<(usize, usize)> {
    let mut positions = Vec::new();

    // Above and below rows (including diagonals)
    if row > 0 {
        let above_row = row - 1;
        if start_col > 0 {
            for c in start_col - 1..=end_col + 1 {
                positions.push((above_row, c));
            }
        } else {
            for c in start_col..=end_col + 1 {
                positions.push((above_row, c));
            }
        }
    }

    // Below row
    let below_row = row + 1;
    if start_col > 0 {
        for c in start_col - 1..=end_col + 1 {
            positions.push((below_row, c));
        }
    } else {
        for c in start_col..=end_col + 1 {
            positions.push((below_row, c));
        }
    }

    // Left and right (same row)
    if start_col > 0 {
        positions.push((row, start_col - 1));
    }
    positions.push((row, end_col + 1));

    positions
}
