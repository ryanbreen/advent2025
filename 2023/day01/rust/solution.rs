use std::fs;

const DIGIT_WORDS: [(&str, u32); 9] = [
    ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5),
    ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9),
];

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn part1(input: &str) -> u32 {
    input.lines()
        .filter_map(|line| {
            let digits: Vec<_> = line.chars().filter_map(|c| c.to_digit(10)).collect();
            Some(digits.first()? * 10 + digits.last()?)
        })
        .sum()
}

fn part2(input: &str) -> u32 {
    input.lines()
        .filter_map(|line| {
            // Extract all digits (numeric or spelled out) with their positions
            let digits: Vec<u32> = line
                .char_indices()
                .filter_map(|(i, c)| {
                    // Check for numeric digit
                    c.to_digit(10).or_else(|| {
                        // Check for spelled-out digit
                        DIGIT_WORDS
                            .iter()
                            .find(|(word, _)| line[i..].starts_with(word))
                            .map(|(_, digit)| *digit)
                    })
                })
                .collect();

            Some(digits.first()? * 10 + digits.last()?)
        })
        .sum()
}
