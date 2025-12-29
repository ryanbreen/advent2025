//! Advent of Code 2023 - Day 9: Mirage Maintenance
//!
//! Extrapolates values from OASIS report sequences using difference pyramids.

use std::fs;

/// Parses the input into a vector of number sequences.
fn parse_input(text: &str) -> Vec<Vec<i64>> {
    text.lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect()
        })
        .collect()
}

/// Extrapolates the next value in a sequence using the difference pyramid method.
///
/// Builds successive difference sequences until all zeros, then sums the last
/// elements working back up to find the extrapolated value.
fn extrapolate(seq: &[i64]) -> i64 {
    if seq.iter().all(|&x| x == 0) {
        return 0;
    }
    let differences: Vec<i64> = seq.windows(2).map(|w| w[1] - w[0]).collect();
    seq.last().unwrap() + extrapolate(&differences)
}

/// Part 1: Sum of extrapolated next values for all sequences.
fn part1(histories: &[Vec<i64>]) -> i64 {
    histories.iter().map(|h| extrapolate(h)).sum()
}

/// Part 2: Sum of extrapolated previous values for all sequences.
///
/// Key insight: Extrapolating backwards is equivalent to extrapolating forwards
/// on the reversed sequence. This avoids O(n) insertions at index 0.
fn part2(histories: &[Vec<i64>]) -> i64 {
    histories
        .iter()
        .map(|h| {
            let reversed: Vec<i64> = h.iter().copied().rev().collect();
            extrapolate(&reversed)
        })
        .sum()
}

fn main() {
    let input = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let histories = parse_input(&input);

    println!("Part 1: {}", part1(&histories));
    println!("Part 2: {}", part2(&histories));
}
