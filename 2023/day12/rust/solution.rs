// Advent of Code 2023 Day 12: Hot Springs
// Rust solution using memoized dynamic programming

use std::collections::HashMap;
use std::fs;

fn count_arrangements(pattern: &[u8], groups: &[usize]) -> i64 {
    let mut memo: HashMap<(usize, usize, usize), i64> = HashMap::new();
    dp(pattern, groups, 0, 0, 0, &mut memo)
}

fn dp(
    pattern: &[u8],
    groups: &[usize],
    pos: usize,
    group_idx: usize,
    current_run: usize,
    memo: &mut HashMap<(usize, usize, usize), i64>,
) -> i64 {
    // Check memoization
    let key = (pos, group_idx, current_run);
    if let Some(&result) = memo.get(&key) {
        return result;
    }

    // Base case: reached end of pattern
    if pos == pattern.len() {
        let result = if group_idx == groups.len() && current_run == 0 {
            // Valid: matched all groups and no partial run
            1
        } else if group_idx == groups.len() - 1 && groups[group_idx] == current_run {
            // Valid: on last group and run matches
            1
        } else {
            0
        };
        memo.insert(key, result);
        return result;
    }

    let mut result = 0;
    let ch = pattern[pos];

    // Option 1: Place operational spring (.)
    if ch == b'.' || ch == b'?' {
        if current_run == 0 {
            // No active run, just move forward
            result += dp(pattern, groups, pos + 1, group_idx, 0, memo);
        } else if group_idx < groups.len() && groups[group_idx] == current_run {
            // End current run if it matches expected group size
            result += dp(pattern, groups, pos + 1, group_idx + 1, 0, memo);
        }
        // Otherwise invalid (run doesn't match group)
    }

    // Option 2: Place damaged spring (#)
    if ch == b'#' || ch == b'?' {
        if group_idx < groups.len() && current_run < groups[group_idx] {
            // Can extend current run
            result += dp(pattern, groups, pos + 1, group_idx, current_run + 1, memo);
        }
        // Otherwise invalid (exceeds group size or no more groups)
    }

    memo.insert(key, result);
    result
}

fn parse_line(line: &str) -> (Vec<u8>, Vec<usize>) {
    let (pattern_str, groups_str) = line.trim().split_once(' ').unwrap();
    let pattern = pattern_str.as_bytes().to_vec();
    let groups: Vec<usize> = groups_str.split(',').map(|x| x.parse().unwrap()).collect();
    (pattern, groups)
}

fn unfold(pattern: &[u8], groups: &[usize]) -> (Vec<u8>, Vec<usize>) {
    let mut unfolded_pattern = Vec::new();
    for i in 0..5 {
        if i > 0 {
            unfolded_pattern.push(b'?');
        }
        unfolded_pattern.extend_from_slice(pattern);
    }
    let unfolded_groups: Vec<usize> = groups.iter().cycle().take(groups.len() * 5).copied().collect();
    (unfolded_pattern, unfolded_groups)
}

fn part1(lines: &[&str]) -> i64 {
    lines.iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            let (pattern, groups) = parse_line(line);
            count_arrangements(&pattern, &groups)
        })
        .sum()
}

fn part2(lines: &[&str]) -> i64 {
    lines.iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            let (pattern, groups) = parse_line(line);
            let (unfolded_pattern, unfolded_groups) = unfold(&pattern, &groups);
            count_arrangements(&unfolded_pattern, &unfolded_groups)
        })
        .sum()
}

fn main() {
    let input = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let lines: Vec<&str> = input.lines().collect();

    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
