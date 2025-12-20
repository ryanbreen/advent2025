use std::collections::HashMap;
use std::fs;
use std::path::Path;

fn count_ways(design: &[u8], patterns: &[&[u8]]) -> u64 {
    let mut memo: HashMap<usize, u64> = HashMap::new();
    dp(0, design, patterns, &mut memo)
}

fn dp(pos: usize, design: &[u8], patterns: &[&[u8]], memo: &mut HashMap<usize, u64>) -> u64 {
    if pos == design.len() {
        return 1;
    }

    if let Some(&cached) = memo.get(&pos) {
        return cached;
    }

    let total: u64 = patterns
        .iter()
        .filter(|p| design[pos..].starts_with(p))
        .map(|p| dp(pos + p.len(), design, patterns, memo))
        .sum();

    memo.insert(pos, total);
    total
}

fn part1(patterns: &[&[u8]], designs: &[&[u8]]) -> usize {
    designs
        .iter()
        .filter(|d| count_ways(d, patterns) > 0)
        .count()
}

fn part2(patterns: &[&[u8]], designs: &[&[u8]]) -> u64 {
    designs.iter().map(|d| count_ways(d, patterns)).sum()
}

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let input = fs::read_to_string(&input_path)
        .unwrap_or_else(|_| fs::read_to_string("../input.txt").expect("Failed to read input.txt"));
    let input = input.trim();

    let parts: Vec<&str> = input.split("\n\n").collect();
    let patterns: Vec<&[u8]> = parts[0].split(',').map(|s| s.trim().as_bytes()).collect();
    let designs: Vec<&[u8]> = parts[1].lines().map(|s| s.as_bytes()).collect();

    println!("Part 1: {}", part1(&patterns, &designs));
    println!("Part 2: {}", part2(&patterns, &designs));
}
