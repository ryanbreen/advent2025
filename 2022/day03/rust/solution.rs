use std::collections::HashSet;
use std::fs;
use std::path::Path;

fn priority(c: char) -> u32 {
    if c.is_lowercase() {
        c as u32 - 'a' as u32 + 1
    } else {
        c as u32 - 'A' as u32 + 27
    }
}

fn part1(rucksacks: &[String]) -> u32 {
    rucksacks
        .iter()
        .map(|rucksack| {
            let mid = rucksack.len() / 2;
            let first: HashSet<char> = rucksack[..mid].chars().collect();
            let second: HashSet<char> = rucksack[mid..].chars().collect();
            let common: Vec<char> = first.intersection(&second).copied().collect();
            priority(common[0])
        })
        .sum()
}

fn part2(rucksacks: &[String]) -> u32 {
    rucksacks
        .chunks(3)
        .map(|group| {
            let set1: HashSet<char> = group[0].chars().collect();
            let set2: HashSet<char> = group[1].chars().collect();
            let set3: HashSet<char> = group[2].chars().collect();
            let common12: HashSet<char> = set1.intersection(&set2).copied().collect();
            let common: Vec<char> = common12.intersection(&set3).copied().collect();
            priority(common[0])
        })
        .sum()
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    // Try relative path first, then fallback to ../input.txt from current dir
    let input = if input_path.exists() {
        fs::read_to_string(&input_path).unwrap()
    } else {
        fs::read_to_string(Path::new("../input.txt")).unwrap()
    };

    let rucksacks: Vec<String> = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(String::from)
        .collect();

    println!("Part 1: {}", part1(&rucksacks));
    println!("Part 2: {}", part2(&rucksacks));
}
