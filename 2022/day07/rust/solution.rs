use std::collections::HashMap;
use std::fs;
use std::path::Path;

fn parse_filesystem(lines: &[&str]) -> HashMap<String, u64> {
    let mut path: Vec<&str> = Vec::new();
    let mut dir_sizes: HashMap<String, u64> = HashMap::new();

    for line in lines {
        if line.starts_with("$ cd") {
            let target = &line[5..];
            match target {
                "/" => {
                    path.clear();
                    path.push("/");
                }
                ".." => {
                    path.pop();
                }
                _ => {
                    path.push(target);
                }
            }
        } else if line.starts_with("$ ls") || line.starts_with("dir ") {
            continue;
        } else {
            // It's a file with size
            let size: u64 = line.split_whitespace().next().unwrap().parse().unwrap();
            // Add size to current directory and all parent directories
            for i in 0..path.len() {
                let dir_path = if i == 0 {
                    "/".to_string()
                } else {
                    path[..=i].join("/")
                };
                *dir_sizes.entry(dir_path).or_insert(0) += size;
            }
        }
    }

    dir_sizes
}

fn part1(dir_sizes: &HashMap<String, u64>) -> u64 {
    dir_sizes.values().filter(|&&size| size <= 100000).sum()
}

fn part2(dir_sizes: &HashMap<String, u64>) -> u64 {
    let total_space: u64 = 70000000;
    let needed_space: u64 = 30000000;
    let used_space = dir_sizes.get("/").unwrap();
    let free_space = total_space - used_space;
    let need_to_free = needed_space - free_space;

    *dir_sizes
        .values()
        .filter(|&&size| size >= need_to_free)
        .min()
        .unwrap()
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    let input = if Path::new(&input_path).exists() {
        fs::read_to_string(&input_path).unwrap()
    } else {
        fs::read_to_string("../input.txt").unwrap()
    };

    let lines: Vec<&str> = input.trim().lines().collect();
    let dir_sizes = parse_filesystem(&lines);

    println!("Part 1: {}", part1(&dir_sizes));
    println!("Part 2: {}", part2(&dir_sizes));
}
