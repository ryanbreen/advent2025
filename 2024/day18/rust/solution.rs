use std::collections::{HashSet, VecDeque};
use std::fs;

const SIZE: usize = 71;

fn parse_input(filename: &str) -> Vec<(i32, i32)> {
    fs::read_to_string(filename)
        .expect("Failed to read input file")
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut parts = line.split(',');
            let x = parts.next().unwrap().parse().unwrap();
            let y = parts.next().unwrap().parse().unwrap();
            (x, y)
        })
        .collect()
}

fn bfs(corrupted: &HashSet<(i32, i32)>) -> i32 {
    let start = (0, 0);
    let goal = (SIZE as i32 - 1, SIZE as i32 - 1);

    if corrupted.contains(&start) || corrupted.contains(&goal) {
        return -1;
    }

    let mut queue = VecDeque::new();
    queue.push_back((start, 0));
    let mut visited = HashSet::new();
    visited.insert(start);

    let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)];

    while let Some(((x, y), steps)) = queue.pop_front() {
        if (x, y) == goal {
            return steps;
        }

        for (dx, dy) in directions.iter() {
            let nx = x + dx;
            let ny = y + dy;

            if nx >= 0
                && nx < SIZE as i32
                && ny >= 0
                && ny < SIZE as i32
                && !visited.contains(&(nx, ny))
                && !corrupted.contains(&(nx, ny))
            {
                visited.insert((nx, ny));
                queue.push_back(((nx, ny), steps + 1));
            }
        }
    }

    -1
}

fn part1(positions: &[(i32, i32)], num_bytes: usize) -> i32 {
    let corrupted: HashSet<(i32, i32)> = positions.iter().take(num_bytes).copied().collect();
    bfs(&corrupted)
}

fn part2(positions: &[(i32, i32)]) -> (i32, i32) {
    let mut left = 0;
    let mut right = positions.len();

    while left < right {
        let mid = (left + right) / 2;
        let corrupted: HashSet<(i32, i32)> = positions.iter().take(mid + 1).copied().collect();
        if bfs(&corrupted) == -1 {
            right = mid;
        } else {
            left = mid + 1;
        }
    }

    positions[left]
}

fn main() {
    let positions = parse_input("../input.txt");

    let answer1 = part1(&positions, 1024);
    println!("Part 1: {}", answer1);

    let (x, y) = part2(&positions);
    println!("Part 2: {},{}", x, y);
}
