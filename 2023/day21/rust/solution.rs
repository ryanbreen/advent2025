// Day 21: Step Counter - Garden plot reachability

use std::collections::{HashMap, VecDeque};
use std::fs;
use std::path::Path;

fn parse_input(filename: &Path) -> (Vec<Vec<char>>, (i64, i64)) {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    let grid: Vec<Vec<char>> = content
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().collect())
        .collect();

    let mut start = (0i64, 0i64);
    for (r, row) in grid.iter().enumerate() {
        for (c, &ch) in row.iter().enumerate() {
            if ch == 'S' {
                start = (r as i64, c as i64);
                break;
            }
        }
    }

    (grid, start)
}

fn count_reachable(grid: &[Vec<char>], start: (i64, i64), steps: i64) -> i64 {
    let rows = grid.len() as i64;
    let cols = grid[0].len() as i64;

    let mut visited: HashMap<(i64, i64), i64> = HashMap::new();
    let mut queue: VecDeque<(i64, i64, i64)> = VecDeque::new();

    queue.push_back((start.0, start.1, 0));
    visited.insert(start, 0);

    let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)];

    while let Some((r, c, dist)) = queue.pop_front() {
        if dist >= steps {
            continue;
        }

        for (dr, dc) in directions.iter() {
            let nr = r + dr;
            let nc = c + dc;

            if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
                let ch = grid[nr as usize][nc as usize];
                if ch != '#' && !visited.contains_key(&(nr, nc)) {
                    visited.insert((nr, nc), dist + 1);
                    queue.push_back((nr, nc, dist + 1));
                }
            }
        }
    }

    // Count cells reachable in exactly 'steps' steps
    // A cell reachable in d steps can be reached in d+2, d+4, ... steps
    // So we need cells where d <= steps and d has same parity as steps
    let target_parity = steps % 2;
    visited
        .values()
        .filter(|&&d| d <= steps && d % 2 == target_parity)
        .count() as i64
}

fn count_reachable_infinite_bfs(grid: &[Vec<char>], start: (i64, i64), steps: i64) -> i64 {
    let rows = grid.len() as i64;
    let cols = grid[0].len() as i64;

    let mut visited: HashMap<(i64, i64), i64> = HashMap::new();
    let mut queue: VecDeque<(i64, i64, i64)> = VecDeque::new();

    queue.push_back((start.0, start.1, 0));
    visited.insert((start.0, start.1), 0);

    let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)];

    while let Some((r, c, dist)) = queue.pop_front() {
        if dist >= steps {
            continue;
        }

        for (dr, dc) in directions.iter() {
            let nr = r + dr;
            let nc = c + dc;

            // Map to grid coordinates (infinite tiling)
            let gr = nr.rem_euclid(rows);
            let gc = nc.rem_euclid(cols);

            let ch = grid[gr as usize][gc as usize];
            if ch != '#' && !visited.contains_key(&(nr, nc)) {
                visited.insert((nr, nc), dist + 1);
                queue.push_back((nr, nc, dist + 1));
            }
        }
    }

    let target_parity = steps % 2;
    visited
        .values()
        .filter(|&&d| d <= steps && d % 2 == target_parity)
        .count() as i64
}

fn count_reachable_infinite(grid: &[Vec<char>], start: (i64, i64), steps: i64) -> i64 {
    let rows = grid.len() as i64;
    let size = rows;
    let half = size / 2;

    if steps <= size * 2 {
        return count_reachable_infinite_bfs(grid, start, steps);
    }

    // The number of full grid widths we travel
    let n = (steps - half) / size;

    // Calculate reachable counts for n=0, 1, 2
    let y0 = count_reachable_infinite_bfs(grid, start, half);
    let y1 = count_reachable_infinite_bfs(grid, start, half + size);
    let y2 = count_reachable_infinite_bfs(grid, start, half + 2 * size);

    // Solve for a, b, c using finite differences
    // f(x) = y0 + (y1-y0)*x + (y2-2*y1+y0)*x*(x-1)/2
    let a = (y2 - 2 * y1 + y0) / 2;
    let b = y1 - y0 - a;
    let c = y0;

    a * n * n + b * n + c
}

fn part1(grid: &[Vec<char>], start: (i64, i64)) -> i64 {
    count_reachable(grid, start, 64)
}

fn part2(grid: &[Vec<char>], start: (i64, i64)) -> i64 {
    count_reachable_infinite(grid, start, 26501365)
}

fn main() {
    let input_path = Path::new("../input.txt");

    let (grid, start) = parse_input(input_path);

    println!("Part 1: {}", part1(&grid, start));
    println!("Part 2: {}", part2(&grid, start));
}
