use std::fs;
use std::path::Path;

fn parse_grid(input: &str) -> Vec<Vec<u8>> {
    input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap() as u8).collect())
        .collect()
}

fn is_visible(grid: &[Vec<u8>], row: usize, col: usize) -> bool {
    let rows = grid.len();
    let cols = grid[0].len();
    let height = grid[row][col];

    // Check from left
    let visible_left = (0..col).all(|c| grid[row][c] < height);
    // Check from right
    let visible_right = (col + 1..cols).all(|c| grid[row][c] < height);
    // Check from top
    let visible_top = (0..row).all(|r| grid[r][col] < height);
    // Check from bottom
    let visible_bottom = (row + 1..rows).all(|r| grid[r][col] < height);

    visible_left || visible_right || visible_top || visible_bottom
}

fn scenic_score(grid: &[Vec<u8>], row: usize, col: usize) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();
    let height = grid[row][col];

    // Count trees visible in each direction
    // Left
    let mut left = 0;
    for c in (0..col).rev() {
        left += 1;
        if grid[row][c] >= height {
            break;
        }
    }

    // Right
    let mut right = 0;
    for c in col + 1..cols {
        right += 1;
        if grid[row][c] >= height {
            break;
        }
    }

    // Up
    let mut up = 0;
    for r in (0..row).rev() {
        up += 1;
        if grid[r][col] >= height {
            break;
        }
    }

    // Down
    let mut down = 0;
    for r in row + 1..rows {
        down += 1;
        if grid[r][col] >= height {
            break;
        }
    }

    left * right * up * down
}

fn part1(grid: &[Vec<u8>]) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut count = 0;

    for r in 0..rows {
        for c in 0..cols {
            if is_visible(grid, r, c) {
                count += 1;
            }
        }
    }

    count
}

fn part2(grid: &[Vec<u8>]) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut max_score = 0;

    for r in 0..rows {
        for c in 0..cols {
            let score = scenic_score(grid, r, c);
            if score > max_score {
                max_score = score;
            }
        }
    }

    max_score
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    let input = fs::read_to_string(&input_path)
        .or_else(|_| fs::read_to_string(Path::new("../input.txt")))
        .expect("Failed to read input.txt");

    let grid = parse_grid(&input);

    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}
