use std::fs;
use std::path::Path;

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let input_text = fs::read_to_string(input_path)
        .expect("Failed to read input file")
        .trim()
        .to_string();

    let grid: Vec<Vec<char>> = input_text
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}

fn part1(grid: &Vec<Vec<char>>) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();
    let target = "XMAS";
    let target_chars: Vec<char> = target.chars().collect();
    let mut count = 0;

    // 8 directions: right, left, down, up, and 4 diagonals
    let directions = [
        (0, 1),   // right
        (0, -1),  // left
        (1, 0),   // down
        (-1, 0),  // up
        (1, 1),   // down-right
        (1, -1),  // down-left
        (-1, 1),  // up-right
        (-1, -1), // up-left
    ];

    for r in 0..rows {
        for c in 0..cols {
            // Try each direction from this position
            for &(dr, dc) in &directions {
                let mut found = true;

                // Check if XMAS fits in this direction
                for (i, &ch) in target_chars.iter().enumerate() {
                    let nr = r as i32 + dr * i as i32;
                    let nc = c as i32 + dc * i as i32;

                    if nr < 0 || nr >= rows as i32 || nc < 0 || nc >= cols as i32 {
                        found = false;
                        break;
                    }

                    if grid[nr as usize][nc as usize] != ch {
                        found = false;
                        break;
                    }
                }

                if found {
                    count += 1;
                }
            }
        }
    }

    count
}

fn part2(grid: &Vec<Vec<char>>) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut count = 0;

    // Check each possible center point (A must be in the middle)
    for r in 1..rows - 1 {
        for c in 1..cols - 1 {
            if grid[r][c] != 'A' {
                continue;
            }

            // Get the four corners
            let top_left = grid[r - 1][c - 1];
            let top_right = grid[r - 1][c + 1];
            let bottom_left = grid[r + 1][c - 1];
            let bottom_right = grid[r + 1][c + 1];

            // Check diagonal 1 (top-left to bottom-right): MAS or SAM
            let diag1_ok = (top_left == 'M' && bottom_right == 'S')
                        || (top_left == 'S' && bottom_right == 'M');

            // Check diagonal 2 (top-right to bottom-left): MAS or SAM
            let diag2_ok = (top_right == 'M' && bottom_left == 'S')
                        || (top_right == 'S' && bottom_left == 'M');

            if diag1_ok && diag2_ok {
                count += 1;
            }
        }
    }

    count
}
