use std::fs;

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input.txt");

    let grid: Vec<Vec<char>> = input
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}

fn part1(grid: &[Vec<char>]) -> usize {
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };

    let mut count = 0;

    for row in 0..rows {
        for col in 0..cols {
            if grid[row][col] == '@' {
                let neighbors = count_neighbors(grid, row, col);
                if neighbors < 4 {
                    count += 1;
                }
            }
        }
    }

    count
}

fn part2(grid: &[Vec<char>]) -> usize {
    let mut grid = grid.to_vec();
    let rows = grid.len();
    let cols = grid[0].len();

    // Precompute neighbor counts for all rolls
    let mut neighbor_count = vec![vec![0; cols]; rows];
    for row in 0..rows {
        for col in 0..cols {
            if grid[row][col] == '@' {
                neighbor_count[row][col] = count_neighbors(&grid, row, col);
            }
        }
    }

    // Initialize queue with all accessible rolls (< 4 neighbors)
    let mut queue = Vec::new();
    for row in 0..rows {
        for col in 0..cols {
            if grid[row][col] == '@' && neighbor_count[row][col] < 4 {
                queue.push((row, col));
            }
        }
    }

    let mut total_removed = 0;
    let mut queue_index = 0;

    // Process queue
    while queue_index < queue.len() {
        let (row, col) = queue[queue_index];
        queue_index += 1;

        // Skip if already removed
        if grid[row][col] != '@' {
            continue;
        }

        // Remove this roll
        grid[row][col] = '.';
        total_removed += 1;

        // Decrement neighbor counts for all adjacent rolls
        let directions = [
            (-1, -1), (-1, 0), (-1, 1),
            (0, -1),           (0, 1),
            (1, -1),  (1, 0),  (1, 1),
        ];

        for (dr, dc) in directions {
            let new_row = row as i32 + dr;
            let new_col = col as i32 + dc;

            if new_row >= 0 && new_row < rows as i32 && new_col >= 0 && new_col < cols as i32 {
                let r = new_row as usize;
                let c = new_col as usize;
                if grid[r][c] == '@' {
                    neighbor_count[r][c] -= 1;
                    // If this neighbor just became accessible, add to queue
                    if neighbor_count[r][c] == 3 {
                        queue.push((r, c));
                    }
                }
            }
        }
    }

    total_removed
}

fn count_neighbors(grid: &[Vec<char>], row: usize, col: usize) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut count = 0;

    // 8 directions: up, down, left, right, and 4 diagonals
    let directions = [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1),           (0, 1),
        (1, -1),  (1, 0),  (1, 1),
    ];

    for (dr, dc) in directions {
        let new_row = row as i32 + dr;
        let new_col = col as i32 + dc;

        if new_row >= 0 && new_row < rows as i32 && new_col >= 0 && new_col < cols as i32 {
            let r = new_row as usize;
            let c = new_col as usize;
            if grid[r][c] == '@' {
                count += 1;
            }
        }
    }

    count
}

fn find_accessible(grid: &[Vec<char>]) -> Vec<(usize, usize)> {
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };
    let mut accessible = Vec::new();

    for row in 0..rows {
        for col in 0..cols {
            if grid[row][col] == '@' {
                let neighbors = count_neighbors(grid, row, col);
                if neighbors < 4 {
                    accessible.push((row, col));
                }
            }
        }
    }

    accessible
}
