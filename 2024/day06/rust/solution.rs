use std::collections::HashSet;
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn turn_right(&self) -> Direction {
        match self {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        }
    }

    fn delta(&self) -> (i32, i32) {
        match self {
            Direction::Up => (-1, 0),
            Direction::Down => (1, 0),
            Direction::Left => (0, -1),
            Direction::Right => (0, 1),
        }
    }
}

fn parse_input(input: &str) -> (Vec<Vec<char>>, (usize, usize), Direction) {
    let grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    // Find starting position and direction
    let mut start_pos = (0, 0);
    let mut start_dir = Direction::Up;

    for (row, line) in grid.iter().enumerate() {
        for (col, &ch) in line.iter().enumerate() {
            match ch {
                '^' => {
                    start_pos = (row, col);
                    start_dir = Direction::Up;
                }
                'v' => {
                    start_pos = (row, col);
                    start_dir = Direction::Down;
                }
                '<' => {
                    start_pos = (row, col);
                    start_dir = Direction::Left;
                }
                '>' => {
                    start_pos = (row, col);
                    start_dir = Direction::Right;
                }
                _ => {}
            }
        }
    }

    (grid, start_pos, start_dir)
}

fn simulate_guard(grid: &Vec<Vec<char>>, start_pos: (usize, usize), start_dir: Direction) -> usize {
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };

    let mut visited = HashSet::new();
    let mut pos = start_pos;
    let mut dir = start_dir;

    // Mark starting position as visited
    visited.insert(pos);

    loop {
        // Calculate next position
        let (dr, dc) = dir.delta();
        let next_row = pos.0 as i32 + dr;
        let next_col = pos.1 as i32 + dc;

        // Check if guard would leave the map
        if next_row < 0 || next_row >= rows as i32 || next_col < 0 || next_col >= cols as i32 {
            break;
        }

        let next_pos = (next_row as usize, next_col as usize);

        // Check if there's an obstacle ahead
        if grid[next_pos.0][next_pos.1] == '#' {
            // Turn right
            dir = dir.turn_right();
        } else {
            // Move forward
            pos = next_pos;
            visited.insert(pos);
        }
    }

    visited.len()
}

fn part1(input: &str) -> usize {
    let (grid, start_pos, start_dir) = parse_input(input);
    simulate_guard(&grid, start_pos, start_dir)
}

fn check_loop(grid: &Vec<Vec<char>>, start_pos: (usize, usize), start_dir: Direction) -> bool {
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };

    // Track states (position, direction) to detect loops
    let mut states = HashSet::new();
    let mut pos = start_pos;
    let mut dir = start_dir;

    loop {
        // If we've seen this state before, we're in a loop
        if !states.insert((pos, dir)) {
            return true;
        }

        // Calculate next position
        let (dr, dc) = dir.delta();
        let next_row = pos.0 as i32 + dr;
        let next_col = pos.1 as i32 + dc;

        // Check if guard would leave the map
        if next_row < 0 || next_row >= rows as i32 || next_col < 0 || next_col >= cols as i32 {
            return false;
        }

        let next_pos = (next_row as usize, next_col as usize);

        // Check if there's an obstacle ahead
        if grid[next_pos.0][next_pos.1] == '#' {
            // Turn right
            dir = dir.turn_right();
        } else {
            // Move forward
            pos = next_pos;
        }
    }
}

fn part2(input: &str) -> usize {
    let (mut grid, start_pos, start_dir) = parse_input(input);
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };

    let mut count = 0;

    // Try placing an obstruction at each position
    for row in 0..rows {
        for col in 0..cols {
            // Skip if there's already an obstacle or if it's the starting position
            if grid[row][col] == '#' || (row, col) == start_pos {
                continue;
            }

            // Place temporary obstruction
            grid[row][col] = '#';

            // Check if this creates a loop
            if check_loop(&grid, start_pos, start_dir) {
                count += 1;
            }

            // Remove the obstruction
            grid[row][col] = '.';
        }
    }

    count
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
