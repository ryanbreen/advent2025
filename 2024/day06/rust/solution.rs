use std::convert::TryInto;
use std::fs;

const MAX_SIZE: usize = 200;

// Direction vectors: up, right, down, left
const DX: [i32; 4] = [-1, 0, 1, 0];
const DY: [i32; 4] = [0, 1, 0, -1];

struct Grid {
    data: Box<[[u8; MAX_SIZE]; MAX_SIZE]>,
    rows: usize,
    cols: usize,
    start_x: usize,
    start_y: usize,
    start_dir: usize,
}

fn parse_input(input: &str) -> Grid {
    // Use vec to allocate on heap, then convert to box
    let mut data = vec![[0u8; MAX_SIZE]; MAX_SIZE];
    let mut rows = 0;
    let mut cols = 0;
    let mut start_x = 0;
    let mut start_y = 0;
    let mut start_dir = 0;

    for (row, line) in input.lines().enumerate() {
        if line.is_empty() {
            continue;
        }
        if cols == 0 {
            cols = line.len();
        }
        for (col, ch) in line.bytes().enumerate() {
            data[row][col] = ch;
            match ch {
                b'^' => {
                    start_x = row;
                    start_y = col;
                    start_dir = 0;
                    data[row][col] = b'.';
                }
                b'>' => {
                    start_x = row;
                    start_y = col;
                    start_dir = 1;
                    data[row][col] = b'.';
                }
                b'v' => {
                    start_x = row;
                    start_y = col;
                    start_dir = 2;
                    data[row][col] = b'.';
                }
                b'<' => {
                    start_x = row;
                    start_y = col;
                    start_dir = 3;
                    data[row][col] = b'.';
                }
                _ => {}
            }
        }
        rows = row + 1;
    }

    // Convert vec to boxed array
    let data: Box<[[u8; MAX_SIZE]; MAX_SIZE]> = data.try_into().unwrap();

    Grid {
        data,
        rows,
        cols,
        start_x,
        start_y,
        start_dir,
    }
}

// Simulate guard movement for part1
// Returns (visited count, set of visited positions)
fn simulate_part1(grid: &Grid) -> (usize, Vec<(usize, usize)>) {
    let mut visited = [[false; MAX_SIZE]; MAX_SIZE];
    let mut path = Vec::new();

    let mut x = grid.start_x;
    let mut y = grid.start_y;
    let mut dir = grid.start_dir;

    visited[x][y] = true;
    path.push((x, y));

    loop {
        // Try to move forward
        let next_x = x as i32 + DX[dir];
        let next_y = y as i32 + DY[dir];

        // Check if we're leaving the map
        if next_x < 0 || next_x >= grid.rows as i32 || next_y < 0 || next_y >= grid.cols as i32 {
            return (path.len(), path);
        }

        let next_x = next_x as usize;
        let next_y = next_y as usize;

        // Check if there's an obstacle
        if grid.data[next_x][next_y] == b'#' {
            // Turn right
            dir = (dir + 1) % 4;
        } else {
            // Move forward
            x = next_x;
            y = next_y;

            if !visited[x][y] {
                visited[x][y] = true;
                path.push((x, y));
            }
        }
    }
}

// Simulate guard movement for part2 with obstruction
// Uses version numbers instead of clearing the array
// Returns true if guard gets stuck in a loop
fn simulate_with_obstruction(
    grid: &Grid,
    obstruction_x: usize,
    obstruction_y: usize,
    state_visited: &mut [[[u32; 4]; MAX_SIZE]; MAX_SIZE],
    version: u32,
) -> bool {
    let mut x = grid.start_x;
    let mut y = grid.start_y;
    let mut dir = grid.start_dir;

    loop {
        // Check if we've seen this state before (loop detection)
        if state_visited[x][y][dir] == version {
            return true;
        }
        state_visited[x][y][dir] = version;

        // Try to move forward
        let next_x = x as i32 + DX[dir];
        let next_y = y as i32 + DY[dir];

        // Check if we're leaving the map
        if next_x < 0 || next_x >= grid.rows as i32 || next_y < 0 || next_y >= grid.cols as i32 {
            return false;
        }

        let next_x = next_x as usize;
        let next_y = next_y as usize;

        // Check if there's an obstacle
        let is_obstacle = grid.data[next_x][next_y] == b'#'
            || (next_x == obstruction_x && next_y == obstruction_y);

        if is_obstacle {
            // Turn right
            dir = (dir + 1) % 4;
        } else {
            // Move forward
            x = next_x;
            y = next_y;
        }
    }
}

fn part1(grid: &Grid) -> (usize, Vec<(usize, usize)>) {
    simulate_part1(grid)
}

fn part2(grid: &Grid, path: &[(usize, usize)]) -> usize {
    let mut loop_positions = 0;

    // Allocate state_visited once on heap and reuse with version numbers
    let mut state_visited: Box<[[[u32; 4]; MAX_SIZE]; MAX_SIZE]> =
        vec![[[0u32; 4]; MAX_SIZE]; MAX_SIZE].try_into().unwrap();
    let mut version = 0u32;

    // Only try placing obstructions on the original path (excluding start)
    // This is a significant optimization - we don't need to check positions
    // the guard would never reach anyway
    for &(r, c) in path.iter().skip(1) {
        // Skip if there's already an obstacle (shouldn't happen on path, but be safe)
        if grid.data[r][c] == b'#' {
            continue;
        }

        version += 1;
        // Simulate with obstruction at (r, c)
        if simulate_with_obstruction(grid, r, c, &mut state_visited, version) {
            loop_positions += 1;
        }
    }

    loop_positions
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let grid = parse_input(&input);
    let (count, path) = part1(&grid);
    println!("Part 1: {}", count);
    println!("Part 2: {}", part2(&grid, &path));
}
