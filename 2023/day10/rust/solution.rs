use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

fn get_pipe_connections(ch: char) -> Option<[(i32, i32); 2]> {
    match ch {
        '|' => Some([(-1, 0), (1, 0)]),  // N, S
        '-' => Some([(0, -1), (0, 1)]),  // W, E
        'L' => Some([(-1, 0), (0, 1)]),  // N, E
        'J' => Some([(-1, 0), (0, -1)]), // N, W
        '7' => Some([(1, 0), (0, -1)]),  // S, W
        'F' => Some([(1, 0), (0, 1)]),   // S, E
        _ => None,
    }
}

fn find_start(grid: &[Vec<char>]) -> (usize, usize) {
    grid.iter()
        .enumerate()
        .find_map(|(r, row)| row.iter().position(|&ch| ch == 'S').map(|c| (r, c)))
        .expect("No start position found")
}

/// Convert signed offset position to valid grid index, returning None if out of bounds.
fn offset_to_index(pos: usize, offset: i32, limit: usize) -> Option<usize> {
    let new_pos = pos as i32 + offset;
    if new_pos >= 0 && (new_pos as usize) < limit {
        Some(new_pos as usize)
    } else {
        None
    }
}

/// Check if adjacent cell at (row + dr, col + dc) connects back to (row, col).
fn connects_back(grid: &[Vec<char>], row: usize, col: usize, dr: i32, dc: i32) -> bool {
    let rows = grid.len();
    let cols = grid[0].len();

    let Some(adj_row) = offset_to_index(row, dr, rows) else {
        return false;
    };
    let Some(adj_col) = offset_to_index(col, dc, cols) else {
        return false;
    };

    let adj_ch = grid[adj_row][adj_col];
    get_pipe_connections(adj_ch).is_some_and(|connections| {
        connections
            .iter()
            .any(|&(adj_dr, adj_dc)| adj_row as i32 + adj_dr == row as i32 && adj_col as i32 + adj_dc == col as i32)
    })
}

fn get_neighbors(grid: &[Vec<char>], pos: (usize, usize)) -> Vec<(usize, usize)> {
    let (row, col) = pos;
    let rows = grid.len();
    let cols = grid[0].len();
    let ch = grid[row][col];

    if ch == 'S' {
        // S can connect to any adjacent pipe that connects back to it
        const DIRECTIONS: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

        DIRECTIONS
            .iter()
            .filter_map(|&(dr, dc)| {
                let adj_row = offset_to_index(row, dr, rows)?;
                let adj_col = offset_to_index(col, dc, cols)?;

                if connects_back(grid, row, col, dr, dc) {
                    Some((adj_row, adj_col))
                } else {
                    None
                }
            })
            .collect()
    } else if let Some(connections) = get_pipe_connections(ch) {
        connections
            .iter()
            .filter_map(|&(dr, dc)| {
                let adj_row = offset_to_index(row, dr, rows)?;
                let adj_col = offset_to_index(col, dc, cols)?;
                Some((adj_row, adj_col))
            })
            .collect()
    } else {
        Vec::new()
    }
}

fn find_loop(grid: &[Vec<char>], start: (usize, usize)) -> HashMap<(usize, usize), usize> {
    let mut distances = HashMap::new();
    distances.insert(start, 0);

    let mut queue = VecDeque::new();
    queue.push_back(start);

    while let Some(pos) = queue.pop_front() {
        let current_dist = distances[&pos];
        for neighbor in get_neighbors(grid, pos) {
            if !distances.contains_key(&neighbor) {
                distances.insert(neighbor, current_dist + 1);
                queue.push_back(neighbor);
            }
        }
    }

    distances
}

fn determine_start_pipe(
    grid: &[Vec<char>],
    start: (usize, usize),
    loop_positions: &HashSet<(usize, usize)>,
) -> char {
    let (row, col) = start;
    let rows = grid.len();
    let cols = grid[0].len();

    const DIRECTIONS: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

    let connections: HashSet<(i32, i32)> = DIRECTIONS
        .iter()
        .filter(|&&(dr, dc)| {
            let Some(adj_row) = offset_to_index(row, dr, rows) else {
                return false;
            };
            let Some(adj_col) = offset_to_index(col, dc, cols) else {
                return false;
            };

            loop_positions.contains(&(adj_row, adj_col)) && connects_back(grid, row, col, dr, dc)
        })
        .copied()
        .collect();

    // Match the connections to find the pipe type
    const PIPES: [char; 6] = ['|', '-', 'L', 'J', '7', 'F'];
    PIPES
        .iter()
        .find(|&&pipe| {
            get_pipe_connections(pipe)
                .map(|dirs| dirs.iter().copied().collect::<HashSet<_>>() == connections)
                .unwrap_or(false)
        })
        .copied()
        .unwrap_or('S')
}

fn part1(grid: &[Vec<char>]) -> usize {
    let start = find_start(grid);
    let distances = find_loop(grid, start);
    *distances.values().max().unwrap()
}

fn part2(grid: &[Vec<char>]) -> usize {
    let start = find_start(grid);
    let distances = find_loop(grid, start);
    let loop_positions: HashSet<(usize, usize)> = distances.keys().copied().collect();

    // Determine what pipe type S represents (without cloning the grid)
    let start_pipe = determine_start_pipe(grid, start, &loop_positions);

    let rows = grid.len();
    let cols = grid[0].len();
    let mut enclosed = 0;

    for row in 0..rows {
        let mut inside = false;
        for col in 0..cols {
            if loop_positions.contains(&(row, col)) {
                // Use start_pipe for S position, otherwise use grid value
                let ch = if (row, col) == start {
                    start_pipe
                } else {
                    grid[row][col]
                };
                // Count vertical crossings using the "north" rule
                // Count pipes that have a north connection: |, L, J
                if ch == '|' || ch == 'L' || ch == 'J' {
                    inside = !inside;
                }
            } else if inside {
                enclosed += 1;
            }
        }
    }

    enclosed
}

fn main() {
    let input = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}
