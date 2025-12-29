use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

fn get_pipe_connections(ch: char) -> Option<[(i32, i32); 2]> {
    match ch {
        '|' => Some([(-1, 0), (1, 0)]),   // N, S
        '-' => Some([(0, -1), (0, 1)]),   // W, E
        'L' => Some([(-1, 0), (0, 1)]),   // N, E
        'J' => Some([(-1, 0), (0, -1)]),  // N, W
        '7' => Some([(1, 0), (0, -1)]),   // S, W
        'F' => Some([(1, 0), (0, 1)]),    // S, E
        _ => None,
    }
}

fn find_start(grid: &[Vec<char>]) -> (usize, usize) {
    for (r, row) in grid.iter().enumerate() {
        for (c, &ch) in row.iter().enumerate() {
            if ch == 'S' {
                return (r, c);
            }
        }
    }
    panic!("No start position found");
}

fn get_neighbors(grid: &[Vec<char>], pos: (usize, usize)) -> Vec<(usize, usize)> {
    let (r, c) = pos;
    let rows = grid.len();
    let cols = grid[0].len();
    let ch = grid[r][c];

    if ch == 'S' {
        // S can connect to any adjacent pipe that connects back to it
        let mut neighbors = Vec::new();
        let directions: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

        for (dr, dc) in directions {
            let nr = r as i32 + dr;
            let nc = c as i32 + dc;

            if nr >= 0 && nr < rows as i32 && nc >= 0 && nc < cols as i32 {
                let nr = nr as usize;
                let nc = nc as usize;
                let adj_ch = grid[nr][nc];

                if let Some(connections) = get_pipe_connections(adj_ch) {
                    for (adj_dr, adj_dc) in connections {
                        if nr as i32 + adj_dr == r as i32 && nc as i32 + adj_dc == c as i32 {
                            neighbors.push((nr, nc));
                            break;
                        }
                    }
                }
            }
        }
        neighbors
    } else if let Some(connections) = get_pipe_connections(ch) {
        let mut neighbors = Vec::new();
        for (dr, dc) in connections {
            let nr = r as i32 + dr;
            let nc = c as i32 + dc;

            if nr >= 0 && nr < rows as i32 && nc >= 0 && nc < cols as i32 {
                neighbors.push((nr as usize, nc as usize));
            }
        }
        neighbors
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
    let (r, c) = start;
    let rows = grid.len();
    let cols = grid[0].len();

    let mut connections = HashSet::new();
    let directions: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

    for (dr, dc) in directions {
        let nr = r as i32 + dr;
        let nc = c as i32 + dc;

        if nr >= 0 && nr < rows as i32 && nc >= 0 && nc < cols as i32 {
            let nr = nr as usize;
            let nc = nc as usize;

            if loop_positions.contains(&(nr, nc)) {
                let adj_ch = grid[nr][nc];
                if let Some(pipe_connections) = get_pipe_connections(adj_ch) {
                    for (adj_dr, adj_dc) in pipe_connections {
                        if nr as i32 + adj_dr == r as i32 && nc as i32 + adj_dc == c as i32 {
                            connections.insert((dr, dc));
                            break;
                        }
                    }
                }
            }
        }
    }

    // Match the connections to find the pipe type
    let pipes = ['|', '-', 'L', 'J', '7', 'F'];
    for pipe in pipes {
        if let Some(pipe_dirs) = get_pipe_connections(pipe) {
            let pipe_set: HashSet<(i32, i32)> = pipe_dirs.iter().cloned().collect();
            if pipe_set == connections {
                return pipe;
            }
        }
    }

    'S'
}

fn part1(grid: &[Vec<char>]) -> usize {
    let start = find_start(grid);
    let distances = find_loop(grid, start);
    *distances.values().max().unwrap()
}

fn part2(grid: &[Vec<char>]) -> usize {
    let start = find_start(grid);
    let distances = find_loop(grid, start);
    let loop_positions: HashSet<(usize, usize)> = distances.keys().cloned().collect();

    // Replace S with its actual pipe type
    let start_pipe = determine_start_pipe(grid, start, &loop_positions);
    let mut grid = grid.to_vec();
    grid[start.0][start.1] = start_pipe;

    let rows = grid.len();
    let cols = grid[0].len();
    let mut enclosed = 0;

    for r in 0..rows {
        let mut inside = false;
        for c in 0..cols {
            if loop_positions.contains(&(r, c)) {
                let ch = grid[r][c];
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
