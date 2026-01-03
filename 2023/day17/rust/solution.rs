// Day 17: Clumsy Crucible - Dijkstra's shortest path with movement constraints
//
// Optimized: uses fixed-size array for visited states instead of HashSet.

use std::collections::BinaryHeap;
use std::cmp::Ordering;
use std::fs;
use std::path::Path;

const MAX_SIZE: usize = 150;
const MAX_CONSEC: usize = 11;  // 0-10
const NUM_DIRS: usize = 4;

// Direction deltas: right, down, left, up
const DR: [i32; 4] = [0, 1, 0, -1];
const DC: [i32; 4] = [1, 0, -1, 0];

#[derive(Clone, Copy, Eq, PartialEq)]
struct Node {
    heat: u32,
    row: i32,
    col: i32,
    direction: i8,    // -1 = no direction, 0=right, 1=down, 2=left, 3=up
    consecutive: u8,
}

// For BinaryHeap - we want min-heap, so reverse the ordering
impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.heat.cmp(&self.heat)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn parse_input(filename: &Path) -> Vec<Vec<u8>> {
    let content = fs::read_to_string(filename).expect("Failed to read input file");
    content
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap() as u8).collect())
        .collect()
}

fn dijkstra(grid: &[Vec<u8>], min_straight: u8, max_straight: u8) -> u32 {
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;

    // Visited array: [row][col][dir][consec]
    let mut visited = vec![[[false; MAX_CONSEC]; NUM_DIRS]; MAX_SIZE * MAX_SIZE];

    let mut pq = BinaryHeap::new();

    // Start with no direction
    pq.push(Node {
        heat: 0,
        row: 0,
        col: 0,
        direction: -1,
        consecutive: 0,
    });

    while let Some(Node { heat, row, col, direction, consecutive }) = pq.pop() {
        // Check if we reached the goal
        if row == rows - 1 && col == cols - 1 {
            if min_straight == 0 || consecutive >= min_straight {
                return heat;
            }
        }

        // Check if already visited (skip dir=-1 case)
        if direction >= 0 {
            let idx = (row as usize) * MAX_SIZE + (col as usize);
            if visited[idx][direction as usize][consecutive as usize] {
                continue;
            }
            visited[idx][direction as usize][consecutive as usize] = true;
        }

        // Try all four directions
        for nd in 0..4i8 {
            // Can't reverse direction
            if direction != -1 && nd == (direction + 2) % 4 {
                continue;
            }

            let nr = row + DR[nd as usize];
            let nc = col + DC[nd as usize];

            // Bounds check
            if nr < 0 || nr >= rows || nc < 0 || nc >= cols {
                continue;
            }

            // Check consecutive constraints
            let new_consec: u8;
            if nd == direction {
                // Continuing in same direction
                new_consec = consecutive + 1;
                if new_consec > max_straight {
                    continue;
                }
            } else {
                // Turning - must have gone min_straight in previous direction first
                if direction != -1 && consecutive < min_straight {
                    continue;
                }
                new_consec = 1;
            }

            // Skip if already visited
            let nr_idx = (nr as usize) * MAX_SIZE + (nc as usize);
            if visited[nr_idx][nd as usize][new_consec as usize] {
                continue;
            }

            let new_heat = heat + grid[nr as usize][nc as usize] as u32;
            pq.push(Node {
                heat: new_heat,
                row: nr,
                col: nc,
                direction: nd,
                consecutive: new_consec,
            });
        }
    }

    0 // No path found
}

fn part1(grid: &[Vec<u8>]) -> u32 {
    dijkstra(grid, 0, 3)
}

fn part2(grid: &[Vec<u8>]) -> u32 {
    dijkstra(grid, 4, 10)
}

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let grid = parse_input(&input_path);

    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}
