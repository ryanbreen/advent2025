use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;
use std::fs;

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    cost: usize,
    x: usize,
    y: usize,
    dir: usize,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse ordering for min-heap
        other.cost.cmp(&self.cost)
            .then_with(|| self.x.cmp(&other.x))
            .then_with(|| self.y.cmp(&other.y))
            .then_with(|| self.dir.cmp(&other.dir))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// Directions: 0=East, 1=South, 2=West, 3=North
const DX: [i32; 4] = [1, 0, -1, 0];
const DY: [i32; 4] = [0, 1, 0, -1];

fn parse_input(text: &str) -> (Vec<Vec<char>>, (usize, usize), (usize, usize)) {
    let grid: Vec<Vec<char>> = text.lines().map(|line| line.chars().collect()).collect();
    let mut start = (0, 0);
    let mut end = (0, 0);

    for (y, row) in grid.iter().enumerate() {
        for (x, &cell) in row.iter().enumerate() {
            if cell == 'S' {
                start = (x, y);
            } else if cell == 'E' {
                end = (x, y);
            }
        }
    }

    (grid, start, end)
}

fn dijkstra_forward(grid: &[Vec<char>], start: (usize, usize)) -> HashMap<(usize, usize, usize), usize> {
    let mut pq = BinaryHeap::new();
    let mut dist = HashMap::new();

    // Start facing East (direction 0)
    pq.push(State {
        cost: 0,
        x: start.0,
        y: start.1,
        dir: 0,
    });

    while let Some(State { cost, x, y, dir }) = pq.pop() {
        let state = (x, y, dir);
        if dist.contains_key(&state) {
            continue;
        }
        dist.insert(state, cost);

        // Move forward
        let nx = x as i32 + DX[dir];
        let ny = y as i32 + DY[dir];
        if ny >= 0 && ny < grid.len() as i32 && nx >= 0 && nx < grid[0].len() as i32 {
            let nx = nx as usize;
            let ny = ny as usize;
            if grid[ny][nx] != '#' {
                pq.push(State {
                    cost: cost + 1,
                    x: nx,
                    y: ny,
                    dir,
                });
            }
        }

        // Turn left
        pq.push(State {
            cost: cost + 1000,
            x,
            y,
            dir: (dir + 3) % 4,
        });

        // Turn right
        pq.push(State {
            cost: cost + 1000,
            x,
            y,
            dir: (dir + 1) % 4,
        });
    }

    dist
}

fn dijkstra_backward(grid: &[Vec<char>], end: (usize, usize)) -> HashMap<(usize, usize, usize), usize> {
    let mut pq = BinaryHeap::new();
    let mut dist = HashMap::new();

    // At end, can arrive facing any direction
    for d in 0..4 {
        pq.push(State {
            cost: 0,
            x: end.0,
            y: end.1,
            dir: d,
        });
    }

    while let Some(State { cost, x, y, dir }) = pq.pop() {
        let state = (x, y, dir);
        if dist.contains_key(&state) {
            continue;
        }
        dist.insert(state, cost);

        // Reverse of "move forward": come from behind
        let px = x as i32 - DX[dir];
        let py = y as i32 - DY[dir];
        if py >= 0 && py < grid.len() as i32 && px >= 0 && px < grid[0].len() as i32 {
            let px = px as usize;
            let py = py as usize;
            if grid[py][px] != '#' {
                pq.push(State {
                    cost: cost + 1,
                    x: px,
                    y: py,
                    dir,
                });
            }
        }

        // Reverse of turn: came from same position with different direction
        pq.push(State {
            cost: cost + 1000,
            x,
            y,
            dir: (dir + 3) % 4,
        });

        pq.push(State {
            cost: cost + 1000,
            x,
            y,
            dir: (dir + 1) % 4,
        });
    }

    dist
}

fn part1(grid: &[Vec<char>], start: (usize, usize), end: (usize, usize)) -> usize {
    let dist = dijkstra_forward(grid, start);

    (0..4)
        .filter_map(|d| dist.get(&(end.0, end.1, d)))
        .min()
        .copied()
        .unwrap_or(usize::MAX)
}

fn part2(grid: &[Vec<char>], start: (usize, usize), end: (usize, usize), best_score: usize) -> usize {
    let dist_from_start = dijkstra_forward(grid, start);
    let dist_to_end = dijkstra_backward(grid, end);

    let mut tiles_on_best_path = HashSet::new();

    for y in 0..grid.len() {
        for x in 0..grid[0].len() {
            if grid[y][x] == '#' {
                continue;
            }

            // Check if this tile is on any optimal path
            for d in 0..4 {
                let from_start = dist_from_start.get(&(x, y, d)).copied().unwrap_or(usize::MAX);
                let to_end = dist_to_end.get(&(x, y, d)).copied().unwrap_or(usize::MAX);

                if from_start != usize::MAX && to_end != usize::MAX && from_start + to_end == best_score {
                    tiles_on_best_path.insert((x, y));
                    break;
                }
            }
        }
    }

    tiles_on_best_path.len()
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    let (grid, start, end) = parse_input(&input);

    let answer1 = part1(&grid, start, end);
    println!("Part 1: {}", answer1);

    let answer2 = part2(&grid, start, end, answer1);
    println!("Part 2: {}", answer2);
}
