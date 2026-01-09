use std::collections::{HashSet, VecDeque};
use std::fs;

fn parse_grid(text: &str) -> (Vec<Vec<char>>, (usize, usize), (usize, usize)) {
    let mut grid: Vec<Vec<char>> = text
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().collect())
        .collect();

    let mut start = (0, 0);
    let mut end = (0, 0);

    for (r, row) in grid.iter_mut().enumerate() {
        for (c, ch) in row.iter_mut().enumerate() {
            if *ch == 'S' {
                start = (r, c);
                *ch = 'a';
            } else if *ch == 'E' {
                end = (r, c);
                *ch = 'z';
            }
        }
    }

    (grid, start, end)
}

fn bfs(grid: &[Vec<char>], starts: &[(usize, usize)], end: (usize, usize)) -> i32 {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut visited: HashSet<(usize, usize)> = HashSet::new();
    let mut queue: VecDeque<(usize, usize, i32)> = VecDeque::new();

    for &start in starts {
        queue.push_back((start.0, start.1, 0));
        visited.insert(start);
    }

    let directions: [(i32, i32); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

    while let Some((r, c, dist)) = queue.pop_front() {
        if (r, c) == end {
            return dist;
        }

        let current_height = grid[r][c] as i32;

        for (dr, dc) in directions {
            let nr = r as i32 + dr;
            let nc = c as i32 + dc;

            if nr >= 0 && nr < rows as i32 && nc >= 0 && nc < cols as i32 {
                let nr = nr as usize;
                let nc = nc as usize;

                if !visited.contains(&(nr, nc)) {
                    let next_height = grid[nr][nc] as i32;
                    // Can move if destination is at most 1 higher
                    if next_height <= current_height + 1 {
                        visited.insert((nr, nc));
                        queue.push_back((nr, nc, dist + 1));
                    }
                }
            }
        }
    }

    -1 // No path found
}

fn part1(text: &str) -> i32 {
    let (grid, start, end) = parse_grid(text);
    bfs(&grid, &[start], end)
}

fn part2(text: &str) -> i32 {
    let (grid, _, end) = parse_grid(text);

    // Find all cells with elevation 'a'
    let mut starts: Vec<(usize, usize)> = Vec::new();
    for (r, row) in grid.iter().enumerate() {
        for (c, ch) in row.iter().enumerate() {
            if *ch == 'a' {
                starts.push((r, c));
            }
        }
    }

    bfs(&grid, &starts, end)
}

fn main() {
    let input_path = std::env::current_dir()
        .unwrap()
        .join("../input.txt");
    let text = fs::read_to_string(input_path).expect("Failed to read input file");

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
