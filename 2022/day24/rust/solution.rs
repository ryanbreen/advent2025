use std::collections::{HashSet, VecDeque};
use std::fs;
use std::path::Path;

#[derive(Clone, Copy)]
struct Blizzard {
    row: i32,
    col: i32,
    direction: char,
}

fn parse_input(text: &str) -> (Vec<Blizzard>, i32, i32, i32, i32, (i32, i32), (i32, i32)) {
    let lines: Vec<&str> = text.trim().lines().collect();
    let height = lines.len() as i32;
    let width = lines[0].len() as i32;

    let inner_h = height - 2;
    let inner_w = width - 2;

    let mut blizzards = Vec::new();
    for (r, line) in lines.iter().enumerate() {
        for (c, ch) in line.chars().enumerate() {
            if ch == '^' || ch == 'v' || ch == '<' || ch == '>' {
                blizzards.push(Blizzard {
                    row: r as i32,
                    col: c as i32,
                    direction: ch,
                });
            }
        }
    }

    let start_col = lines[0].chars().position(|c| c == '.').unwrap() as i32;
    let end_col = lines[lines.len() - 1].chars().position(|c| c == '.').unwrap() as i32;

    let start = (0, start_col);
    let end = (height - 1, end_col);

    (blizzards, height, width, inner_h, inner_w, start, end)
}

fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 { a } else { gcd(b, a % b) }
}

fn lcm(a: i32, b: i32) -> i32 {
    a * b / gcd(a, b)
}

fn get_blizzard_positions(blizzards: &[Blizzard], inner_h: i32, inner_w: i32, time: i32) -> HashSet<(i32, i32)> {
    let mut positions = HashSet::new();

    for blizzard in blizzards {
        let ir = blizzard.row - 1;
        let ic = blizzard.col - 1;

        let (nr, nc) = match blizzard.direction {
            '^' => (((ir - time) % inner_h + inner_h) % inner_h, ic),
            'v' => ((ir + time) % inner_h, ic),
            '<' => (ir, ((ic - time) % inner_w + inner_w) % inner_w),
            '>' => (ir, (ic + time) % inner_w),
            _ => unreachable!(),
        };

        positions.insert((nr + 1, nc + 1));
    }

    positions
}

fn bfs(
    blizzards: &[Blizzard],
    height: i32,
    width: i32,
    inner_h: i32,
    inner_w: i32,
    start: (i32, i32),
    end: (i32, i32),
    start_time: i32,
) -> i32 {
    let period = lcm(inner_h, inner_w);

    // Precompute blizzard positions for all times in one period
    let mut blizzard_cache: Vec<HashSet<(i32, i32)>> = Vec::with_capacity(period as usize);
    for t in 0..period {
        blizzard_cache.push(get_blizzard_positions(blizzards, inner_h, inner_w, t));
    }

    // BFS: state = (time, row, col)
    let mut queue = VecDeque::new();
    queue.push_back((start_time, start.0, start.1));

    let mut visited = HashSet::new();
    visited.insert((start_time % period, start.0, start.1));

    let directions = [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]; // wait, up, down, left, right

    while let Some((time, r, c)) = queue.pop_front() {
        if (r, c) == end {
            return time;
        }

        let next_time = time + 1;
        let next_blizzards = &blizzard_cache[(next_time % period) as usize];

        for (dr, dc) in directions.iter() {
            let nr = r + dr;
            let nc = c + dc;

            // Check bounds
            let valid_pos = if (nr, nc) == start || (nr, nc) == end {
                true
            } else if nr <= 0 || nr >= height - 1 || nc <= 0 || nc >= width - 1 {
                false
            } else {
                true
            };

            if !valid_pos {
                continue;
            }

            // Check blizzards
            if next_blizzards.contains(&(nr, nc)) {
                continue;
            }

            let state = (next_time % period, nr, nc);
            if !visited.contains(&state) {
                visited.insert(state);
                queue.push_back((next_time, nr, nc));
            }
        }
    }

    -1 // No path found
}

fn part1(text: &str) -> i32 {
    let (blizzards, height, width, inner_h, inner_w, start, end) = parse_input(text);
    bfs(&blizzards, height, width, inner_h, inner_w, start, end, 0)
}

fn part2(text: &str) -> i32 {
    let (blizzards, height, width, inner_h, inner_w, start, end) = parse_input(text);

    // Trip 1: start to end
    let t1 = bfs(&blizzards, height, width, inner_h, inner_w, start, end, 0);

    // Trip 2: end to start
    let t2 = bfs(&blizzards, height, width, inner_h, inner_w, end, start, t1);

    // Trip 3: start to end again
    let t3 = bfs(&blizzards, height, width, inner_h, inner_w, start, end, t2);

    t3
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    // Try multiple possible input locations
    let input_file = if input_path.exists() {
        input_path
    } else {
        Path::new("../input.txt").to_path_buf()
    };

    let text = fs::read_to_string(&input_file)
        .expect("Could not read input file");

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
