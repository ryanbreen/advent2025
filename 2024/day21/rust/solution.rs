use std::collections::HashMap;
use std::fs;

type Position = (i32, i32);

// Keypad layouts
fn numeric_keypad() -> HashMap<char, Position> {
    [
        ('7', (0, 0)), ('8', (0, 1)), ('9', (0, 2)),
        ('4', (1, 0)), ('5', (1, 1)), ('6', (1, 2)),
        ('1', (2, 0)), ('2', (2, 1)), ('3', (2, 2)),
        ('0', (3, 1)), ('A', (3, 2)),
    ]
    .iter()
    .cloned()
    .collect()
}

fn directional_keypad() -> HashMap<char, Position> {
    [
        ('^', (0, 1)), ('A', (0, 2)),
        ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2)),
    ]
    .iter()
    .cloned()
    .collect()
}

const NUMERIC_GAP: Position = (3, 0);
const DIRECTIONAL_GAP: Position = (0, 0);

/// Find all shortest paths from start to end, avoiding gap
fn shortest_paths(
    keypad: &HashMap<char, Position>,
    gap: Position,
    start: char,
    end: char,
) -> Vec<String> {
    let (sr, sc) = keypad[&start];
    let (er, ec) = keypad[&end];

    let mut paths = Vec::new();

    fn dfs(
        r: i32,
        c: i32,
        er: i32,
        ec: i32,
        gap: Position,
        path: String,
        paths: &mut Vec<String>,
    ) {
        if (r, c) == gap {
            return;
        }
        if (r, c) == (er, ec) {
            paths.push(path);
            return;
        }
        // Move vertically toward target
        if r < er {
            dfs(r + 1, c, er, ec, gap, path.clone() + "v", paths);
        } else if r > er {
            dfs(r - 1, c, er, ec, gap, path.clone() + "^", paths);
        }
        // Move horizontally toward target
        if c < ec {
            dfs(r, c + 1, er, ec, gap, path.clone() + ">", paths);
        } else if c > ec {
            dfs(r, c - 1, er, ec, gap, path + "<", paths);
        }
    }

    dfs(sr, sc, er, ec, gap, String::new(), &mut paths);

    if paths.is_empty() {
        vec![String::new()] // Empty path if start == end
    } else {
        paths
    }
}

/// Minimum presses needed to move from from_char to to_char and press, at given depth
fn min_presses_for_move(
    from_char: char,
    to_char: char,
    depth: u32,
    is_numeric: bool,
    cache: &mut HashMap<(char, char, u32, bool), u64>,
) -> u64 {
    let key = (from_char, to_char, depth, is_numeric);
    if let Some(&cached) = cache.get(&key) {
        return cached;
    }

    let (keypad, gap) = if is_numeric {
        (numeric_keypad(), NUMERIC_GAP)
    } else {
        (directional_keypad(), DIRECTIONAL_GAP)
    };

    let paths = shortest_paths(&keypad, gap, from_char, to_char);

    let result = if depth == 0 {
        // At human level, just return path length + 1 for 'A' press
        paths.iter().map(|p| p.len() as u64).min().unwrap() + 1
    } else {
        let mut best = u64::MAX;
        for path in paths {
            // Need to type path + 'A' on the directional keypad above
            let sequence = path + "A";
            let mut cost = 0u64;
            let mut current = 'A';
            for ch in sequence.chars() {
                cost += min_presses_for_move(current, ch, depth - 1, false, cache);
                current = ch;
            }
            best = best.min(cost);
        }
        best
    };

    cache.insert(key, result);
    result
}

/// Compute minimum presses to type code on numeric keypad with given robot depth
fn solve_code(code: &str, depth: u32, cache: &mut HashMap<(char, char, u32, bool), u64>) -> u64 {
    let mut total = 0u64;
    let mut current = 'A';
    for ch in code.chars() {
        total += min_presses_for_move(current, ch, depth, true, cache);
        current = ch;
    }
    total
}

/// Compute complexity: length * numeric part of code
fn complexity(code: &str, length: u64) -> u64 {
    let numeric_part: u64 = code
        .trim_end_matches('A')
        .parse()
        .unwrap();
    length * numeric_part
}

fn part1(codes: &[String]) -> u64 {
    let mut cache = HashMap::new();
    let mut total = 0;
    for code in codes {
        let length = solve_code(code, 2, &mut cache);
        total += complexity(code, length);
    }
    total
}

fn part2(codes: &[String]) -> u64 {
    let mut cache = HashMap::new();
    let mut total = 0;
    for code in codes {
        let length = solve_code(code, 25, &mut cache);
        total += complexity(code, length);
    }
    total
}

fn main() {
    let input = fs::read_to_string("../input.txt")
        .expect("Failed to read input.txt");

    let codes: Vec<String> = input
        .lines()
        .map(|line| line.trim().to_string())
        .filter(|line| !line.is_empty())
        .collect();

    println!("Part 1: {}", part1(&codes));
    println!("Part 2: {}", part2(&codes));
}
