use std::collections::HashSet;
use std::fs;
use std::path::Path;

fn parse_paths(text: &str) -> HashSet<(i32, i32)> {
    let mut rocks = HashSet::new();

    for line in text.trim().lines() {
        let points: Vec<(i32, i32)> = line
            .split(" -> ")
            .map(|p| {
                let parts: Vec<i32> = p.split(',').map(|n| n.parse().unwrap()).collect();
                (parts[0], parts[1])
            })
            .collect();

        for i in 0..points.len() - 1 {
            let (x1, y1) = points[i];
            let (x2, y2) = points[i + 1];

            if x1 == x2 {
                // Vertical line
                let (min_y, max_y) = if y1 < y2 { (y1, y2) } else { (y2, y1) };
                for y in min_y..=max_y {
                    rocks.insert((x1, y));
                }
            } else {
                // Horizontal line
                let (min_x, max_x) = if x1 < x2 { (x1, x2) } else { (x2, x1) };
                for x in min_x..=max_x {
                    rocks.insert((x, y1));
                }
            }
        }
    }

    rocks
}

fn simulate_sand(blocked: &HashSet<(i32, i32)>, max_y: i32, floor: bool) -> Option<(i32, i32)> {
    let (mut x, mut y) = (500, 0);

    loop {
        // Check if sand has fallen below all rocks (into abyss)
        if !floor && y > max_y {
            return None;
        }

        // Try to move down
        if floor && y + 1 == max_y + 2 {
            // Hit the floor
            return Some((x, y));
        } else if !blocked.contains(&(x, y + 1)) {
            y += 1;
        } else if !blocked.contains(&(x - 1, y + 1)) {
            // Try to move down-left
            x -= 1;
            y += 1;
        } else if !blocked.contains(&(x + 1, y + 1)) {
            // Try to move down-right
            x += 1;
            y += 1;
        } else {
            // Sand comes to rest
            return Some((x, y));
        }
    }
}

fn part1(text: &str) -> usize {
    let rocks = parse_paths(text);
    let max_y = rocks.iter().map(|(_, y)| *y).max().unwrap();
    let mut blocked = rocks.clone();
    let mut count = 0;

    loop {
        match simulate_sand(&blocked, max_y, false) {
            None => break,
            Some(pos) => {
                blocked.insert(pos);
                count += 1;
            }
        }
    }

    count
}

fn part2(text: &str) -> usize {
    let rocks = parse_paths(text);
    let max_y = rocks.iter().map(|(_, y)| *y).max().unwrap();
    let mut blocked = rocks.clone();
    let mut count = 0;

    loop {
        let pos = simulate_sand(&blocked, max_y, true).unwrap();
        blocked.insert(pos);
        count += 1;
        if pos == (500, 0) {
            break;
        }
    }

    count
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let dir = exe_path.parent().unwrap();
    let input_path = dir.join("..").join("input.txt");

    let text = fs::read_to_string(&input_path)
        .unwrap_or_else(|_| {
            // Fallback: try relative to current directory
            let alt_path = Path::new("../input.txt");
            fs::read_to_string(alt_path).expect("Could not read input.txt")
        });

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
