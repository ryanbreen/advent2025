use std::collections::HashMap;
use std::fs;
use std::path::Path;

struct Line {
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
}

fn parse_input() -> Vec<Line> {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let content = fs::read_to_string(&input_path).expect("Failed to read input file");

    content
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let parts: Vec<&str> = line.split(" -> ").collect();
            let start: Vec<i32> = parts[0].split(',').map(|x| x.parse().unwrap()).collect();
            let end: Vec<i32> = parts[1].split(',').map(|x| x.parse().unwrap()).collect();
            Line {
                x1: start[0],
                y1: start[1],
                x2: end[0],
                y2: end[1],
            }
        })
        .collect()
}

fn sign(x: i32) -> i32 {
    match x.cmp(&0) {
        std::cmp::Ordering::Greater => 1,
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
    }
}

fn count_overlaps(lines: &[Line], include_diagonals: bool) -> usize {
    let mut grid: HashMap<(i32, i32), i32> = HashMap::new();

    for line in lines {
        let dx = sign(line.x2 - line.x1);
        let dy = sign(line.y2 - line.y1);

        // Skip diagonals in part 1
        if !include_diagonals && dx != 0 && dy != 0 {
            continue;
        }

        let mut x = line.x1;
        let mut y = line.y1;
        loop {
            *grid.entry((x, y)).or_insert(0) += 1;
            if x == line.x2 && y == line.y2 {
                break;
            }
            x += dx;
            y += dy;
        }
    }

    grid.values().filter(|&&v| v >= 2).count()
}

fn part1(lines: &[Line]) -> usize {
    count_overlaps(lines, false)
}

fn part2(lines: &[Line]) -> usize {
    count_overlaps(lines, true)
}

fn main() {
    let lines = parse_input();
    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
