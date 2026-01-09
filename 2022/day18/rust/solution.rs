use std::collections::{HashSet, VecDeque};
use std::fs;
use std::path::Path;

type Coord = (i32, i32, i32);

// 6 directions: +x, -x, +y, -y, +z, -z
const DIRECTIONS: [Coord; 6] = [
    (1, 0, 0), (-1, 0, 0),
    (0, 1, 0), (0, -1, 0),
    (0, 0, 1), (0, 0, -1),
];

fn parse_input(text: &str) -> HashSet<Coord> {
    text.trim()
        .lines()
        .map(|line| {
            let parts: Vec<i32> = line.split(',')
                .map(|s| s.parse().unwrap())
                .collect();
            (parts[0], parts[1], parts[2])
        })
        .collect()
}

fn part1(cubes: &HashSet<Coord>) -> i32 {
    let mut surface_area = 0;

    for &(x, y, z) in cubes {
        for &(dx, dy, dz) in &DIRECTIONS {
            if !cubes.contains(&(x + dx, y + dy, z + dz)) {
                surface_area += 1;
            }
        }
    }

    surface_area
}

fn part2(cubes: &HashSet<Coord>) -> i32 {
    // Find bounding box with 1 unit padding
    let min_x = cubes.iter().map(|c| c.0).min().unwrap() - 1;
    let max_x = cubes.iter().map(|c| c.0).max().unwrap() + 1;
    let min_y = cubes.iter().map(|c| c.1).min().unwrap() - 1;
    let max_y = cubes.iter().map(|c| c.1).max().unwrap() + 1;
    let min_z = cubes.iter().map(|c| c.2).min().unwrap() - 1;
    let max_z = cubes.iter().map(|c| c.2).max().unwrap() + 1;

    // BFS to find all exterior air cells
    let mut exterior: HashSet<Coord> = HashSet::new();
    let mut queue: VecDeque<Coord> = VecDeque::new();

    let start = (min_x, min_y, min_z);
    exterior.insert(start);
    queue.push_back(start);

    while let Some((x, y, z)) = queue.pop_front() {
        for &(dx, dy, dz) in &DIRECTIONS {
            let (nx, ny, nz) = (x + dx, y + dy, z + dz);

            // Stay within bounds
            if nx < min_x || nx > max_x ||
               ny < min_y || ny > max_y ||
               nz < min_z || nz > max_z {
                continue;
            }

            let neighbor = (nx, ny, nz);

            // Skip cubes and already visited
            if cubes.contains(&neighbor) || exterior.contains(&neighbor) {
                continue;
            }

            exterior.insert(neighbor);
            queue.push_back(neighbor);
        }
    }

    // Count faces touching exterior air
    let mut surface_area = 0;
    for &(x, y, z) in cubes {
        for &(dx, dy, dz) in &DIRECTIONS {
            if exterior.contains(&(x + dx, y + dy, z + dz)) {
                surface_area += 1;
            }
        }
    }

    surface_area
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    // Try multiple paths to find input
    let text = if Path::new(&input_path).exists() {
        fs::read_to_string(&input_path).unwrap()
    } else if Path::new("../input.txt").exists() {
        fs::read_to_string("../input.txt").unwrap()
    } else {
        fs::read_to_string("input.txt").unwrap()
    };

    let cubes = parse_input(&text);

    println!("Part 1: {}", part1(&cubes));
    println!("Part 2: {}", part2(&cubes));
}
