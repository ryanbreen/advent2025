use std::fs;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug)]
enum Direction {
    Left,
    Right,
}

#[derive(Debug)]
struct Rotation {
    direction: Direction,
    distance: i32,
}

impl Rotation {
    fn parse(line: &str) -> Result<Self, String> {
        let line = line.trim();
        if line.is_empty() {
            return Err("Empty line".to_string());
        }

        let direction = match line.chars().next() {
            Some('L') => Direction::Left,
            Some('R') => Direction::Right,
            _ => return Err(format!("Invalid direction: {}", line)),
        };

        let distance: i32 = line[1..]
            .parse()
            .map_err(|e| format!("Invalid distance: {}", e))?;

        Ok(Rotation { direction, distance })
    }
}

fn solve_part1(rotations: &[Rotation]) -> i32 {
    let mut position = 50;
    let mut count = 0;

    for rotation in rotations {
        match rotation.direction {
            Direction::Left => {
                position = (position - rotation.distance).rem_euclid(100);
            }
            Direction::Right => {
                position = (position + rotation.distance) % 100;
            }
        }

        if position == 0 {
            count += 1;
        }
    }

    count
}

fn solve_part2(rotations: &[Rotation]) -> i32 {
    let mut position = 50;
    let mut count = 0;

    for rotation in rotations {
        let old_position = position;

        match rotation.direction {
            Direction::Left => {
                position = (position - rotation.distance).rem_euclid(100);
                // Count how many times we pass through 0 during rotation
                // Going left from old_position, we need to count 0 crossings
                // We cross 0 if: distance > old_position (wraps from 0 to 99)
                // Exception: if we START at 0, we don't count it
                if old_position == 0 {
                    // Starting from 0, going left: 0 -> 99 -> 98 -> ...
                    // We count how many full wraps we make (each wrap passes through 0)
                    count += rotation.distance / 100;
                } else {
                    // Not starting from 0
                    // If distance >= old_position, we reach 0
                    // Then we count additional wraps
                    if rotation.distance >= old_position {
                        let remaining = rotation.distance - old_position;
                        count += 1 + (remaining / 100);
                    }
                }
            }
            Direction::Right => {
                position = (position + rotation.distance) % 100;
                // Count how many times we pass through 0 during rotation
                // Going right from old_position, we need to count 0 crossings
                // We cross 0 if: old_position + distance >= 100
                // Exception: if we START at 0, we don't count it
                if old_position == 0 {
                    // Starting from 0, going right: 0 -> 1 -> 2 -> ...
                    // We count how many full wraps we make
                    count += rotation.distance / 100;
                } else {
                    // Not starting from 0
                    // If we reach or pass 100, we hit 0
                    let steps_to_zero = 100 - old_position;
                    if rotation.distance >= steps_to_zero {
                        let remaining = rotation.distance - steps_to_zero;
                        count += 1 + (remaining / 100);
                    }
                }
            }
        }
    }

    count
}

fn main() -> io::Result<()> {
    let input_path = Path::new("/Users/wrb/fun/code/advent2025/day01/input.txt");
    let file = fs::File::open(input_path)?;
    let reader = io::BufReader::new(file);

    let rotations: Vec<Rotation> = reader
        .lines()
        .filter_map(|line| line.ok())
        .filter_map(|line| Rotation::parse(&line).ok())
        .collect();

    println!("Parsed {} rotations", rotations.len());

    let part1_answer = solve_part1(&rotations);
    println!("Part 1: {}", part1_answer);

    let part2_answer = solve_part2(&rotations);
    println!("Part 2: {}", part2_answer);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example() {
        let input = vec![
            "L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82",
        ];

        let rotations: Vec<Rotation> = input
            .iter()
            .map(|s| Rotation::parse(s).unwrap())
            .collect();

        assert_eq!(solve_part1(&rotations), 3);
        assert_eq!(solve_part2(&rotations), 6);
    }

    #[test]
    fn test_r1000_from_50() {
        // R1000 from position 50 should pass through 0 exactly 10 times
        let rotation = Rotation::parse("R1000").unwrap();
        let result = solve_part2(&[rotation]);
        assert_eq!(result, 10);
    }
}
