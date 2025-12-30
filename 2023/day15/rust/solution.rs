use std::fs;

/// Compute the HASH algorithm on a string
fn hash(s: &str) -> usize {
    s.bytes().fold(0, |acc, b| ((acc + b as usize) * 17) % 256)
}

/// Parse a step into (label, operation, focal_length)
/// operation is '-' for remove, '=' for add/replace
fn parse_step(step: &str) -> (&str, char, Option<u8>) {
    if let Some(label) = step.strip_suffix('-') {
        (label, '-', None)
    } else {
        let parts: Vec<&str> = step.split('=').collect();
        let label = parts[0];
        let focal_length = parts[1].parse::<u8>().unwrap();
        (label, '=', Some(focal_length))
    }
}

/// Part 1: Sum of HASH values for all steps
fn part1(input: &str) -> usize {
    input
        .trim()
        .split(',')
        .map(|step| hash(step))
        .sum()
}

/// Part 2: HASHMAP procedure and focusing power calculation
fn part2(input: &str) -> usize {
    // 256 boxes, each containing an ordered list of (label, focal_length)
    let mut boxes: Vec<Vec<(&str, u8)>> = vec![Vec::new(); 256];

    for step in input.trim().split(',') {
        let (label, op, focal_length) = parse_step(step);
        let box_num = hash(label);

        match op {
            '-' => {
                // Remove lens with matching label
                boxes[box_num].retain(|(l, _)| *l != label);
            }
            '=' => {
                let fl = focal_length.unwrap();
                // Check if lens with same label exists
                if let Some(pos) = boxes[box_num].iter().position(|(l, _)| *l == label) {
                    // Replace existing lens
                    boxes[box_num][pos] = (label, fl);
                } else {
                    // Add new lens at end
                    boxes[box_num].push((label, fl));
                }
            }
            _ => unreachable!(),
        }
    }

    // Calculate focusing power
    boxes
        .iter()
        .enumerate()
        .map(|(box_num, lenses)| {
            lenses
                .iter()
                .enumerate()
                .map(|(slot, (_, focal_length))| {
                    (box_num + 1) * (slot + 1) * (*focal_length as usize)
                })
                .sum::<usize>()
        })
        .sum()
}

fn main() {
    let input = fs::read_to_string("../input.txt").expect("Failed to read input file");

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
