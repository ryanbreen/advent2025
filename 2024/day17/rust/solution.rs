//! Day 17: Chronospatial Computer - 3-bit VM emulator

use std::fs;

/// Parse the input file and return (A, B, C, program)
fn parse_input(text: &str) -> (u64, u64, u64, Vec<u64>) {
    let lines: Vec<&str> = text.trim().lines().collect();

    let a = lines[0]
        .split(": ")
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();
    let b = lines[1]
        .split(": ")
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();
    let c = lines[2]
        .split(": ")
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();

    let program: Vec<u64> = lines[4]
        .split(": ")
        .nth(1)
        .unwrap()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    (a, b, c, program)
}

/// Execute the 3-bit computer program and return output
fn run_program(mut a: u64, mut b: u64, mut c: u64, program: &[u64]) -> Vec<u64> {
    let mut ip: usize = 0;
    let mut output = Vec::new();

    // Get combo operand value
    let combo = |operand: u64, a: u64, b: u64, c: u64| -> u64 {
        match operand {
            0..=3 => operand,
            4 => a,
            5 => b,
            6 => c,
            _ => panic!("Invalid combo operand: {}", operand),
        }
    };

    while ip < program.len() {
        let opcode = program[ip];
        let operand = program[ip + 1];

        match opcode {
            0 => {
                // adv - A = A >> combo
                a >>= combo(operand, a, b, c);
            }
            1 => {
                // bxl - B = B XOR literal
                b ^= operand;
            }
            2 => {
                // bst - B = combo % 8
                b = combo(operand, a, b, c) & 7;
            }
            3 => {
                // jnz - jump if A != 0
                if a != 0 {
                    ip = operand as usize;
                    continue;
                }
            }
            4 => {
                // bxc - B = B XOR C
                b ^= c;
            }
            5 => {
                // out - output combo % 8
                output.push(combo(operand, a, b, c) & 7);
            }
            6 => {
                // bdv - B = A >> combo
                b = a >> combo(operand, a, b, c);
            }
            7 => {
                // cdv - C = A >> combo
                c = a >> combo(operand, a, b, c);
            }
            _ => panic!("Invalid opcode: {}", opcode),
        }

        ip += 2;
    }

    output
}

/// Part 1: Run the program and return comma-separated output
fn part1(text: &str) -> String {
    let (a, b, c, program) = parse_input(text);
    let output = run_program(a, b, c, &program);
    output
        .iter()
        .map(|n| n.to_string())
        .collect::<Vec<_>>()
        .join(",")
}

/// Part 2: Find initial A value that makes program output itself
fn part2(text: &str) -> u64 {
    let (_, b, c, program) = parse_input(text);

    // The program loops, outputting one digit per iteration, dividing A by 8 each time.
    // We need to find A such that output == program.
    // Work backwards from the last digit - build A 3 bits at a time.

    fn search(target_idx: i64, current_a: u64, b: u64, c: u64, program: &[u64]) -> Option<u64> {
        if target_idx < 0 {
            return Some(current_a);
        }

        // Try all 8 possible 3-bit values for this position
        for bits in 0..8u64 {
            let candidate_a = (current_a << 3) | bits;

            // A can't be 0 at start (would halt immediately without output)
            if candidate_a == 0 && target_idx == (program.len() - 1) as i64 {
                continue;
            }

            let output = run_program(candidate_a, b, c, program);

            // Check if output matches the suffix of the program
            let expected = &program[target_idx as usize..];
            if output.as_slice() == expected {
                if let Some(result) = search(target_idx - 1, candidate_a, b, c, program) {
                    return Some(result);
                }
            }
        }

        None
    }

    search((program.len() - 1) as i64, 0, b, c, &program).unwrap()
}

fn main() {
    let text = fs::read_to_string("../input.txt")
        .expect("Failed to read input file");

    println!("Part 1: {}", part1(&text));
    println!("Part 2: {}", part2(&text));
}
