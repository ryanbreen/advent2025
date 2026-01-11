use std::fs;
use std::path::Path;

fn parse_input() -> Vec<String> {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    fs::read_to_string(input_path)
        .expect("Failed to read input file")
        .lines()
        .filter(|line| !line.is_empty())
        .map(|s| s.to_string())
        .collect()
}

fn part1(numbers: &[String]) -> u32 {
    let num_bits = numbers[0].len();
    let mut gamma: u32 = 0;

    for pos in 0..num_bits {
        let ones: usize = numbers
            .iter()
            .filter(|n| n.chars().nth(pos) == Some('1'))
            .count();
        let zeros = numbers.len() - ones;

        if ones >= zeros {
            gamma |= 1 << (num_bits - 1 - pos);
        }
    }

    // epsilon is bitwise NOT of gamma (within num_bits)
    let epsilon = gamma ^ ((1 << num_bits) - 1);

    gamma * epsilon
}

fn find_rating(numbers: &[String], use_most_common: bool) -> u32 {
    let num_bits = numbers[0].len();
    let mut candidates: Vec<&String> = numbers.iter().collect();

    for pos in 0..num_bits {
        if candidates.len() == 1 {
            break;
        }

        let ones: usize = candidates
            .iter()
            .filter(|n| n.chars().nth(pos) == Some('1'))
            .count();
        let zeros = candidates.len() - ones;

        let target = if use_most_common {
            if ones >= zeros { '1' } else { '0' }
        } else {
            if zeros <= ones { '0' } else { '1' }
        };

        candidates = candidates
            .into_iter()
            .filter(|n| n.chars().nth(pos) == Some(target))
            .collect();
    }

    u32::from_str_radix(candidates[0], 2).expect("Invalid binary string")
}

fn part2(numbers: &[String]) -> u32 {
    let oxygen = find_rating(numbers, true);
    let co2 = find_rating(numbers, false);
    oxygen * co2
}

fn main() {
    let numbers = parse_input();
    println!("Part 1: {}", part1(&numbers));
    println!("Part 2: {}", part2(&numbers));
}
