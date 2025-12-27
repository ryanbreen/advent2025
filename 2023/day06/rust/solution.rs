use std::fs;
use std::path::Path;

fn count_ways_to_win(time: u64, record: u64) -> u64 {
    // If we hold the button for t ms, we travel t * (time - t) mm.
    // We need: t * (time - t) > record
    // Solving: -t^2 + time*t - record > 0
    // Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2

    let time_f = time as f64;
    let record_f = record as f64;

    let discriminant = time_f * time_f - 4.0 * record_f;
    if discriminant <= 0.0 {
        return 0;
    }

    let sqrt_d = discriminant.sqrt();
    let t_low = (time_f - sqrt_d) / 2.0;
    let t_high = (time_f + sqrt_d) / 2.0;

    // We need integer values strictly between the roots
    let first = t_low.floor() as u64 + 1;
    let last = t_high.ceil() as u64 - 1;

    if last < first {
        return 0;
    }
    last - first + 1
}

fn parse_races(input: &str) -> Vec<(u64, u64)> {
    let lines: Vec<&str> = input.lines().collect();

    let times: Vec<u64> = lines[0]
        .split(':')
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();

    let distances: Vec<u64> = lines[1]
        .split(':')
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();

    times.into_iter().zip(distances).collect()
}

fn part1(input: &str) -> u64 {
    let races = parse_races(input);
    races
        .iter()
        .map(|&(time, record)| count_ways_to_win(time, record))
        .product()
}

fn part2(input: &str) -> u64 {
    let races = parse_races(input);

    // Concatenate all times and distances into single numbers
    let time_str: String = races.iter().map(|(t, _)| t.to_string()).collect();
    let distance_str: String = races.iter().map(|(_, d)| d.to_string()).collect();

    let time: u64 = time_str.parse().unwrap();
    let record: u64 = distance_str.parse().unwrap();

    count_ways_to_win(time, record)
}

fn main() {
    let input_path = Path::new(file!()).parent().unwrap().join("../input.txt");
    let input = fs::read_to_string(&input_path)
        .expect("Failed to read input file")
        .trim()
        .to_string();

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
