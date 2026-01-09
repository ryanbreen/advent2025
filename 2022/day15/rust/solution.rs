use std::fs;
use std::path::Path;

#[derive(Debug)]
struct Sensor {
    sx: i64,
    sy: i64,
    bx: i64,
    by: i64,
    dist: i64,
}

fn parse_sensors(text: &str) -> Vec<Sensor> {
    let mut sensors = Vec::new();

    for line in text.lines() {
        if line.is_empty() {
            continue;
        }

        // Extract all numbers (including negative) from the line
        let numbers: Vec<i64> = line
            .split(|c: char| !c.is_ascii_digit() && c != '-')
            .filter(|s| !s.is_empty())
            .filter_map(|s| s.parse().ok())
            .collect();

        if numbers.len() >= 4 {
            let sx = numbers[0];
            let sy = numbers[1];
            let bx = numbers[2];
            let by = numbers[3];
            let dist = (sx - bx).abs() + (sy - by).abs();
            sensors.push(Sensor { sx, sy, bx, by, dist });
        }
    }

    sensors
}

fn get_coverage_at_row(sensors: &[Sensor], row: i64) -> Vec<(i64, i64)> {
    let mut ranges = Vec::new();

    for sensor in sensors {
        let row_dist = (sensor.sy - row).abs();
        if row_dist > sensor.dist {
            continue;
        }

        let x_spread = sensor.dist - row_dist;
        ranges.push((sensor.sx - x_spread, sensor.sx + x_spread));
    }

    merge_ranges(&mut ranges)
}

fn merge_ranges(ranges: &mut Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    if ranges.is_empty() {
        return Vec::new();
    }

    ranges.sort();
    let mut merged = vec![ranges[0]];

    for &(start, end) in &ranges[1..] {
        let last = merged.last_mut().unwrap();
        if start <= last.1 + 1 {
            last.1 = last.1.max(end);
        } else {
            merged.push((start, end));
        }
    }

    merged
}

fn part1(sensors: &[Sensor]) -> i64 {
    let target_row = 2000000;
    let ranges = get_coverage_at_row(sensors, target_row);

    // Count total coverage
    let total: i64 = ranges.iter().map(|(start, end)| end - start + 1).sum();

    // Subtract beacons on this row
    let mut beacons_on_row: Vec<i64> = sensors
        .iter()
        .filter(|s| s.by == target_row)
        .map(|s| s.bx)
        .collect();
    beacons_on_row.sort();
    beacons_on_row.dedup();

    total - beacons_on_row.len() as i64
}

fn part2(sensors: &[Sensor]) -> i64 {
    let max_coord = 4000000;

    for row in 0..=max_coord {
        let ranges = get_coverage_at_row(sensors, row);

        // Clip ranges to search area
        let mut clipped: Vec<(i64, i64)> = ranges
            .iter()
            .filter(|&&(start, end)| end >= 0 && start <= max_coord)
            .map(|&(start, end)| (start.max(0), end.min(max_coord)))
            .collect();

        let clipped = merge_ranges(&mut clipped);

        // Check if full row is covered
        if clipped.len() == 1 && clipped[0].0 == 0 && clipped[0].1 == max_coord {
            continue;
        }

        // Found a gap - the beacon is in the gap
        let x = if clipped.len() > 1 {
            clipped[0].1 + 1
        } else if clipped[0].0 > 0 {
            0
        } else {
            clipped[0].1 + 1
        };

        return x * 4000000 + row;
    }

    0
}

fn main() {
    let exe_path = std::env::current_exe().unwrap();
    let exe_dir = exe_path.parent().unwrap();
    let input_path = exe_dir.join("../input.txt");

    // Try relative path first, then fall back to finding input.txt
    let text = if input_path.exists() {
        fs::read_to_string(&input_path).unwrap()
    } else {
        // Try from current directory
        let alt_path = Path::new("../input.txt");
        if alt_path.exists() {
            fs::read_to_string(alt_path).unwrap()
        } else {
            // Try looking in expected location
            fs::read_to_string("/Users/wrb/fun/code/agenticadvent/2022/day15/input.txt").unwrap()
        }
    };

    let sensors = parse_sensors(&text);

    println!("Part 1: {}", part1(&sensors));
    println!("Part 2: {}", part2(&sensors));
}
