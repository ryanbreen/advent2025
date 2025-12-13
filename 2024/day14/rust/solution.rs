use std::collections::HashSet;
use std::fs;

const WIDTH: i32 = 101;
const HEIGHT: i32 = 103;

#[derive(Debug, Clone, Copy)]
struct Robot {
    px: i32,
    py: i32,
    vx: i32,
    vy: i32,
}

fn parse_robots(text: &str) -> Vec<Robot> {
    let mut robots = Vec::new();

    for line in text.lines() {
        // Parse format: p=x,y v=vx,vy
        if let Some(p_start) = line.find("p=") {
            if let Some(v_start) = line.find(" v=") {
                let p_part = &line[p_start + 2..v_start];
                let v_part = &line[v_start + 3..];

                if let Some(comma_p) = p_part.find(',') {
                    if let Some(comma_v) = v_part.find(',') {
                        let px = p_part[..comma_p].parse::<i32>().ok();
                        let py = p_part[comma_p + 1..].parse::<i32>().ok();
                        let vx = v_part[..comma_v].parse::<i32>().ok();
                        let vy = v_part[comma_v + 1..].parse::<i32>().ok();

                        if let (Some(px), Some(py), Some(vx), Some(vy)) = (px, py, vx, vy) {
                            robots.push(Robot { px, py, vx, vy });
                        }
                    }
                }
            }
        }
    }

    robots
}

fn simulate(robots: &[Robot], seconds: i32) -> Vec<(i32, i32)> {
    robots
        .iter()
        .map(|robot| {
            // Use rem_euclid for proper modular arithmetic with negative numbers
            let new_x = (robot.px + robot.vx * seconds).rem_euclid(WIDTH);
            let new_y = (robot.py + robot.vy * seconds).rem_euclid(HEIGHT);
            (new_x, new_y)
        })
        .collect()
}

fn count_quadrants(positions: &[(i32, i32)]) -> (i32, i32, i32, i32) {
    let mid_x = WIDTH / 2;   // 50
    let mid_y = HEIGHT / 2;  // 51

    let mut q1 = 0;
    let mut q2 = 0;
    let mut q3 = 0;
    let mut q4 = 0;

    for &(x, y) in positions {
        // Skip robots on middle lines
        if x == mid_x || y == mid_y {
            continue;
        }

        if x < mid_x && y < mid_y {
            q1 += 1;  // Top-left
        } else if x > mid_x && y < mid_y {
            q2 += 1;  // Top-right
        } else if x < mid_x && y > mid_y {
            q3 += 1;  // Bottom-left
        } else {
            q4 += 1;  // Bottom-right
        }
    }

    (q1, q2, q3, q4)
}

fn part1(robots: &[Robot]) -> i32 {
    let positions = simulate(robots, 100);
    let (q1, q2, q3, q4) = count_quadrants(&positions);
    q1 * q2 * q3 * q4
}

fn part2(robots: &[Robot]) -> i32 {
    // The Christmas tree appears when robots cluster together
    // Look for a frame with a long horizontal line of robots (tree base/border)
    for seconds in 1..=(WIDTH * HEIGHT) {
        let positions = simulate(robots, seconds);
        let pos_set: HashSet<(i32, i32)> = positions.into_iter().collect();

        // Look for a horizontal line of at least 20 consecutive robots
        for y in 0..HEIGHT {
            let mut max_consecutive = 0;
            let mut consecutive = 0;

            for x in 0..WIDTH {
                if pos_set.contains(&(x, y)) {
                    consecutive += 1;
                    max_consecutive = max_consecutive.max(consecutive);
                } else {
                    consecutive = 0;
                }
            }

            if max_consecutive >= 20 {
                return seconds;
            }
        }
    }

    -1
}

fn main() {
    let input_text = fs::read_to_string("../input.txt")
        .expect("Failed to read input.txt");

    let robots = parse_robots(&input_text);

    println!("Part 1: {}", part1(&robots));
    println!("Part 2: {}", part2(&robots));
}
