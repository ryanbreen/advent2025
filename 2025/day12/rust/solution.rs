use std::collections::HashMap;
use std::fs;

type ShapeSizes = HashMap<usize, usize>;
type Region = (usize, usize, Vec<usize>);

/// Parse input into shapes (as cell counts) and regions
fn parse_input(text: &str) -> (ShapeSizes, Vec<Region>) {
    let mut shapes = HashMap::new();
    let mut regions = Vec::new();

    for section in text.trim().split("\n\n") {
        let lines: Vec<&str> = section.trim().lines().collect();
        if lines.is_empty() {
            continue;
        }

        // Check if this is a shape definition (has ':' but not 'x')
        if lines[0].contains(':') && !lines[0].contains('x') {
            // Shape definition
            let idx = lines[0]
                .trim_end_matches(':')
                .parse::<usize>()
                .expect("Invalid shape index");

            let cell_count: usize = lines[1..]
                .iter()
                .flat_map(|line| line.chars())
                .filter(|&c| c == '#')
                .count();

            shapes.insert(idx, cell_count);
        } else {
            // Region definitions
            for line in lines {
                if let Some((dims, counts_str)) = line.split_once(':') {
                    if let Some((w_str, h_str)) = dims.trim().split_once('x') {
                        let width = w_str.parse::<usize>().expect("Invalid width");
                        let height = h_str.parse::<usize>().expect("Invalid height");

                        let counts: Vec<usize> = counts_str
                            .split_whitespace()
                            .map(|s| s.parse::<usize>().expect("Invalid count"))
                            .collect();

                        regions.push((width, height, counts));
                    }
                }
            }
        }
    }

    (shapes, regions)
}

/// Check if all presents can fit in the region
fn can_fit_region(width: usize, height: usize, counts: &[usize], shape_sizes: &ShapeSizes) -> bool {
    let total_cells_needed: usize = counts
        .iter()
        .enumerate()
        .map(|(i, &count)| count * shape_sizes.get(&i).copied().unwrap_or(0))
        .sum();

    total_cells_needed <= width * height
}

/// Part 1: Count regions that can fit all their presents
fn part1(shapes: &ShapeSizes, regions: &[Region]) -> usize {
    regions.iter()
        .filter(|(width, height, counts)| can_fit_region(*width, *height, counts, shapes))
        .count()
}

/// Part 2: No computation needed (just a button click)
fn part2() -> usize {
    0
}

fn main() {
    let text = fs::read_to_string("../input.txt").expect("Failed to read input file");
    let (shapes, regions) = parse_input(&text);

    println!("Part 1: {}", part1(&shapes, &regions));
    println!("Part 2: {}", part2());
}
