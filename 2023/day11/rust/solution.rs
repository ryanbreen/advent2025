use std::fs;

type Galaxy = (usize, usize);

fn parse_grid(lines: &[&str]) -> Vec<Galaxy> {
    lines
        .iter()
        .enumerate()
        .flat_map(|(r, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, ch)| *ch == '#')
                .map(move |(c, _)| (r, c))
        })
        .collect()
}

fn build_prefix_sums(lines: &[&str], galaxies: &[Galaxy]) -> (Vec<i32>, Vec<i32>) {
    let rows = lines.len();
    let cols = if rows > 0 { lines[0].len() } else { 0 };

    // Track which rows and columns contain galaxies
    let mut empty_rows = vec![true; rows];
    let mut empty_cols = vec![true; cols];

    for &(r, c) in galaxies {
        empty_rows[r] = false;
        empty_cols[c] = false;
    }

    // Build prefix sums for O(1) range queries
    let mut prefix_rows = vec![0i32; rows + 1];
    for r in 0..rows {
        prefix_rows[r + 1] = prefix_rows[r] + if empty_rows[r] { 1 } else { 0 };
    }

    let mut prefix_cols = vec![0i32; cols + 1];
    for c in 0..cols {
        prefix_cols[c + 1] = prefix_cols[c] + if empty_cols[c] { 1 } else { 0 };
    }

    (prefix_rows, prefix_cols)
}

fn calculate_distances(
    galaxies: &[Galaxy],
    prefix_rows: &[i32],
    prefix_cols: &[i32],
    expansion_factor: u64,
) -> u64 {
    let mut total: u64 = 0;

    for i in 0..galaxies.len() {
        for j in (i + 1)..galaxies.len() {
            let (r1, c1) = galaxies[i];
            let (r2, c2) = galaxies[j];

            // Calculate row distance with expansion using prefix sums
            let (min_r, max_r) = (r1.min(r2), r1.max(r2));
            let empty_row_count = (prefix_rows[max_r] - prefix_rows[min_r]) as u64;
            let row_dist = (max_r - min_r) as u64 + empty_row_count * (expansion_factor - 1);

            // Calculate column distance with expansion using prefix sums
            let (min_c, max_c) = (c1.min(c2), c1.max(c2));
            let empty_col_count = (prefix_cols[max_c] - prefix_cols[min_c]) as u64;
            let col_dist = (max_c - min_c) as u64 + empty_col_count * (expansion_factor - 1);

            total += row_dist + col_dist;
        }
    }

    total
}

fn solve(lines: &[&str]) -> (u64, u64) {
    let galaxies = parse_grid(lines);
    let (prefix_rows, prefix_cols) = build_prefix_sums(lines, &galaxies);

    let part1 = calculate_distances(&galaxies, &prefix_rows, &prefix_cols, 2);
    let part2 = calculate_distances(&galaxies, &prefix_rows, &prefix_cols, 1_000_000);

    (part1, part2)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let input_file = if args.len() > 1 {
        &args[1]
    } else {
        "../input.txt"
    };

    let content = fs::read_to_string(input_file).expect("Failed to read input file");
    let lines: Vec<&str> = content.lines().filter(|line| !line.is_empty()).collect();

    let (part1, part2) = solve(&lines);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
