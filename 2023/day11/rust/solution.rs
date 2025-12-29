use std::collections::HashSet;
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

fn find_empty_rows_and_cols(lines: &[&str]) -> (HashSet<usize>, HashSet<usize>) {
    let rows = lines.len();
    let cols = if rows > 0 { lines[0].len() } else { 0 };

    let empty_rows: HashSet<usize> = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| !line.contains('#'))
        .map(|(r, _)| r)
        .collect();

    let empty_cols: HashSet<usize> = (0..cols)
        .filter(|&c| {
            (0..rows).all(|r| {
                lines[r].as_bytes().get(c).map_or(true, |&b| b != b'#')
            })
        })
        .collect();

    (empty_rows, empty_cols)
}

fn calculate_distances(
    galaxies: &[Galaxy],
    empty_rows: &HashSet<usize>,
    empty_cols: &HashSet<usize>,
    expansion_factor: u64,
) -> u64 {
    let mut total: u64 = 0;

    for i in 0..galaxies.len() {
        for j in (i + 1)..galaxies.len() {
            let (r1, c1) = galaxies[i];
            let (r2, c2) = galaxies[j];

            let (min_r, max_r) = (r1.min(r2), r1.max(r2));
            let row_crossings = (min_r..max_r).filter(|r| empty_rows.contains(r)).count() as u64;
            let row_dist = (max_r - min_r) as u64 + row_crossings * (expansion_factor - 1);

            let (min_c, max_c) = (c1.min(c2), c1.max(c2));
            let col_crossings = (min_c..max_c).filter(|c| empty_cols.contains(c)).count() as u64;
            let col_dist = (max_c - min_c) as u64 + col_crossings * (expansion_factor - 1);

            total += row_dist + col_dist;
        }
    }

    total
}

fn solve(lines: &[&str]) -> (u64, u64) {
    let galaxies = parse_grid(lines);
    let (empty_rows, empty_cols) = find_empty_rows_and_cols(lines);

    let part1 = calculate_distances(&galaxies, &empty_rows, &empty_cols, 2);
    let part2 = calculate_distances(&galaxies, &empty_rows, &empty_cols, 1_000_000);

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
