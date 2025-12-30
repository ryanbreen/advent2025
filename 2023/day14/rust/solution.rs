use std::collections::HashMap;
use std::fs;
use std::path::Path;

type Grid = Vec<Vec<char>>;

fn parse_input(text: &str) -> Grid {
    text.trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect()
}

fn tilt_north(grid: &mut Grid) {
    let rows = grid.len();
    let cols = grid[0].len();

    for col in 0..cols {
        let mut write_pos = 0;
        for row in 0..rows {
            match grid[row][col] {
                '#' => write_pos = row + 1,
                'O' => {
                    grid[row][col] = '.';
                    grid[write_pos][col] = 'O';
                    write_pos += 1;
                }
                _ => {}
            }
        }
    }
}

fn tilt_south(grid: &mut Grid) {
    let rows = grid.len();
    let cols = grid[0].len();

    for col in 0..cols {
        let mut write_pos = rows - 1;
        for row in (0..rows).rev() {
            match grid[row][col] {
                '#' => {
                    if row > 0 {
                        write_pos = row - 1;
                    }
                }
                'O' => {
                    grid[row][col] = '.';
                    grid[write_pos][col] = 'O';
                    if write_pos > 0 {
                        write_pos -= 1;
                    }
                }
                _ => {}
            }
        }
    }
}

fn tilt_west(grid: &mut Grid) {
    let rows = grid.len();
    let cols = grid[0].len();

    for row in 0..rows {
        let mut write_pos = 0;
        for col in 0..cols {
            match grid[row][col] {
                '#' => write_pos = col + 1,
                'O' => {
                    grid[row][col] = '.';
                    grid[row][write_pos] = 'O';
                    write_pos += 1;
                }
                _ => {}
            }
        }
    }
}

fn tilt_east(grid: &mut Grid) {
    let rows = grid.len();
    let cols = grid[0].len();

    for row in 0..rows {
        let mut write_pos = cols - 1;
        for col in (0..cols).rev() {
            match grid[row][col] {
                '#' => {
                    if col > 0 {
                        write_pos = col - 1;
                    }
                }
                'O' => {
                    grid[row][col] = '.';
                    grid[row][write_pos] = 'O';
                    if write_pos > 0 {
                        write_pos -= 1;
                    }
                }
                _ => {}
            }
        }
    }
}

fn spin_cycle(grid: &mut Grid) {
    tilt_north(grid);
    tilt_west(grid);
    tilt_south(grid);
    tilt_east(grid);
}

fn grid_to_key(grid: &Grid) -> Vec<Vec<char>> {
    grid.clone()
}

fn calculate_load(grid: &Grid) -> usize {
    let rows = grid.len();
    grid.iter()
        .enumerate()
        .map(|(row_idx, row)| {
            row.iter().filter(|&&c| c == 'O').count() * (rows - row_idx)
        })
        .sum()
}

fn part1(grid: &Grid) -> usize {
    let mut grid = grid.clone();
    tilt_north(&mut grid);
    calculate_load(&grid)
}

fn part2(grid: &Grid) -> usize {
    let mut grid = grid.clone();
    let target: usize = 1_000_000_000;

    let mut seen: HashMap<Vec<Vec<char>>, usize> = HashMap::new();
    let mut cycle_num = 0;

    while cycle_num < target {
        let state = grid_to_key(&grid);
        if let Some(&cycle_start) = seen.get(&state) {
            let cycle_length = cycle_num - cycle_start;
            let remaining = (target - cycle_num) % cycle_length;
            for _ in 0..remaining {
                spin_cycle(&mut grid);
            }
            return calculate_load(&grid);
        }

        seen.insert(state, cycle_num);
        spin_cycle(&mut grid);
        cycle_num += 1;
    }

    calculate_load(&grid)
}

fn main() {
    let input_path = Path::new(file!())
        .parent()
        .unwrap()
        .join("../input.txt");
    let text = fs::read_to_string(&input_path)
        .expect("Failed to read input file");

    let grid = parse_input(&text);

    println!("Part 1: {}", part1(&grid));
    println!("Part 2: {}", part2(&grid));
}
