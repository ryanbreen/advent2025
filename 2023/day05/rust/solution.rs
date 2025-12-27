use std::fs;
use std::ops::Range;

/// Represents a mapping rule that transforms source values to destination values.
///
/// The mapping applies to values in the range `source_start..(source_start + length)`,
/// shifting them by `(destination_start - source_start)`.
#[derive(Debug, Clone)]
struct MappingRule {
    destination_start: u64,
    source_range: Range<u64>,
}

impl MappingRule {
    /// Creates a new mapping rule from destination start, source start, and length.
    fn new(destination_start: u64, source_start: u64, length: u64) -> Self {
        Self {
            destination_start,
            source_range: source_start..(source_start + length),
        }
    }

    /// Returns the offset to apply when mapping values within this rule's range.
    fn offset(&self) -> i64 {
        self.destination_start as i64 - self.source_range.start as i64
    }

    /// Checks if a value falls within this mapping rule's source range.
    fn contains(&self, value: u64) -> bool {
        self.source_range.contains(&value)
    }

    /// Applies this mapping rule to a value, returning the mapped value.
    fn apply(&self, value: u64) -> u64 {
        (value as i64 + self.offset()) as u64
    }
}

/// A category map containing multiple mapping rules (e.g., seed-to-soil).
type CategoryMap = Vec<MappingRule>;

/// An interval representing a range of values [start, end).
#[derive(Debug, Clone, Copy)]
struct Interval {
    start: u64,
    end: u64,
}

impl Interval {
    /// Creates a new interval from start (inclusive) to end (exclusive).
    fn new(start: u64, end: u64) -> Self {
        Self { start, end }
    }

    /// Checks if the interval is non-empty.
    fn is_valid(&self) -> bool {
        self.start < self.end
    }

    /// Returns the intersection of this interval with another range.
    fn intersect(&self, range: &Range<u64>) -> Option<Self> {
        let overlap_start = self.start.max(range.start);
        let overlap_end = self.end.min(range.end);
        if overlap_start < overlap_end {
            Some(Self::new(overlap_start, overlap_end))
        } else {
            None
        }
    }
}

/// Parses the puzzle input into seeds and a list of category maps.
///
/// # Returns
/// A tuple containing:
/// - A vector of seed numbers
/// - A vector of category maps, each containing mapping rules
fn parse_input(input_text: &str) -> (Vec<u64>, Vec<CategoryMap>) {
    let sections: Vec<&str> = input_text.trim().split("\n\n").collect();

    let seeds = sections[0]
        .split(": ")
        .nth(1)
        .expect("Input should contain seeds after 'seeds: '")
        .split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect();

    let category_maps = sections[1..]
        .iter()
        .map(|section| {
            section
                .lines()
                .skip(1) // Skip the header line (e.g., "seed-to-soil map:")
                .filter_map(|line| {
                    let numbers: Vec<u64> = line
                        .split_whitespace()
                        .filter_map(|s| s.parse().ok())
                        .collect();
                    if numbers.len() == 3 {
                        Some(MappingRule::new(numbers[0], numbers[1], numbers[2]))
                    } else {
                        None
                    }
                })
                .collect()
        })
        .collect();

    (seeds, category_maps)
}

/// Applies a single category map to transform a value.
///
/// If the value falls within any mapping rule's source range, it is transformed.
/// Otherwise, the value passes through unchanged.
fn apply_category_map(value: u64, category_map: &CategoryMap) -> u64 {
    category_map
        .iter()
        .find(|rule| rule.contains(value))
        .map(|rule| rule.apply(value))
        .unwrap_or(value)
}

/// Converts a seed number to a location number by applying all category maps.
fn seed_to_location(seed: u64, category_maps: &[CategoryMap]) -> u64 {
    category_maps
        .iter()
        .fold(seed, |current_value, category_map| {
            apply_category_map(current_value, category_map)
        })
}

/// Finds the lowest location number for any initial seed (Part 1).
///
/// Each seed number is transformed through all category maps, and the
/// minimum resulting location is returned.
fn solve_part1(seeds: &[u64], category_maps: &[CategoryMap]) -> u64 {
    seeds
        .iter()
        .map(|&seed| seed_to_location(seed, category_maps))
        .min()
        .expect("At least one seed should be present")
}

/// Applies a category map to a list of intervals, returning transformed intervals.
///
/// Each input interval may be split into multiple output intervals based on
/// how it overlaps with the mapping rules.
fn apply_category_map_to_intervals(
    input_intervals: &[Interval],
    category_map: &CategoryMap,
) -> Vec<Interval> {
    input_intervals
        .iter()
        .flat_map(|&interval| {
            let mut mapped_intervals = Vec::new();
            let mut unmapped_intervals = vec![interval];

            for rule in category_map {
                let mut still_unmapped = Vec::new();

                for current in unmapped_intervals {
                    // Part before the mapping rule's range (stays unmapped)
                    let before = Interval::new(current.start, current.end.min(rule.source_range.start));
                    if before.is_valid() {
                        still_unmapped.push(before);
                    }

                    // Part overlapping with the mapping rule's range (gets mapped)
                    if let Some(overlap) = current.intersect(&rule.source_range) {
                        mapped_intervals.push(Interval::new(
                            rule.apply(overlap.start),
                            rule.apply(overlap.end),
                        ));
                    }

                    // Part after the mapping rule's range (stays unmapped)
                    let after = Interval::new(current.start.max(rule.source_range.end), current.end);
                    if after.is_valid() {
                        still_unmapped.push(after);
                    }
                }

                unmapped_intervals = still_unmapped;
            }

            // Remaining unmapped intervals pass through unchanged
            mapped_intervals.extend(unmapped_intervals);
            mapped_intervals
        })
        .collect()
}

/// Finds the lowest location for seed ranges (Part 2).
///
/// Seeds are interpreted as pairs: (start, length), defining ranges of seeds.
/// The solution processes these ranges efficiently without iterating over
/// individual seed values.
fn solve_part2(seeds: &[u64], category_maps: &[CategoryMap]) -> u64 {
    let initial_intervals: Vec<Interval> = seeds
        .chunks_exact(2)
        .map(|chunk| Interval::new(chunk[0], chunk[0] + chunk[1]))
        .collect();

    let final_intervals = category_maps
        .iter()
        .fold(initial_intervals, |intervals, category_map| {
            apply_category_map_to_intervals(&intervals, category_map)
        });

    final_intervals
        .iter()
        .map(|interval| interval.start)
        .min()
        .expect("At least one interval should be present")
}

fn main() {
    let input_path = std::path::Path::new(file!())
        .parent()
        .unwrap()
        .join("../input.txt");
    let input_text = fs::read_to_string(&input_path)
        .unwrap_or_else(|_| fs::read_to_string("../input.txt").unwrap());

    let (seeds, category_maps) = parse_input(&input_text);

    println!("Part 1: {}", solve_part1(&seeds, &category_maps));
    println!("Part 2: {}", solve_part2(&seeds, &category_maps));
}
