#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_grid(lines)
  galaxies = []
  lines.each_with_index do |line, r|
    line.each_char.with_index do |ch, c|
      galaxies << [r, c] if ch == '#'
    end
  end
  galaxies
end

def find_empty_rows_and_cols(lines)
  cols = lines.empty? ? 0 : lines[0].length
  rows = lines.length

  empty_rows = lines.each_index.reject { |r| lines[r].include?('#') }
  empty_cols = (0...cols).select { |c| lines.none? { |line| line[c] == '#' } }

  # Build cumulative count arrays for O(1) range lookups
  empty_row_counts = Array.new(rows + 1, 0)
  empty_col_counts = Array.new(cols + 1, 0)

  empty_rows.each { |r| empty_row_counts[r + 1] = 1 }
  empty_cols.each { |c| empty_col_counts[c + 1] = 1 }

  # Convert to cumulative sums
  (1..rows).each { |i| empty_row_counts[i] += empty_row_counts[i - 1] }
  (1..cols).each { |i| empty_col_counts[i] += empty_col_counts[i - 1] }

  [empty_row_counts, empty_col_counts]
end

def calculate_distances(galaxies, empty_row_counts, empty_col_counts, expansion_factor: 2)
  expansion_bonus = expansion_factor - 1
  total = 0

  galaxies.combination(2) do |(r1, c1), (r2, c2)|
    min_r, max_r = r1 < r2 ? [r1, r2] : [r2, r1]
    min_c, max_c = c1 < c2 ? [c1, c2] : [c2, c1]

    # O(1) lookup using cumulative counts
    empty_rows_between = empty_row_counts[max_r] - empty_row_counts[min_r]
    empty_cols_between = empty_col_counts[max_c] - empty_col_counts[min_c]

    row_dist = max_r - min_r + expansion_bonus * empty_rows_between
    col_dist = max_c - min_c + expansion_bonus * empty_cols_between

    total += row_dist + col_dist
  end

  total
end

def solve(lines)
  galaxies = parse_grid(lines)
  empty_rows, empty_cols = find_empty_rows_and_cols(lines)

  part1 = calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor: 2)
  part2 = calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor: 1_000_000)

  [part1, part2]
end

def main
  input_file = ARGV[0] || '../input.txt'
  lines = File.readlines(input_file, chomp: true)

  # Remove any trailing empty lines
  lines.pop while lines.any? && lines.last.empty?

  part1, part2 = solve(lines)
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end

main if __FILE__ == $PROGRAM_NAME
