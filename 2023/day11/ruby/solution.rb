#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

def parse_grid(lines)
  lines.each_with_index.flat_map do |line, r|
    line.chars.each_with_index.select { |ch, _| ch == '#' }.map { |_, c| [r, c] }
  end
end

def find_empty_rows_and_cols(lines)
  cols = lines.empty? ? 0 : lines[0].length

  empty_rows = lines.each_index.reject { |r| lines[r].include?('#') }.to_set
  empty_cols = (0...cols).select { |c| lines.none? { |line| line[c] == '#' } }.to_set

  [empty_rows, empty_cols]
end

def calculate_distances(galaxies, empty_rows, empty_cols, expansion_factor: 2)
  expansion_bonus = expansion_factor - 1

  galaxies.combination(2).sum do |(r1, c1), (r2, c2)|
    min_r, max_r = [r1, r2].minmax
    min_c, max_c = [c1, c2].minmax

    row_dist = max_r - min_r + expansion_bonus * (min_r...max_r).count { |r| empty_rows.include?(r) }
    col_dist = max_c - min_c + expansion_bonus * (min_c...max_c).count { |c| empty_cols.include?(c) }

    row_dist + col_dist
  end
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
