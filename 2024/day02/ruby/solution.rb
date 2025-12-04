#!/usr/bin/env ruby

input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
lines = File.read(input_path).strip.split("\n")

# Parse reports
reports = lines.map { |line| line.split.map(&:to_i) }

def safe?(levels)
  return false if levels.length < 2

  diffs = (0...levels.length - 1).map { |i| levels[i + 1] - levels[i] }

  # All increasing or all decreasing
  all_increasing = diffs.all? { |d| d > 0 }
  all_decreasing = diffs.all? { |d| d < 0 }

  return false unless all_increasing || all_decreasing

  # All diffs must be 1-3 in absolute value
  diffs.all? { |d| d.abs >= 1 && d.abs <= 3 }
end

def safe_with_dampener?(levels)
  # Already safe without removing anything
  return true if safe?(levels)

  # Try removing each level one at a time
  (0...levels.length).each do |i|
    modified = levels[0...i] + levels[i + 1..-1]
    return true if safe?(modified)
  end

  false
end

def part1(reports)
  reports.count { |report| safe?(report) }
end

def part2(reports)
  reports.count { |report| safe_with_dampener?(report) }
end

puts "Part 1: #{part1(reports)}"
puts "Part 2: #{part2(reports)}"
