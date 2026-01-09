#!/usr/bin/env ruby

def parse_input(filename)
  pairs = []
  File.readlines(filename).each do |line|
    line = line.strip
    next if line.empty?

    left, right = line.split(',')
    a1, b1 = left.split('-').map(&:to_i)
    a2, b2 = right.split('-').map(&:to_i)
    pairs << [a1, b1, a2, b2]
  end
  pairs
end

def fully_contains?(a1, b1, a2, b2)
  # Check if one range fully contains the other
  (a1 <= a2 && b1 >= b2) || (a2 <= a1 && b2 >= b1)
end

def overlaps?(a1, b1, a2, b2)
  # Check if ranges overlap at all
  a1 <= b2 && a2 <= b1
end

def part1(pairs)
  pairs.count { |a1, b1, a2, b2| fully_contains?(a1, b1, a2, b2) }
end

def part2(pairs)
  pairs.count { |a1, b1, a2, b2| overlaps?(a1, b1, a2, b2) }
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  pairs = parse_input(input_file)

  puts "Part 1: #{part1(pairs)}"
  puts "Part 2: #{part2(pairs)}"
end

main
