#!/usr/bin/env ruby

def parse_input
  input_path = File.join(__dir__, '..', 'input.txt')
  File.readlines(input_path).map do |line|
    line = line.strip
    next if line.empty?

    parts = line.split(' -> ')
    x1, y1 = parts[0].split(',').map(&:to_i)
    x2, y2 = parts[1].split(',').map(&:to_i)
    [x1, y1, x2, y2]
  end.compact
end

def sign(x)
  x <=> 0
end

def count_overlaps(lines, include_diagonals: false)
  grid = Hash.new(0)

  lines.each do |x1, y1, x2, y2|
    dx = sign(x2 - x1)
    dy = sign(y2 - y1)

    # Skip diagonals in part 1
    next if !include_diagonals && dx != 0 && dy != 0

    x, y = x1, y1
    loop do
      grid[[x, y]] += 1
      break if x == x2 && y == y2
      x += dx
      y += dy
    end
  end

  grid.values.count { |v| v >= 2 }
end

def part1(lines)
  count_overlaps(lines, include_diagonals: false)
end

def part2(lines)
  count_overlaps(lines, include_diagonals: true)
end

if __FILE__ == $PROGRAM_NAME
  lines = parse_input
  puts "Part 1: #{part1(lines)}"
  puts "Part 2: #{part2(lines)}"
end
