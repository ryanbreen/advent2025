#!/usr/bin/env ruby

require 'set'

def in_bounds?(r, c, rows, cols)
  r >= 0 && r < rows && c >= 0 && c < cols
end

def parse_input(filename)
  grid = File.readlines(filename, chomp: true)

  rows = grid.length
  cols = grid.empty? ? 0 : grid[0].length

  # Group antenna positions by frequency
  antennas = Hash.new { |h, k| h[k] = [] }
  grid.each_with_index do |row, r|
    row.chars.each_with_index do |ch, c|
      antennas[ch] << [r, c] unless ch == '.'
    end
  end

  [rows, cols, antennas]
end

def part1
  rows, cols, antennas = parse_input('../input.txt')

  antinodes = Set.new

  antennas.each do |freq, positions|
    # For each pair of antennas with same frequency
    positions.combination(2).each do |(r1, c1), (r2, c2)|
      # Calculate the two antinodes
      # Antinode beyond antenna 1 (away from antenna 2)
      ar1, ac1 = 2*r1 - r2, 2*c1 - c2
      # Antinode beyond antenna 2 (away from antenna 1)
      ar2, ac2 = 2*r2 - r1, 2*c2 - c1

      # Add if within bounds
      antinodes.add([ar1, ac1]) if in_bounds?(ar1, ac1, rows, cols)
      antinodes.add([ar2, ac2]) if in_bounds?(ar2, ac2, rows, cols)
    end
  end

  antinodes.size
end

def part2
  rows, cols, antennas = parse_input('../input.txt')

  antinodes = Set.new

  antennas.each do |freq, positions|
    # For each pair of antennas with same frequency
    positions.combination(2).each do |(r1, c1), (r2, c2)|
      dr, dc = r2 - r1, c2 - c1

      # Extend bidirectionally along the line defined by the two antennas
      # Direction 1: from antenna 1 towards and beyond antenna 2
      r, c = r1, c1
      while in_bounds?(r, c, rows, cols)
        antinodes.add([r, c])
        r += dr
        c += dc
      end

      # Direction 2: from antenna 1 away from antenna 2
      r, c = r1 - dr, c1 - dc
      while in_bounds?(r, c, rows, cols)
        antinodes.add([r, c])
        r -= dr
        c -= dc
      end
    end
  end

  antinodes.size
end

if __FILE__ == $PROGRAM_NAME
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end
