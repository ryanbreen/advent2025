#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

# Read and parse input
input_path = File.join(__dir__, '..', 'input.txt')
input_text = File.read(input_path).strip

# Parse grid
grid = input_text.split("\n").map(&:chars)
rows = grid.length
cols = grid[0].length

def find_regions(grid, rows, cols)
  """Find all connected regions in the grid using BFS."""
  visited = Set.new
  regions = []

  rows.times do |r|
    cols.times do |c|
      next if visited.include?([r, c])

      # BFS to find all cells in this region
      plant = grid[r][c]
      region = Set.new
      queue = [[r, c]]

      until queue.empty?
        cr, cc = queue.shift
        next if visited.include?([cr, cc])
        next if cr < 0 || cr >= rows || cc < 0 || cc >= cols
        next if grid[cr][cc] != plant

        visited.add([cr, cc])
        region.add([cr, cc])

        [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dr, dc|
          nr, nc = cr + dr, cc + dc
          queue.push([nr, nc]) unless visited.include?([nr, nc])
        end
      end

      regions.push(region)
    end
  end

  regions
end

def calculate_perimeter(region)
  """Calculate perimeter of a region (edges not touching same region)."""
  perimeter = 0
  region.each do |r, c|
    [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dr, dc|
      nr, nc = r + dr, c + dc
      perimeter += 1 unless region.include?([nr, nc])
    end
  end
  perimeter
end

def count_sides(region)
  """Count number of sides (corners) in a region."""
  corners = 0
  region.each do |r, c|
    # Check all 4 corners of this cell
    # Each corner is defined by checking two orthogonal neighbors and the diagonal
    # Convex: both orthogonal out
    # Concave: both orthogonal in, diagonal out

    up = region.include?([r - 1, c])
    down = region.include?([r + 1, c])
    left = region.include?([r, c - 1])
    right = region.include?([r, c + 1])
    up_left = region.include?([r - 1, c - 1])
    up_right = region.include?([r - 1, c + 1])
    down_left = region.include?([r + 1, c - 1])
    down_right = region.include?([r + 1, c + 1])

    # Top-left corner
    corners += 1 if !up && !left  # convex
    corners += 1 if up && left && !up_left  # concave

    # Top-right corner
    corners += 1 if !up && !right  # convex
    corners += 1 if up && right && !up_right  # concave

    # Bottom-left corner
    corners += 1 if !down && !left  # convex
    corners += 1 if down && left && !down_left  # concave

    # Bottom-right corner
    corners += 1 if !down && !right  # convex
    corners += 1 if down && right && !down_right  # concave
  end
  corners
end

def part1(grid, rows, cols)
  """Calculate total fencing cost: sum of area * perimeter for each region."""
  regions = find_regions(grid, rows, cols)
  total = 0
  regions.each do |region|
    area = region.size
    perimeter = calculate_perimeter(region)
    total += area * perimeter
  end
  total
end

def part2(grid, rows, cols)
  """Calculate total fencing cost using sides instead of perimeter."""
  regions = find_regions(grid, rows, cols)
  total = 0
  regions.each do |region|
    area = region.size
    sides = count_sides(region)
    total += area * sides
  end
  total
end

# Run solutions
puts "Part 1: #{part1(grid, rows, cols)}"
puts "Part 2: #{part2(grid, rows, cols)}"
