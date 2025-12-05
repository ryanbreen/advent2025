#!/usr/bin/env ruby

# Read input
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
input_text = File.read(input_path).strip

# Parse input
grid = input_text.split("\n")
rows = grid.length
cols = grid[0].length

# 8 directions: right, left, down, up, and 4 diagonals
DIRECTIONS = [
  [0, 1],    # right
  [0, -1],   # left
  [1, 0],    # down
  [-1, 0],   # up
  [1, 1],    # down-right
  [1, -1],   # down-left
  [-1, 1],   # up-right
  [-1, -1],  # up-left
]

def part1(grid, rows, cols)
  target = "XMAS"
  count = 0

  (0...rows).each do |r|
    (0...cols).each do |c|
      # Try each direction from this position
      DIRECTIONS.each do |dr, dc|
        # Check if XMAS fits in this direction
        found = true
        target.chars.each_with_index do |ch, i|
          nr = r + dr * i
          nc = c + dc * i
          if nr < 0 || nr >= rows || nc < 0 || nc >= cols
            found = false
            break
          end
          if grid[nr][nc] != ch
            found = false
            break
          end
        end
        count += 1 if found
      end
    end
  end

  count
end

def part2(grid, rows, cols)
  # Find X-MAS patterns: two MAS strings forming an X with A in the center
  # Each diagonal can be MAS or SAM
  count = 0

  # Check each possible center point (A must be in the middle)
  (1...rows - 1).each do |r|
    (1...cols - 1).each do |c|
      next if grid[r][c] != 'A'

      # Get the four corners
      top_left = grid[r - 1][c - 1]
      top_right = grid[r - 1][c + 1]
      bottom_left = grid[r + 1][c - 1]
      bottom_right = grid[r + 1][c + 1]

      # Check diagonal 1 (top-left to bottom-right): MAS or SAM
      diag1_ok = (top_left == 'M' && bottom_right == 'S') || (top_left == 'S' && bottom_right == 'M')

      # Check diagonal 2 (top-right to bottom-left): MAS or SAM
      diag2_ok = (top_right == 'M' && bottom_left == 'S') || (top_right == 'S' && bottom_left == 'M')

      count += 1 if diag1_ok && diag2_ok
    end
  end

  count
end

# Run both parts
puts "Part 1: #{part1(grid, rows, cols)}"
puts "Part 2: #{part2(grid, rows, cols)}"
