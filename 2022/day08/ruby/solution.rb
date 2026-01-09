#!/usr/bin/env ruby

def parse_grid(lines)
  lines.map { |line| line.chars.map(&:to_i) }
end

def visible?(grid, row, col)
  rows = grid.length
  cols = grid[0].length
  height = grid[row][col]

  # Check from left
  visible_left = (0...col).all? { |c| grid[row][c] < height }
  # Check from right
  visible_right = ((col + 1)...cols).all? { |c| grid[row][c] < height }
  # Check from top
  visible_top = (0...row).all? { |r| grid[r][col] < height }
  # Check from bottom
  visible_bottom = ((row + 1)...rows).all? { |r| grid[r][col] < height }

  visible_left || visible_right || visible_top || visible_bottom
end

def scenic_score(grid, row, col)
  rows = grid.length
  cols = grid[0].length
  height = grid[row][col]

  # Count trees visible in each direction
  # Left
  left = 0
  (col - 1).downto(0) do |c|
    left += 1
    break if grid[row][c] >= height
  end

  # Right
  right = 0
  ((col + 1)...cols).each do |c|
    right += 1
    break if grid[row][c] >= height
  end

  # Up
  up = 0
  (row - 1).downto(0) do |r|
    up += 1
    break if grid[r][col] >= height
  end

  # Down
  down = 0
  ((row + 1)...rows).each do |r|
    down += 1
    break if grid[r][col] >= height
  end

  left * right * up * down
end

def part1(grid)
  rows = grid.length
  cols = grid[0].length
  count = 0
  (0...rows).each do |r|
    (0...cols).each do |c|
      count += 1 if visible?(grid, r, c)
    end
  end
  count
end

def part2(grid)
  rows = grid.length
  cols = grid[0].length
  max_score = 0
  (0...rows).each do |r|
    (0...cols).each do |c|
      score = scenic_score(grid, r, c)
      max_score = [max_score, score].max
    end
  end
  max_score
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  lines = File.read(input_file).strip.split("\n")
  grid = parse_grid(lines)

  puts "Part 1: #{part1(grid)}"
  puts "Part 2: #{part2(grid)}"
end

main
