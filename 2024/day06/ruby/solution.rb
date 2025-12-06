#!/usr/bin/env ruby

require 'set'

def parse_grid(input)
  grid = input.split("\n").map { |line| line.chars }
  rows = grid.length
  cols = grid[0].length

  # Find the starting position and direction
  start_row, start_col = nil, nil
  start_dir = nil

  grid.each_with_index do |row, r|
    row.each_with_index do |cell, c|
      if ['^', 'v', '<', '>'].include?(cell)
        start_row, start_col = r, c
        start_dir = case cell
                    when '^' then 0  # up
                    when '>' then 1  # right
                    when 'v' then 2  # down
                    when '<' then 3  # left
                    end
        # Replace guard position with empty space for simulation
        grid[r][c] = '.'
        break
      end
    end
    break if start_row
  end

  [grid, rows, cols, start_row, start_col, start_dir]
end

def simulate_patrol(grid, rows, cols, start_row, start_col, start_dir, detect_loop: false)
  # Direction vectors: up, right, down, left
  dirs = [[-1, 0], [0, 1], [1, 0], [0, -1]]

  # Track visited positions
  visited_positions = Set.new
  visited_positions.add([start_row, start_col])

  # For loop detection, track (row, col, direction) states
  visited_states = Set.new if detect_loop
  visited_states.add([start_row, start_col, start_dir]) if detect_loop

  row, col, dir = start_row, start_col, start_dir

  loop do
    # Calculate next position
    dr, dc = dirs[dir]
    next_row = row + dr
    next_col = col + dc

    # Check if next position is out of bounds
    if next_row < 0 || next_row >= rows || next_col < 0 || next_col >= cols
      # Guard leaves the area - not a loop
      return detect_loop ? false : visited_positions.size
    end

    # Check if there's an obstacle ahead
    if grid[next_row][next_col] == '#'
      # Turn right 90 degrees
      dir = (dir + 1) % 4
    else
      # Move forward
      row, col = next_row, next_col
      visited_positions.add([row, col])

      if detect_loop
        state = [row, col, dir]
        if visited_states.include?(state)
          # We've been here before with the same direction - loop detected!
          return true
        end
        visited_states.add(state)
      end
    end
  end
end

def part1(input)
  grid, rows, cols, start_row, start_col, start_dir = parse_grid(input)
  simulate_patrol(grid, rows, cols, start_row, start_col, start_dir, detect_loop: false)
end

def part2(input)
  grid, rows, cols, start_row, start_col, start_dir = parse_grid(input)

  # First, get all positions the guard visits in the original path
  # We only need to try placing obstructions on positions the guard would visit
  original_path = Set.new
  dirs = [[-1, 0], [0, 1], [1, 0], [0, -1]]

  row, col, dir = start_row, start_col, start_dir
  original_path.add([row, col])

  loop do
    dr, dc = dirs[dir]
    next_row = row + dr
    next_col = col + dc

    break if next_row < 0 || next_row >= rows || next_col < 0 || next_col >= cols

    if grid[next_row][next_col] == '#'
      dir = (dir + 1) % 4
    else
      row, col = next_row, next_col
      original_path.add([row, col])
    end
  end

  # Try placing an obstruction at each position in the original path
  # (except the starting position)
  loop_count = 0

  original_path.each do |pos_r, pos_c|
    # Skip the starting position
    next if pos_r == start_row && pos_c == start_col

    # Skip positions that already have obstacles
    next if grid[pos_r][pos_c] == '#'

    # Try placing an obstruction at this position
    grid[pos_r][pos_c] = '#'

    # Simulate and check for loop
    if simulate_patrol(grid, rows, cols, start_row, start_col, start_dir, detect_loop: true)
      loop_count += 1
    end

    # Remove the obstruction
    grid[pos_r][pos_c] = '.'
  end

  loop_count
end

# Read input from file
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
input = File.read(input_path).strip

# Solve and output
puts "Part 1: #{part1(input)}"
puts "Part 2: #{part2(input)}"
