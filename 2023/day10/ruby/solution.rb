#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

# Day 10: Pipe Maze

# Direction constants
DIRECTIONS = [[-1, 0], [1, 0], [0, -1], [0, 1]].freeze
NORTH_PIPES = '|LJ'

# Define pipe connections: each pipe connects to certain directions
# Directions: N=[-1,0], S=[1,0], E=[0,1], W=[0,-1]
PIPE_CONNECTIONS = {
  '|' => [[-1, 0], [1, 0]],   # N, S
  '-' => [[0, -1], [0, 1]],   # W, E
  'L' => [[-1, 0], [0, 1]],   # N, E
  'J' => [[-1, 0], [0, -1]],  # N, W
  '7' => [[1, 0], [0, -1]],   # S, W
  'F' => [[1, 0], [0, 1]]     # S, E
}.freeze

def in_bounds?(r, c, rows, cols)
  r.between?(0, rows - 1) && c.between?(0, cols - 1)
end

def connects_back?(grid, nr, nc, r, c)
  adj_ch = grid[nr][nc]
  return false unless PIPE_CONNECTIONS.key?(adj_ch)

  PIPE_CONNECTIONS[adj_ch].any? { |adj_dr, adj_dc| nr + adj_dr == r && nc + adj_dc == c }
end

def find_start(grid)
  grid.each_with_index do |row, r|
    c = row.index('S')
    return [r, c] if c
  end
  nil
end

def get_neighbors(grid, pos)
  r, c = pos
  rows = grid.length
  cols = grid[0].length
  ch = grid[r][c]

  if ch == 'S'
    # S can connect to any adjacent pipe that connects back to it
    DIRECTIONS.map do |dr, dc|
      nr, nc = r + dr, c + dc
      [nr, nc] if in_bounds?(nr, nc, rows, cols) && connects_back?(grid, nr, nc, r, c)
    end.compact
  elsif PIPE_CONNECTIONS.key?(ch)
    PIPE_CONNECTIONS[ch].map do |dr, dc|
      nr, nc = r + dr, c + dc
      [nr, nc] if in_bounds?(nr, nc, rows, cols)
    end.compact
  else
    []
  end
end

def find_loop(grid, start)
  # BFS to find the main loop and distances from start
  distances = { start => 0 }
  queue = [start]

  until queue.empty?
    pos = queue.shift
    get_neighbors(grid, pos).each do |neighbor|
      unless distances.key?(neighbor)
        distances[neighbor] = distances[pos] + 1
        queue << neighbor
      end
    end
  end

  distances
end

def determine_start_pipe(grid, start, loop_positions)
  # Determine what pipe type S actually is based on its connections
  r, c = start

  connections = DIRECTIONS.map do |dr, dc|
    nr, nc = r + dr, c + dc
    [dr, dc] if loop_positions.include?([nr, nc]) && connects_back?(grid, nr, nc, r, c)
  end.compact

  connections_set = connections.to_set

  result = PIPE_CONNECTIONS.find { |_, dirs| dirs.to_set == connections_set }
  result ? result.first : 'S'
end

def part1(grid)
  start = find_start(grid)
  distances = find_loop(grid, start)
  distances.values.max
end

def part2(grid)
  # Count tiles enclosed by the loop using ray casting (crossing number)
  start = find_start(grid)
  distances = find_loop(grid, start)
  loop_positions = distances.keys.to_set

  # Replace S with its actual pipe type
  start_pipe = determine_start_pipe(grid, start, loop_positions)
  grid = grid.map(&:dup)
  grid[start[0]][start[1]] = start_pipe

  grid.each_with_index.sum do |row, r|
    inside = false
    row.each_char.with_index.sum do |ch, c|
      if loop_positions.include?([r, c])
        # Count vertical crossings (|, L, J go "north")
        # Using "north" rule: count pipes that have a north connection
        inside = !inside if NORTH_PIPES.include?(ch)
        0
      elsif inside
        1
      else
        0
      end
    end
  end
end

input_path = File.join(__dir__, '..', 'input.txt')
grid = File.read(input_path).strip.split("\n")

puts "Part 1: #{part1(grid)}"
puts "Part 2: #{part2(grid)}"
