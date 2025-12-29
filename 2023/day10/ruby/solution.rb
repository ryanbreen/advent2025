#!/usr/bin/env ruby

require 'set'

# Day 10: Pipe Maze

input_path = File.join(__dir__, '..', 'input.txt')
lines = File.read(input_path).strip.split("\n")

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

def find_start(grid)
  grid.each_with_index do |row, r|
    row.chars.each_with_index do |ch, c|
      return [r, c] if ch == 'S'
    end
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
    neighbors = []
    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
      nr, nc = r + dr, c + dc
      next unless nr >= 0 && nr < rows && nc >= 0 && nc < cols

      adj_ch = grid[nr][nc]
      if PIPE_CONNECTIONS.key?(adj_ch)
        PIPE_CONNECTIONS[adj_ch].each do |adj_dr, adj_dc|
          if nr + adj_dr == r && nc + adj_dc == c
            neighbors << [nr, nc]
            break
          end
        end
      end
    end
    neighbors
  elsif PIPE_CONNECTIONS.key?(ch)
    neighbors = []
    PIPE_CONNECTIONS[ch].each do |dr, dc|
      nr, nc = r + dr, c + dc
      neighbors << [nr, nc] if nr >= 0 && nr < rows && nc >= 0 && nc < cols
    end
    neighbors
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
  rows = grid.length
  cols = grid[0].length

  connections = []
  [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
    nr, nc = r + dr, c + dc
    next unless loop_positions.include?([nr, nc])

    adj_ch = grid[nr][nc]
    if PIPE_CONNECTIONS.key?(adj_ch)
      PIPE_CONNECTIONS[adj_ch].each do |adj_dr, adj_dc|
        if nr + adj_dr == r && nc + adj_dc == c
          connections << [dr, dc]
          break
        end
      end
    end
  end

  connections_set = connections.to_set

  PIPE_CONNECTIONS.each do |pipe, dirs|
    return pipe if dirs.to_set == connections_set
  end

  'S'
end

def part1(lines)
  start = find_start(lines)
  distances = find_loop(lines, start)
  distances.values.max
end

def part2(lines)
  # Count tiles enclosed by the loop using ray casting (crossing number)
  start = find_start(lines)
  distances = find_loop(lines, start)
  loop_positions = distances.keys.to_set

  # Replace S with its actual pipe type
  start_pipe = determine_start_pipe(lines, start, loop_positions)
  grid = lines.map(&:chars)
  grid[start[0]][start[1]] = start_pipe

  rows = grid.length
  cols = grid[0].length
  enclosed = 0

  (0...rows).each do |r|
    inside = false
    (0...cols).each do |c|
      if loop_positions.include?([r, c])
        ch = grid[r][c]
        # Count vertical crossings (|, L, J go "north")
        # Using "north" rule: count pipes that have a north connection
        inside = !inside if '|LJ'.include?(ch)
      elsif inside
        enclosed += 1
      end
    end
  end

  enclosed
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
