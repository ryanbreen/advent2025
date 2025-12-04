#!/usr/bin/env ruby

# Read input file
input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip
lines = input_text.split("\n")

# Define the 8 directions (N, NE, E, SE, S, SW, W, NW)
DIRECTIONS = [
  [-1, 0],  # N
  [-1, 1],  # NE
  [0, 1],   # E
  [1, 1],   # SE
  [1, 0],   # S
  [1, -1],  # SW
  [0, -1],  # W
  [-1, -1]  # NW
].freeze

# ============== PRECOMPUTE ROLL POSITIONS AND NEIGHBORS ==============
grid = lines.map { |line| line.chars }
rows = grid.length
cols = grid[0].length

roll_positions = []   # Array of [r, c] for each roll
pos_to_index = {}     # Hash: [r, c] => index in roll_positions
roll_neighbors = []   # Array of arrays: neighbors for each roll

(0...rows).each do |r|
  (0...cols).each do |c|
    if grid[r][c] == '@'
      idx = roll_positions.length
      roll_positions << [r, c]
      pos_to_index[[r, c]] = idx

      # Precompute neighbors for this roll
      neighbors = []
      DIRECTIONS.each do |dr, dc|
        nr = r + dr
        nc = c + dc
        if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] == '@'
          neighbors << [nr, nc]
        end
      end
      roll_neighbors << neighbors
    end
  end
end

num_rolls = roll_positions.length

def part1(roll_neighbors, num_rolls)
  accessible_count = 0

  (0...num_rolls).each do |i|
    neighbor_count = roll_neighbors[i].length
    accessible_count += 1 if neighbor_count < 4
  end

  accessible_count
end

def part2(roll_positions, roll_neighbors, pos_to_index, num_rolls)
  # Track which rolls are still active
  active = {}
  (0...num_rolls).each do |i|
    active[roll_positions[i]] = true
  end

  # Compute initial neighbor counts
  neighbor_count = Array.new(num_rolls)
  (0...num_rolls).each do |i|
    count = 0
    roll_neighbors[i].each do |neighbor_pos|
      count += 1 if active[neighbor_pos]
    end
    neighbor_count[i] = count
  end

  # Initialize queue with accessible rolls (neighbor count < 4)
  queue = []
  in_queue = {}
  (0...num_rolls).each do |i|
    if neighbor_count[i] < 4
      queue << i
      in_queue[i] = true
    end
  end

  # Process queue
  total_removed = 0

  until queue.empty?
    next_queue = []

    queue.each do |idx|
      pos = roll_positions[idx]

      # Skip if already removed
      next unless active[pos]

      # Remove this roll
      active.delete(pos)
      total_removed += 1

      # Update neighbors' counts
      roll_neighbors[idx].each do |neighbor_pos|
        if active[neighbor_pos]
          neighbor_idx = pos_to_index[neighbor_pos]
          neighbor_count[neighbor_idx] -= 1

          # Add to queue if now accessible and not already queued
          if neighbor_count[neighbor_idx] < 4 && !in_queue[neighbor_idx]
            next_queue << neighbor_idx
            in_queue[neighbor_idx] = true
          end
        end
      end
    end

    queue = next_queue
  end

  total_removed
end

puts "Part 1: #{part1(roll_neighbors, num_rolls)}"
puts "Part 2: #{part2(roll_positions, roll_neighbors, pos_to_index, num_rolls)}"
