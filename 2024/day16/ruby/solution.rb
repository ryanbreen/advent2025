#!/usr/bin/env ruby
# Day 16: Reindeer Maze - Weighted shortest path with turn costs

require 'set'

def parse_input(text)
  grid = text.strip.split("\n").map(&:chars)
  start_pos = nil
  end_pos = nil

  grid.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      start_pos = [x, y] if cell == 'S'
      end_pos = [x, y] if cell == 'E'
    end
  end

  [grid, start_pos, end_pos]
end

# Directions: 0=East, 1=South, 2=West, 3=North
DX = [1, 0, -1, 0]
DY = [0, 1, 0, -1]

def dijkstra_forward(grid, start)
  # Priority queue: [cost, x, y, direction]
  # Ruby doesn't have built-in heap, so we'll use a sorted array
  pq = [[0, start[0], start[1], 0]] # Start facing East
  dist = {}

  until pq.empty?
    cost, x, y, d = pq.shift

    state = [x, y, d]
    next if dist.key?(state)
    dist[state] = cost

    # Move forward
    nx = x + DX[d]
    ny = y + DY[d]
    if ny >= 0 && ny < grid.length && nx >= 0 && nx < grid[0].length && grid[ny][nx] != '#'
      insert_sorted(pq, [cost + 1, nx, ny, d])
    end

    # Turn left and right
    insert_sorted(pq, [cost + 1000, x, y, (d - 1) % 4])
    insert_sorted(pq, [cost + 1000, x, y, (d + 1) % 4])
  end

  dist
end

def dijkstra_backward(grid, end_pos)
  # Run Dijkstra backward from end (all directions at end have cost 0)
  pq = (0..3).map { |d| [0, end_pos[0], end_pos[1], d] }
  dist = {}

  until pq.empty?
    cost, x, y, d = pq.shift

    state = [x, y, d]
    next if dist.key?(state)
    dist[state] = cost

    # Reverse of "move forward": come from behind
    px = x - DX[d]
    py = y - DY[d]
    if py >= 0 && py < grid.length && px >= 0 && px < grid[0].length && grid[py][px] != '#'
      insert_sorted(pq, [cost + 1, px, py, d])
    end

    # Reverse of turn: came from same position with different direction
    insert_sorted(pq, [cost + 1000, x, y, (d - 1) % 4])
    insert_sorted(pq, [cost + 1000, x, y, (d + 1) % 4])
  end

  dist
end

def insert_sorted(arr, item)
  # Binary search insertion to maintain sorted order by cost
  left = 0
  right = arr.length

  while left < right
    mid = (left + right) / 2
    if arr[mid][0] < item[0]
      left = mid + 1
    else
      right = mid
    end
  end

  arr.insert(left, item)
end

def part1(grid, start, end_pos)
  dist = dijkstra_forward(grid, start)
  (0..3).map { |d| dist[[end_pos[0], end_pos[1], d]] || Float::INFINITY }.min
end

def part2(grid, start, end_pos, best_score)
  dist_from_start = dijkstra_forward(grid, start)
  dist_to_end = dijkstra_backward(grid, end_pos)

  tiles_on_best_path = Set.new

  grid.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      next if cell == '#'

      # Check if this tile is on any optimal path
      (0..3).each do |d|
        state = [x, y, d]
        from_start = dist_from_start[state] || Float::INFINITY
        to_end = dist_to_end[state] || Float::INFINITY

        if from_start + to_end == best_score
          tiles_on_best_path.add([x, y])
          break
        end
      end
    end
  end

  tiles_on_best_path.size
end

def main
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  text = File.read(input_path)
  grid, start, end_pos = parse_input(text)

  answer1 = part1(grid, start, end_pos)
  puts "Part 1: #{answer1}"

  answer2 = part2(grid, start, end_pos, answer1)
  puts "Part 2: #{answer2}"
end

main if __FILE__ == $PROGRAM_NAME
