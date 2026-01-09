#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_grid(text)
  grid = text.strip.split("\n").map(&:chars)
  start_pos = nil
  end_pos = nil

  grid.each_with_index do |row, r|
    row.each_with_index do |ch, c|
      if ch == 'S'
        start_pos = [r, c]
        grid[r][c] = 'a'
      elsif ch == 'E'
        end_pos = [r, c]
        grid[r][c] = 'z'
      end
    end
  end

  [grid, start_pos, end_pos]
end

def bfs(grid, starts, end_pos)
  rows = grid.length
  cols = grid[0].length
  visited = {}
  queue = []

  starts.each do |start|
    queue.push([start[0], start[1], 0])
    visited[start] = true
  end

  directions = [[-1, 0], [1, 0], [0, -1], [0, 1]]

  until queue.empty?
    r, c, dist = queue.shift

    return dist if [r, c] == end_pos

    current_height = grid[r][c].ord

    directions.each do |dr, dc|
      nr = r + dr
      nc = c + dc

      next unless nr >= 0 && nr < rows && nc >= 0 && nc < cols
      next if visited[[nr, nc]]

      next_height = grid[nr][nc].ord
      # Can move if destination is at most 1 higher
      if next_height <= current_height + 1
        visited[[nr, nc]] = true
        queue.push([nr, nc, dist + 1])
      end
    end
  end

  -1 # No path found
end

def part1(text)
  grid, start_pos, end_pos = parse_grid(text)
  bfs(grid, [start_pos], end_pos)
end

def part2(text)
  grid, _, end_pos = parse_grid(text)

  # Find all cells with elevation 'a'
  starts = []
  grid.each_with_index do |row, r|
    row.each_with_index do |ch, c|
      starts.push([r, c]) if ch == 'a'
    end
  end

  bfs(grid, starts, end_pos)
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
