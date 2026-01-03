#!/usr/bin/env ruby
# Day 21: Step Counter - Garden plot reachability

def parse_input(filename)
  grid = File.read(filename).strip.split("\n")
  start = nil

  grid.each_with_index do |row, r|
    row.chars.each_with_index do |ch, c|
      if ch == 'S'
        start = [r, c]
        break
      end
    end
    break if start
  end

  [grid, start]
end

def count_reachable(grid, start, steps)
  rows = grid.length
  cols = grid[0].length

  # BFS to find minimum steps to each cell
  visited = { start => 0 }
  queue = [[start[0], start[1], 0]]

  directions = [[-1, 0], [1, 0], [0, -1], [0, 1]]

  while !queue.empty?
    r, c, dist = queue.shift

    next if dist >= steps

    directions.each do |dr, dc|
      nr, nc = r + dr, c + dc

      if nr >= 0 && nr < rows && nc >= 0 && nc < cols
        if grid[nr][nc] != '#' && !visited.key?([nr, nc])
          visited[[nr, nc]] = dist + 1
          queue << [nr, nc, dist + 1]
        end
      end
    end
  end

  # Count cells reachable in exactly 'steps' steps
  # A cell reachable in d steps can be reached in d+2, d+4, ... steps
  target_parity = steps % 2

  visited.values.count { |d| d <= steps && d % 2 == target_parity }
end

def count_reachable_infinite_bfs(grid, start, steps)
  rows = grid.length
  cols = grid[0].length

  visited = { [start[0], start[1]] => 0 }
  queue = [[start[0], start[1], 0]]

  directions = [[-1, 0], [1, 0], [0, -1], [0, 1]]

  while !queue.empty?
    r, c, dist = queue.shift

    next if dist >= steps

    directions.each do |dr, dc|
      nr, nc = r + dr, c + dc

      # Map to grid coordinates (infinite tiling)
      gr = nr % rows
      gc = nc % cols

      if grid[gr][gc] != '#' && !visited.key?([nr, nc])
        visited[[nr, nc]] = dist + 1
        queue << [nr, nc, dist + 1]
      end
    end
  end

  target_parity = steps % 2
  visited.values.count { |d| d <= steps && d % 2 == target_parity }
end

def count_reachable_infinite(grid, start, steps)
  rows = grid.length
  size = rows
  half = size / 2

  if steps <= size * 2
    return count_reachable_infinite_bfs(grid, start, steps)
  end

  # The number of full grid widths we travel
  n = (steps - half) / size

  # Calculate reachable counts for n=0, 1, 2 to determine the quadratic
  y0 = count_reachable_infinite_bfs(grid, start, half)
  y1 = count_reachable_infinite_bfs(grid, start, half + size)
  y2 = count_reachable_infinite_bfs(grid, start, half + 2 * size)

  # Solve for a, b, c using Lagrange interpolation
  # f(x) = y0 + (y1-y0)*x + (y2-2*y1+y0)*x*(x-1)/2
  a = (y2 - 2 * y1 + y0) / 2
  b = y1 - y0 - a
  c = y0

  a * n * n + b * n + c
end

def part1(grid, start)
  count_reachable(grid, start, 64)
end

def part2(grid, start)
  count_reachable_infinite(grid, start, 26501365)
end

def main
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  grid, start = parse_input(input_path)

  puts "Part 1: #{part1(grid, start)}"
  puts "Part 2: #{part2(grid, start)}"
end

main if __FILE__ == $0
