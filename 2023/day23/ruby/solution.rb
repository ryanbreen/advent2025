#!/usr/bin/env ruby
# Day 23: A Long Walk - Longest path through hiking trails.

def parse_input(filename)
  File.read(filename).strip.split("\n")
end

def find_junctions(grid)
  rows = grid.length
  cols = grid[0].length
  junctions = []

  # Start and end points
  start_col = grid[0].index('.')
  end_col = grid[rows - 1].index('.')
  junctions << [0, start_col]
  junctions << [rows - 1, end_col]

  # Find intersections (cells with 3+ walkable neighbors)
  rows.times do |r|
    cols.times do |c|
      next if grid[r][c] == '#'

      neighbors = 0
      [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
        nr, nc = r + dr, c + dc
        if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#'
          neighbors += 1
        end
      end

      junctions << [r, c] if neighbors >= 3
    end
  end

  junctions.uniq
end

def build_graph(grid, junctions, respect_slopes)
  rows = grid.length
  cols = grid[0].length

  # Create a set of junction coordinates for fast lookup
  junction_set = {}
  junctions.each_with_index do |j, idx|
    junction_set[[j[0], j[1]]] = idx
  end

  # Direction mappings for slopes
  slope_dirs = {
    '^' => [-1, 0],
    'v' => [1, 0],
    '<' => [0, -1],
    '>' => [0, 1]
  }

  # Graph: array of arrays, graph[i] = [[neighbor_idx, dist], ...]
  graph = Array.new(junctions.length) { [] }
  directions = [[-1, 0], [1, 0], [0, -1], [0, 1]]

  junctions.each_with_index do |start_junction, start_idx|
    # BFS from each junction to find reachable junctions
    stack = [[start_junction[0], start_junction[1], 0]]
    visited = { (start_junction[0] * cols + start_junction[1]) => true }

    while !stack.empty?
      r, c, dist = stack.pop

      if dist > 0
        junction_idx = junction_set[[r, c]]
        if junction_idx
          # Found another junction
          graph[start_idx] << [junction_idx, dist]
          next
        end
      end

      # Explore neighbors
      directions.each do |dr, dc|
        nr, nc = r + dr, c + dc

        next unless nr >= 0 && nr < rows && nc >= 0 && nc < cols
        next if grid[nr][nc] == '#'

        key = nr * cols + nc
        next if visited[key]

        # Check slope constraints for Part 1
        if respect_slopes
          cell = grid[r][c]
          if slope_dirs.key?(cell)
            req_dr, req_dc = slope_dirs[cell]
            next if dr != req_dr || dc != req_dc
          end
        end

        visited[key] = true
        stack << [nr, nc, dist + 1]
      end
    end
  end

  graph
end

def longest_path_dfs(graph, start_idx, end_idx)
  n = graph.length
  visited = Array.new(n, false)

  # Iterative DFS with explicit stack to avoid Ruby lambda overhead
  # But for longest path with backtracking, we need recursion
  # Use a simple recursive approach with minimal overhead

  dfs = nil
  dfs = ->(node) {
    return 0 if node == end_idx

    visited[node] = true
    max_dist = nil

    graph[node].each do |neighbor, dist|
      unless visited[neighbor]
        result = dfs.call(neighbor)
        if result
          candidate = dist + result
          max_dist = candidate if max_dist.nil? || candidate > max_dist
        end
      end
    end

    visited[node] = false
    max_dist
  }

  dfs.call(start_idx)
end

def solve(grid, respect_slopes)
  rows = grid.length
  cols = grid[0].length
  start_pos = [0, grid[0].index('.')]
  end_pos = [rows - 1, grid[rows - 1].index('.')]

  junctions = find_junctions(grid)

  # Find indices of start and end in junctions array
  start_idx = junctions.index(start_pos)
  end_idx = junctions.index(end_pos)

  graph = build_graph(grid, junctions, respect_slopes)

  longest_path_dfs(graph, start_idx, end_idx)
end

def part1(grid)
  solve(grid, true)
end

def part2(grid)
  solve(grid, false)
end

def main
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  grid = parse_input(input_path)
  puts "Part 1: #{part1(grid)}"
  puts "Part 2: #{part2(grid)}"
end

main if __FILE__ == $0
