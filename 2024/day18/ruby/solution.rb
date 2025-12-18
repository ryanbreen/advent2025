#!/usr/bin/env ruby

# Day 18: RAM Run
# BFS pathfinding on a corrupted memory grid

def parse_input(filename)
  positions = []
  File.readlines(filename).each do |line|
    line = line.strip
    next if line.empty?
    x, y = line.split(',').map(&:to_i)
    positions << [x, y]
  end
  positions
end

def bfs(corrupted, size = 71)
  start = [0, 0]
  goal = [size - 1, size - 1]

  return -1 if corrupted.include?(start) || corrupted.include?(goal)

  queue = [[start, 0]]
  visited = { start => true }
  directions = [[0, 1], [0, -1], [1, 0], [-1, 0]]

  until queue.empty?
    pos, steps = queue.shift
    x, y = pos

    return steps if pos == goal

    directions.each do |dx, dy|
      nx, ny = x + dx, y + dy
      next_pos = [nx, ny]

      if nx >= 0 && nx < size && ny >= 0 && ny < size &&
         !visited[next_pos] && !corrupted.include?(next_pos)
        visited[next_pos] = true
        queue << [next_pos, steps + 1]
      end
    end
  end

  -1
end

def part1(positions, num_bytes = 1024, size = 71)
  corrupted = positions.take(num_bytes).to_set
  bfs(corrupted, size)
end

def part2(positions, size = 71)
  left = 0
  right = positions.length

  while left < right
    mid = (left + right) / 2
    corrupted = positions.take(mid + 1).to_set
    if bfs(corrupted, size) == -1
      right = mid
    else
      left = mid + 1
    end
  end

  blocking_pos = positions[left]
  "#{blocking_pos[0]},#{blocking_pos[1]}"
end

if __FILE__ == $0
  require 'set'

  positions = parse_input("../input.txt")

  puts "Part 1: #{part1(positions)}"
  puts "Part 2: #{part2(positions)}"
end
