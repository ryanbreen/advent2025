#!/usr/bin/env ruby
# frozen_string_literal: true

# Day 17: Clumsy Crucible - Dijkstra's shortest path with movement constraints

require 'set'

# Parse the grid of heat loss values
def parse_input(filename)
  File.read(filename).strip.lines.map { |line| line.chomp.chars.map(&:to_i) }
end

# Priority queue implementation using a binary heap
class PriorityQueue
  def initialize
    @heap = []
  end

  def push(priority, item)
    @heap << [priority, item]
    bubble_up(@heap.size - 1)
  end

  def pop
    return nil if @heap.empty?

    swap(0, @heap.size - 1)
    result = @heap.pop
    bubble_down(0) unless @heap.empty?
    result
  end

  def empty?
    @heap.empty?
  end

  private

  def bubble_up(index)
    parent = (index - 1) / 2
    while index > 0 && @heap[parent][0] > @heap[index][0]
      swap(index, parent)
      index = parent
      parent = (index - 1) / 2
    end
  end

  def bubble_down(index)
    loop do
      smallest = index
      left = 2 * index + 1
      right = 2 * index + 2

      smallest = left if left < @heap.size && @heap[left][0] < @heap[smallest][0]
      smallest = right if right < @heap.size && @heap[right][0] < @heap[smallest][0]

      break if smallest == index

      swap(index, smallest)
      index = smallest
    end
  end

  def swap(i, j)
    @heap[i], @heap[j] = @heap[j], @heap[i]
  end
end

# Find minimum heat loss path using Dijkstra's algorithm
# State: (row, col, direction, consecutive_steps)
# Directions: 0=right, 1=down, 2=left, 3=up
def dijkstra(grid, min_straight, max_straight)
  rows = grid.size
  cols = grid[0].size

  # Direction deltas: right, down, left, up
  dr = [0, 1, 0, -1]
  dc = [1, 0, -1, 0]

  # Priority queue: stores [priority, [row, col, direction, consecutive]]
  pq = PriorityQueue.new
  pq.push(0, [0, 0, -1, 0]) # -1 direction means no direction yet

  visited = Set.new

  until pq.empty?
    heat, state = pq.pop
    r, c, d, consec = state

    # Check if we reached the goal
    if r == rows - 1 && c == cols - 1
      return heat if min_straight.zero? || consec >= min_straight
    end

    state_key = [r, c, d, consec]
    next if visited.include?(state_key)

    visited.add(state_key)

    # Try all four directions
    4.times do |nd|
      # Can't reverse direction
      next if d != -1 && nd == (d + 2) % 4

      nr = r + dr[nd]
      nc = c + dc[nd]

      # Bounds check
      next if nr < 0 || nr >= rows || nc < 0 || nc >= cols

      # Check consecutive constraints
      if nd == d
        # Continuing in same direction
        new_consec = consec + 1
        next if new_consec > max_straight
      else
        # Turning - must have gone min_straight in previous direction first
        next if d != -1 && consec < min_straight

        new_consec = 1
      end

      new_heat = heat + grid[nr][nc]
      new_state = [nr, nc, nd, new_consec]

      pq.push(new_heat, new_state) unless visited.include?(new_state)
    end
  end

  -1 # No path found
end

# Part 1: Normal crucible, max 3 consecutive blocks
def part1(grid)
  dijkstra(grid, 0, 3)
end

# Part 2: Ultra crucible, min 4 and max 10 consecutive blocks
def part2(grid)
  dijkstra(grid, 4, 10)
end

def main
  input_path = File.join(__dir__, '..', 'input.txt')
  grid = parse_input(input_path)

  puts "Part 1: #{part1(grid)}"
  puts "Part 2: #{part2(grid)}"
end

main if __FILE__ == $PROGRAM_NAME
