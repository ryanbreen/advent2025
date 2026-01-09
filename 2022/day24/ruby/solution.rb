#!/usr/bin/env ruby

def parse_input(text)
  lines = text.strip.split("\n")
  height = lines.length
  width = lines[0].length

  # Inner dimensions (excluding walls)
  inner_h = height - 2
  inner_w = width - 2

  blizzards = []
  lines.each_with_index do |line, r|
    line.chars.each_with_index do |ch, c|
      if '^v<>'.include?(ch)
        blizzards << [r, c, ch]
      end
    end
  end

  # Find start and end positions
  start_pos = [0, lines[0].index('.')]
  end_pos = [height - 1, lines[-1].index('.')]

  [blizzards, height, width, inner_h, inner_w, start_pos, end_pos]
end

def lcm(a, b)
  a * b / a.gcd(b)
end

def get_blizzard_positions(blizzards, inner_h, inner_w, time)
  positions = Set.new

  blizzards.each do |r, c, direction|
    # Adjust to inner coordinates (subtract 1 for wall)
    ir = r - 1
    ic = c - 1

    case direction
    when '^'
      nr = (ir - time) % inner_h
      nc = ic
    when 'v'
      nr = (ir + time) % inner_h
      nc = ic
    when '<'
      nr = ir
      nc = (ic - time) % inner_w
    when '>'
      nr = ir
      nc = (ic + time) % inner_w
    end

    # Convert back to full coordinates
    positions.add([nr + 1, nc + 1])
  end

  positions
end

def bfs(blizzards, height, width, inner_h, inner_w, start_pos, end_pos, start_time = 0)
  period = lcm(inner_h, inner_w)

  # Precompute blizzard positions for all times in one period
  blizzard_cache = {}
  (0...period).each do |t|
    blizzard_cache[t] = get_blizzard_positions(blizzards, inner_h, inner_w, t)
  end

  # BFS: state = [time, row, col]
  queue = [[start_time, start_pos[0], start_pos[1]]]
  visited = Set.new
  visited.add([start_time % period, start_pos[0], start_pos[1]])

  # wait, up, down, left, right
  directions = [[0, 0], [-1, 0], [1, 0], [0, -1], [0, 1]]

  while !queue.empty?
    time, r, c = queue.shift

    if [r, c] == end_pos
      return time
    end

    next_time = time + 1
    next_blizzards = blizzard_cache[next_time % period]

    directions.each do |dr, dc|
      nr = r + dr
      nc = c + dc

      # Check bounds
      if [nr, nc] == start_pos || [nr, nc] == end_pos
        # Always valid
      elsif nr <= 0 || nr >= height - 1 || nc <= 0 || nc >= width - 1
        next  # Wall
      end

      # Check blizzards
      next if next_blizzards.include?([nr, nc])

      state = [next_time % period, nr, nc]
      unless visited.include?(state)
        visited.add(state)
        queue << [next_time, nr, nc]
      end
    end
  end

  -1  # No path found
end

def part1(text)
  blizzards, height, width, inner_h, inner_w, start_pos, end_pos = parse_input(text)
  bfs(blizzards, height, width, inner_h, inner_w, start_pos, end_pos, 0)
end

def part2(text)
  blizzards, height, width, inner_h, inner_w, start_pos, end_pos = parse_input(text)

  # Trip 1: start to end
  t1 = bfs(blizzards, height, width, inner_h, inner_w, start_pos, end_pos, 0)

  # Trip 2: end to start
  t2 = bfs(blizzards, height, width, inner_h, inner_w, end_pos, start_pos, t1)

  # Trip 3: start to end again
  t3 = bfs(blizzards, height, width, inner_h, inner_w, start_pos, end_pos, t2)

  t3
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

require 'set'

main if __FILE__ == $0
