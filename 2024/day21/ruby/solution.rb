#!/usr/bin/env ruby
# Day 21: Keypad Conundrum - Robot chain control with shortest path optimization

# Keypad layouts - positions as [row, col]
NUMERIC = {
  '7' => [0, 0], '8' => [0, 1], '9' => [0, 2],
  '4' => [1, 0], '5' => [1, 1], '6' => [1, 2],
  '1' => [2, 0], '2' => [2, 1], '3' => [2, 2],
  '0' => [3, 1], 'A' => [3, 2]
}
NUMERIC_GAP = [3, 0]

DIRECTIONAL = {
  '^' => [0, 1], 'A' => [0, 2],
  '<' => [1, 0], 'v' => [1, 1], '>' => [1, 2]
}
DIRECTIONAL_GAP = [0, 0]

# Find all shortest paths from start to end, avoiding gap
def shortest_paths(keypad, gap, start_char, end_char)
  sr, sc = keypad[start_char]
  er, ec = keypad[end_char]

  paths = []

  dfs = lambda do |r, c, path|
    return if [r, c] == gap
    if [r, c] == [er, ec]
      paths << path
      return
    end

    # Move vertically toward target
    if r < er
      dfs.call(r + 1, c, path + 'v')
    elsif r > er
      dfs.call(r - 1, c, path + '^')
    end

    # Move horizontally toward target
    if c < ec
      dfs.call(r, c + 1, path + '>')
    elsif c > ec
      dfs.call(r, c - 1, path + '<')
    end
  end

  dfs.call(sr, sc, '')
  paths.empty? ? [''] : paths
end

# Memoization cache
$memo_cache = {}

# Minimum presses needed to move from from_char to to_char and press, at given depth
def min_presses_for_move(from_char, to_char, depth, is_numeric)
  cache_key = [from_char, to_char, depth, is_numeric]
  return $memo_cache[cache_key] if $memo_cache.key?(cache_key)

  keypad, gap = is_numeric ? [NUMERIC, NUMERIC_GAP] : [DIRECTIONAL, DIRECTIONAL_GAP]
  paths = shortest_paths(keypad, gap, from_char, to_char)

  result = if depth == 0
    # At human level, just return path length + 1 for 'A' press
    paths.map(&:length).min + 1
  else
    best = Float::INFINITY
    paths.each do |path|
      # Need to type path + 'A' on the directional keypad above
      sequence = path + 'A'
      cost = 0
      current = 'A'
      sequence.each_char do |char|
        cost += min_presses_for_move(current, char, depth - 1, false)
        current = char
      end
      best = [best, cost].min
    end
    best
  end

  $memo_cache[cache_key] = result
  result
end

# Compute minimum presses to type code on numeric keypad with given robot depth
def solve_code(code, depth)
  total = 0
  current = 'A'
  code.each_char do |char|
    total += min_presses_for_move(current, char, depth, true)
    current = char
  end
  total
end

# Compute complexity: length * numeric part of code
def complexity(code, length)
  numeric_part = code.sub(/A$/, '').to_i
  length * numeric_part
end

# Part 1: 2 intermediate robots (depth = 2)
def part1(codes)
  total = 0
  codes.each do |code|
    length = solve_code(code, 2)
    total += complexity(code, length)
  end
  total
end

# Part 2: 25 intermediate robots (depth = 25)
def part2(codes)
  total = 0
  codes.each do |code|
    length = solve_code(code, 25)
    total += complexity(code, length)
  end
  total
end

# Main
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
codes = File.read(input_path).strip.split("\n").map(&:strip).reject(&:empty?)

puts "Part 1: #{part1(codes)}"
puts "Part 2: #{part2(codes)}"
