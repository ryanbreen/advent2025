#!/usr/bin/env ruby

# Solver class encapsulating stone counting logic with memoization
class StoneSolver
  def initialize
    @memo = Hash.new { |h, k| h[k] = {} }
  end

  # Count how many stones result from a single stone after N blinks
  def count_stones(value, blinks)
    # Base case: no more blinks
    return 1 if blinks.zero?

    # Check memo cache
    return @memo[value][blinks] if @memo[value].key?(blinks)

    result = if value.zero?
      # Rule 1: 0 becomes 1
      count_stones(1, blinks - 1)
    else
      s = value.to_s
      if s.length.even?
        # Rule 2: Even number of digits -> split in half
        mid = s.length / 2
        left = s[0...mid].to_i
        right = s[mid..].to_i
        count_stones(left, blinks - 1) + count_stones(right, blinks - 1)
      else
        # Rule 3: Multiply by 2024
        count_stones(value * 2024, blinks - 1)
      end
    end

    @memo[value][blinks] = result
  end

  def solve(stones, blinks)
    stones.sum { |stone| count_stones(stone, blinks) }
  end
end

# Read input from ../input.txt
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
input_text = File.read(input_path).strip

# Parse input - space-separated numbers
stones = input_text.split.map(&:to_i)

# Create solver instance
solver = StoneSolver.new

puts "Part 1: #{solver.solve(stones, 25)}"
puts "Part 2: #{solver.solve(stones, 75)}"
