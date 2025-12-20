#!/usr/bin/env ruby

# Day 19: Linen Layout
#
# Given a set of towel patterns and desired designs, determine:
# - Part 1: How many designs can be formed using the available patterns
# - Part 2: Total number of ways to form all possible designs
#
# Uses dynamic programming with memoization to count pattern combinations.

def count_ways(design, patterns)
  memo = {}
  dp = lambda do |pos|
    return 1 if pos == design.length
    return memo[pos] if memo.key?(pos)

    memo[pos] = patterns.sum do |pattern|
      design[pos, pattern.length] == pattern ? dp.call(pos + pattern.length) : 0
    end
  end
  dp.call(0)
end

def part1(designs, patterns)
  designs.count { |d| count_ways(d, patterns) > 0 }
end

def part2(designs, patterns)
  designs.sum { |d| count_ways(d, patterns) }
end

if __FILE__ == $0
  input_path = File.join(File.dirname(__FILE__), "..", "input.txt")
  input_text = File.read(input_path).strip

  # Parse input
  parts = input_text.split("\n\n")
  patterns = parts[0].split(",").map(&:strip)
  designs = parts[1].strip.split("\n")

  puts "Part 1: #{part1(designs, patterns)}"
  puts "Part 2: #{part2(designs, patterns)}"
end
