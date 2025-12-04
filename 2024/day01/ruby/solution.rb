#!/usr/bin/env ruby

# Read input file
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
input_text = File.read(input_path).strip

# Parse input into two lists
left_list = []
right_list = []

input_text.each_line do |line|
  left, right = line.split.map(&:to_i)
  left_list << left
  right_list << right
end

def part1(left_list, right_list)
  # Sort both lists
  sorted_left = left_list.sort
  sorted_right = right_list.sort

  # Calculate total distance
  total_distance = 0
  sorted_left.each_with_index do |left_val, i|
    total_distance += (left_val - sorted_right[i]).abs
  end

  total_distance
end

def part2(left_list, right_list)
  # Count occurrences in right list
  right_counts = Hash.new(0)
  right_list.each { |num| right_counts[num] += 1 }

  # Calculate similarity score
  similarity_score = 0
  left_list.each do |num|
    similarity_score += num * right_counts[num]
  end

  similarity_score
end

puts "Part 1: #{part1(left_list, right_list)}"
puts "Part 2: #{part2(left_list, right_list)}"
