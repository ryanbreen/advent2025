#!/usr/bin/env ruby

def parse_input
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  File.readlines(input_path).map(&:strip).reject(&:empty?)
end

def part1(numbers)
  num_bits = numbers[0].length
  gamma = 0

  num_bits.times do |pos|
    ones = numbers.count { |n| n[pos] == '1' }
    zeros = numbers.length - ones

    gamma |= (1 << (num_bits - 1 - pos)) if ones >= zeros
  end

  # epsilon is bitwise NOT of gamma (within num_bits)
  epsilon = gamma ^ ((1 << num_bits) - 1)

  gamma * epsilon
end

def find_rating(numbers, use_most_common)
  num_bits = numbers[0].length
  candidates = numbers.dup

  num_bits.times do |pos|
    break if candidates.length == 1

    ones = candidates.count { |n| n[pos] == '1' }
    zeros = candidates.length - ones

    target = if use_most_common
               ones >= zeros ? '1' : '0'
             else
               zeros <= ones ? '0' : '1'
             end

    candidates = candidates.select { |n| n[pos] == target }
  end

  candidates[0].to_i(2)
end

def part2(numbers)
  oxygen = find_rating(numbers, true)
  co2 = find_rating(numbers, false)
  oxygen * co2
end

if __FILE__ == $PROGRAM_NAME
  numbers = parse_input
  puts "Part 1: #{part1(numbers)}"
  puts "Part 2: #{part2(numbers)}"
end
