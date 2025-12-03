#!/usr/bin/env ruby

input_file = File.join(__dir__, '..', 'input.txt')
input_text = File.read(input_file).strip
lines = input_text.split("\n")

def part1(lines)
  total = 0

  lines.each do |line|
    digits = line.chars.map(&:to_i)
    n = digits.length

    # Precompute max suffix for each position
    # max_suffix[i] = maximum digit from position i to end
    max_suffix = Array.new(n)
    max_suffix[n - 1] = digits[n - 1]

    (n - 2).downto(0) do |i|
      max_suffix[i] = [digits[i], max_suffix[i + 1]].max
    end

    # Find maximum two-digit number
    max_joltage = 0
    (0...n - 1).each do |i|
      # For position i as first digit, the best second digit is max_suffix[i+1]
      joltage = digits[i] * 10 + max_suffix[i + 1]
      max_joltage = [max_joltage, joltage].max
    end

    total += max_joltage
  end

  total
end

def part2(lines)
  total = 0

  lines.each do |line|
    digits = line.chars.map(&:to_i)
    n = digits.length
    k = 12

    # Greedy algorithm to select k digits forming maximum number
    result = []
    current_pos = 0

    k.times do |i|
      # For position i in result, search from current_pos to ensure
      # enough digits remain for the rest of the result
      remaining_needed = k - i
      search_end = n - remaining_needed + 1
      max_digit = -1
      max_pos = current_pos

      (current_pos...search_end).each do |j|
        if digits[j] > max_digit
          max_digit = digits[j]
          max_pos = j
        end
      end

      result << max_digit
      current_pos = max_pos + 1
    end

    # Convert result array to integer
    joltage = result.join.to_i
    total += joltage
  end

  total
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
