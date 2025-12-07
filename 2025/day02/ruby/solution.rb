#!/usr/bin/env ruby

require 'set'

# Read input file
input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip

def parse_ranges(input_text)
  input_text.split(',').map do |part|
    parts = part.strip.split('-')
    [parts[0].to_i, parts[1].to_i] if parts.length == 2
  end.compact
end

# Part 1: Generate invalid IDs (pattern repeated exactly twice)
# Instead of iterating through all numbers in the range, we enumerate patterns
# and check if their repeated form falls within the range
def generate_invalid_ids_part1(start_val, end_val)
  invalid_ids = []

  start_digits = start_val.to_s.size
  end_digits = end_val.to_s.size

  (start_digits..end_digits).each do |total_digits|
    next unless total_digits.even?

    pattern_digits = total_digits >> 1
    pattern_min = 10 ** (pattern_digits - 1)
    pattern_max = 10 ** pattern_digits - 1

    (pattern_min..pattern_max).each do |pattern|
      # Create the number by repeating the pattern twice
      repeated = (pattern.to_s * 2).to_i

      # Check if it falls within our range
      invalid_ids << repeated if repeated >= start_val && repeated <= end_val
    end
  end

  invalid_ids
end

# Part 2: Generate invalid IDs (pattern repeated at least twice)
# Enumerate all patterns and repetition counts
def generate_invalid_ids_part2(start_val, end_val)
  invalid_ids = Set.new

  start_digits = start_val.to_s.size
  end_digits = end_val.to_s.size

  (start_digits..end_digits).each do |total_digits|
    # Try all divisors of total_digits as pattern lengths
    1.upto(total_digits >> 1) do |pattern_digits|
      next unless (total_digits % pattern_digits).zero?

      repetitions = total_digits / pattern_digits
      next if repetitions < 2

      # Generate all patterns of this length
      pattern_min = pattern_digits == 1 ? 1 : 10 ** (pattern_digits - 1)
      pattern_max = 10 ** pattern_digits - 1

      (pattern_min..pattern_max).each do |pattern|
        # Create the repeated number
        repeated = (pattern.to_s * repetitions).to_i

        # Check if it falls within our range and has correct number of digits
        if repeated >= start_val && repeated <= end_val && repeated.to_s.size == total_digits
          invalid_ids << repeated
        end
      end
    end
  end

  invalid_ids
end

def part1(input_text)
  ranges = parse_ranges(input_text)
  total = 0
  ranges.each do |start_val, end_val|
    total += generate_invalid_ids_part1(start_val, end_val).sum
  end
  total
end

def part2(input_text)
  ranges = parse_ranges(input_text)
  total = 0
  ranges.each do |start_val, end_val|
    total += generate_invalid_ids_part2(start_val, end_val).sum
  end
  total
end

puts "Part 1: #{part1(input_text)}"
puts "Part 2: #{part2(input_text)}"
