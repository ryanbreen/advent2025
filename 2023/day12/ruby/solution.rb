# frozen_string_literal: true

# Advent of Code 2023 Day 12: Hot Springs

def count_arrangements(pattern, groups)
  memo = Hash.new do |hash, key|
    pos, group_idx, current_run = key

    # Base case: reached end of pattern
    if pos == pattern.length
      hash[key] = if group_idx == groups.length && current_run.zero?
                    1
                  elsif group_idx == groups.length - 1 && groups[group_idx] == current_run
                    1
                  else
                    0
                  end
      next hash[key]
    end

    result = 0
    char = pattern[pos]

    # Option 1: Place operational spring (.)
    if char == '.' || char == '?'
      if current_run.zero?
        # No active run, just move forward
        result += hash[[pos + 1, group_idx, 0]]
      elsif group_idx < groups.length && groups[group_idx] == current_run
        # End current run if it matches expected group size
        result += hash[[pos + 1, group_idx + 1, 0]]
      end
      # Otherwise invalid (run doesn't match group)
    end

    # Option 2: Place damaged spring (#)
    if char == '#' || char == '?'
      if group_idx < groups.length && current_run < groups[group_idx]
        # Can extend current run
        result += hash[[pos + 1, group_idx, current_run + 1]]
      end
      # Otherwise invalid (exceeds group size or no more groups)
    end

    hash[key] = result
  end

  memo[[0, 0, 0]]
end

def parse_line(line)
  pattern, groups_str = line.strip.split
  groups = groups_str.split(',').map(&:to_i)
  [pattern, groups]
end

def unfold(pattern, groups, times: 5)
  unfolded_pattern = ([pattern] * times).join('?')
  unfolded_groups = groups * times
  [unfolded_pattern, unfolded_groups]
end

def part1(lines)
  lines.reject { |line| line.strip.empty? }.sum do |line|
    pattern, groups = parse_line(line)
    count_arrangements(pattern, groups)
  end
end

def part2(lines)
  lines.reject { |line| line.strip.empty? }.sum do |line|
    pattern, groups = parse_line(line)
    unfolded_pattern, unfolded_groups = unfold(pattern, groups)
    count_arrangements(unfolded_pattern, unfolded_groups)
  end
end

def main
  lines = File.readlines(File.join(__dir__, '..', 'input.txt'))

  puts "Part 1: #{part1(lines)}"
  puts "Part 2: #{part2(lines)}"
end

main if __FILE__ == $PROGRAM_NAME
