#!/usr/bin/env ruby

def find_marker(data, window_size)
  # Find first position where last window_size characters are all unique
  (window_size..data.length).each do |i|
    window = data[i - window_size, window_size]
    return i if window.chars.uniq.length == window_size
  end
  -1
end

def part1(data)
  find_marker(data, 4)
end

def part2(data)
  find_marker(data, 14)
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  data = File.read(input_file).strip

  puts "Part 1: #{part1(data)}"
  puts "Part 2: #{part2(data)}"
end

main
