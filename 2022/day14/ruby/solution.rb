#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

def parse_paths(text)
  rocks = Set.new
  text.strip.split("\n").each do |line|
    points = line.split(' -> ')
    (0...points.length - 1).each do |i|
      x1, y1 = points[i].split(',').map(&:to_i)
      x2, y2 = points[i + 1].split(',').map(&:to_i)

      if x1 == x2 # vertical line
        ([y1, y2].min..[y1, y2].max).each { |y| rocks.add([x1, y]) }
      else # horizontal line
        ([x1, x2].min..[x1, x2].max).each { |x| rocks.add([x, y1]) }
      end
    end
  end
  rocks
end

def simulate_sand(blocked, max_y, floor: false)
  x, y = 500, 0

  loop do
    # Check if sand has fallen below all rocks (into abyss)
    return nil if !floor && y > max_y

    # Try to move down
    if floor && y + 1 == max_y + 2
      # Hit the floor
      return [x, y]
    elsif !blocked.include?([x, y + 1])
      y += 1
    # Try to move down-left
    elsif !blocked.include?([x - 1, y + 1])
      x -= 1
      y += 1
    # Try to move down-right
    elsif !blocked.include?([x + 1, y + 1])
      x += 1
      y += 1
    # Sand comes to rest
    else
      return [x, y]
    end
  end
end

def part1(text)
  rocks = parse_paths(text)
  max_y = rocks.map { |_, y| y }.max
  blocked = rocks.dup
  count = 0

  loop do
    pos = simulate_sand(blocked, max_y)
    break if pos.nil?

    blocked.add(pos)
    count += 1
  end

  count
end

def part2(text)
  rocks = parse_paths(text)
  max_y = rocks.map { |_, y| y }.max
  blocked = rocks.dup
  count = 0

  loop do
    pos = simulate_sand(blocked, max_y, floor: true)
    blocked.add(pos)
    count += 1
    break if pos == [500, 0]
  end

  count
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')
  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
