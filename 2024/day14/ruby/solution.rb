#!/usr/bin/env ruby

require 'pathname'
require 'set'

input_text = Pathname.new(__FILE__).dirname.parent.join('input.txt').read.strip

WIDTH = 101
HEIGHT = 103

# Parse robot positions and velocities
def parse_robots(text)
  robots = []
  text.each_line do |line|
    if line =~ /p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/
      px, py, vx, vy = $1.to_i, $2.to_i, $3.to_i, $4.to_i
      robots << [px, py, vx, vy]
    end
  end
  robots
end

# Simulate robot movement for given seconds
def simulate(robots, seconds)
  positions = []
  robots.each do |px, py, vx, vy|
    # Position after 'seconds' time, with wrapping
    # Ruby's % handles negative numbers correctly
    new_x = (px + vx * seconds) % WIDTH
    new_y = (py + vy * seconds) % HEIGHT
    positions << [new_x, new_y]
  end
  positions
end

# Count robots in each quadrant, excluding middle row/column
def count_quadrants(positions)
  mid_x = WIDTH / 2   # 50
  mid_y = HEIGHT / 2  # 51

  q1 = q2 = q3 = q4 = 0

  positions.each do |x, y|
    next if x == mid_x || y == mid_y  # Skip robots on middle lines

    if x < mid_x && y < mid_y
      q1 += 1  # Top-left
    elsif x > mid_x && y < mid_y
      q2 += 1  # Top-right
    elsif x < mid_x && y > mid_y
      q3 += 1  # Bottom-left
    else
      q4 += 1  # Bottom-right
    end
  end

  [q1, q2, q3, q4]
end

# Part 1: Safety factor after 100 seconds
def part1(robots)
  positions = simulate(robots, 100)
  q1, q2, q3, q4 = count_quadrants(positions)
  q1 * q2 * q3 * q4
end

# Part 2: Find when robots form a Christmas tree pattern
def part2(robots)
  # The Christmas tree appears when robots cluster together
  # Look for a frame with a long horizontal line of robots (tree base/border)
  (1..(WIDTH * HEIGHT)).each do |seconds|
    positions = simulate(robots, seconds)
    pos_set = positions.to_set

    # Look for a horizontal line of at least 20 consecutive robots
    HEIGHT.times do |y|
      max_consecutive = 0
      consecutive = 0

      WIDTH.times do |x|
        if pos_set.include?([x, y])
          consecutive += 1
          max_consecutive = [max_consecutive, consecutive].max
        else
          consecutive = 0
        end
      end

      return seconds if max_consecutive >= 20
    end
  end

  -1
end

if __FILE__ == $0
  robots = parse_robots(input_text)
  puts "Part 1: #{part1(robots)}"
  puts "Part 2: #{part2(robots)}"
end
