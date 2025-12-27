#!/usr/bin/env ruby
# frozen_string_literal: true

input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip
lines = input_text.split("\n")

def parse_races(lines)
  times = lines[0].split(':')[1].split.map(&:to_i)
  distances = lines[1].split(':')[1].split.map(&:to_i)
  times.zip(distances)
end

def count_ways_to_win(time, record)
  # Count the number of ways to beat the record.
  #
  # If we hold the button for t ms, we travel t * (time - t) mm.
  # We need: t * (time - t) > record
  # Solving: -t^2 + time*t - record > 0
  # Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2

  discriminant = time * time - 4 * record
  return 0 if discriminant <= 0

  sqrt_d = Math.sqrt(discriminant)
  t_low = (time - sqrt_d) / 2.0
  t_high = (time + sqrt_d) / 2.0

  # We need integer values strictly between the roots
  first = t_low.floor + 1
  last = t_high.ceil - 1

  return 0 if last < first

  last - first + 1
end

def part1(lines)
  races = parse_races(lines)
  races.map { |time, record| count_ways_to_win(time, record) }.reduce(1, :*)
end

def part2(lines)
  races = parse_races(lines)
  time = races.map { |t, _| t.to_s }.join.to_i
  record = races.map { |_, d| d.to_s }.join.to_i
  count_ways_to_win(time, record)
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
