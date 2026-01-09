#!/usr/bin/env ruby
require 'set'

def parse_input(filename)
  File.readlines(filename).map(&:strip).reject(&:empty?)
end

def priority(char)
  if char.match?(/[a-z]/)
    char.ord - 'a'.ord + 1
  else
    char.ord - 'A'.ord + 27
  end
end

def part1(rucksacks)
  rucksacks.sum do |rucksack|
    mid = rucksack.length / 2
    first = rucksack[0...mid].chars.to_set
    second = rucksack[mid..].chars.to_set
    common = (first & second).first
    priority(common)
  end
end

def part2(rucksacks)
  rucksacks.each_slice(3).sum do |group|
    common = group.map { |r| r.chars.to_set }.reduce(:&).first
    priority(common)
  end
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  rucksacks = parse_input(input_file)

  puts "Part 1: #{part1(rucksacks)}"
  puts "Part 2: #{part2(rucksacks)}"
end

main if __FILE__ == $PROGRAM_NAME
