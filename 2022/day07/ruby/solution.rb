#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_filesystem(lines)
  path = []
  dir_sizes = Hash.new(0)

  lines.each do |line|
    if line.start_with?('$ cd')
      target = line[5..]
      if target == '/'
        path = ['/']
      elsif target == '..'
        path.pop
      else
        path.push(target)
      end
    elsif line.start_with?('$ ls')
      next
    elsif line.start_with?('dir ')
      next
    else
      # It's a file with size
      size = line.split[0].to_i
      # Add size to current directory and all parent directories
      path.length.times do |i|
        dir_path = path[0..i].join('/')
        dir_path = '/' if dir_path.empty?
        dir_sizes[dir_path] += size
      end
    end
  end

  dir_sizes
end

def part1(dir_sizes)
  dir_sizes.values.select { |size| size <= 100_000 }.sum
end

def part2(dir_sizes)
  total_space = 70_000_000
  needed_space = 30_000_000
  used_space = dir_sizes['/']
  free_space = total_space - used_space
  need_to_free = needed_space - free_space

  dir_sizes.values.select { |size| size >= need_to_free }.min
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  lines = File.read(input_file).strip.split("\n")
  dir_sizes = parse_filesystem(lines)

  puts "Part 1: #{part1(dir_sizes)}"
  puts "Part 2: #{part2(dir_sizes)}"
end

main
