#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

# 6 directions: +x, -x, +y, -y, +z, -z
DIRECTIONS = [
  [1, 0, 0], [-1, 0, 0],
  [0, 1, 0], [0, -1, 0],
  [0, 0, 1], [0, 0, -1]
].freeze

def parse_input(text)
  cubes = Set.new
  text.strip.each_line do |line|
    x, y, z = line.strip.split(',').map(&:to_i)
    cubes.add([x, y, z])
  end
  cubes
end

def part1(text)
  cubes = parse_input(text)
  surface_area = 0

  cubes.each do |x, y, z|
    DIRECTIONS.each do |dx, dy, dz|
      surface_area += 1 unless cubes.include?([x + dx, y + dy, z + dz])
    end
  end

  surface_area
end

def part2(text)
  cubes = parse_input(text)

  # Find bounding box with 1 unit padding
  xs = cubes.map { |c| c[0] }
  ys = cubes.map { |c| c[1] }
  zs = cubes.map { |c| c[2] }

  min_x = xs.min - 1
  max_x = xs.max + 1
  min_y = ys.min - 1
  max_y = ys.max + 1
  min_z = zs.min - 1
  max_z = zs.max + 1

  # BFS to find all exterior air cells
  exterior = Set.new
  queue = [[min_x, min_y, min_z]]
  exterior.add([min_x, min_y, min_z])

  until queue.empty?
    x, y, z = queue.shift

    DIRECTIONS.each do |dx, dy, dz|
      nx = x + dx
      ny = y + dy
      nz = z + dz

      # Stay within bounds
      next unless nx.between?(min_x, max_x) &&
                  ny.between?(min_y, max_y) &&
                  nz.between?(min_z, max_z)

      neighbor = [nx, ny, nz]

      # Skip cubes and already visited
      next if cubes.include?(neighbor) || exterior.include?(neighbor)

      exterior.add(neighbor)
      queue.push(neighbor)
    end
  end

  # Count faces touching exterior air
  surface_area = 0
  cubes.each do |x, y, z|
    DIRECTIONS.each do |dx, dy, dz|
      surface_area += 1 if exterior.include?([x + dx, y + dy, z + dz])
    end
  end

  surface_area
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')
  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
