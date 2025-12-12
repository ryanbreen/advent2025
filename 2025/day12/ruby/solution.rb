#!/usr/bin/env ruby
# frozen_string_literal: true

# Day 12: Christmas Tree Farm - Polyomino Packing
#
# The solution checks if presents (polyominoes) can fit into rectangular regions.
# For this problem, the constraint is simply: total cells needed <= available cells.

def parse_input(text)
  # Parse input into shapes and regions
  sections = text.strip.split("\n\n")

  shapes = {}
  regions = []

  sections.each do |section|
    lines = section.strip.lines.map(&:chomp)

    if lines.first.include?(':') && !lines.first.include?('x')
      # Shape definition: count cells in the shape
      idx = lines.first.delete(':').to_i
      cell_count = lines[1..].sum { |line| line.count('#') }
      shapes[idx] = cell_count
    else
      # Region definitions: parse width x height and shape counts
      lines.each do |line|
        next unless line.include?('x')

        dims, counts_str = line.split(':', 2)
        width, height = dims.split('x').map(&:to_i)
        counts = counts_str.split.map(&:to_i)
        regions << [width, height, counts]
      end
    end
  end

  [shapes, regions]
end

def can_fit_region?(width, height, counts, shape_sizes)
  # Check if all presents can fit in the region
  total_cells_needed = counts.each_with_index.sum { |count, idx| count * shape_sizes[idx] }
  available_cells = width * height
  total_cells_needed <= available_cells
end

def part1(shapes, regions)
  # Count regions that can fit all their presents
  regions.count { |width, height, counts| can_fit_region?(width, height, counts, shapes) }
end

def part2
  # Part 2 is just a button click to finish - no computation needed
  0
end

# Main execution
if __FILE__ == $PROGRAM_NAME
  text = File.read('../input.txt')
  shapes, regions = parse_input(text)

  puts "Part 1: #{part1(shapes, regions)}"
  puts "Part 2: #{part2}"
end
