#!/usr/bin/env ruby
# frozen_string_literal: true

# Advent of Code 2023 Day 14: Parabolic Reflector Dish
# Tilting platform with rolling rocks (O) and fixed rocks (#)

def parse_input(text)
  text.strip.lines.map { |line| line.chomp.chars }
end

def tilt_north!(grid)
  rows = grid.length
  cols = grid[0].length

  cols.times do |col|
    write_pos = 0
    rows.times do |row|
      case grid[row][col]
      when '#'
        write_pos = row + 1
      when 'O'
        grid[row][col] = '.'
        grid[write_pos][col] = 'O'
        write_pos += 1
      end
    end
  end
end

def tilt_south!(grid)
  rows = grid.length
  cols = grid[0].length

  cols.times do |col|
    write_pos = rows - 1
    (rows - 1).downto(0) do |row|
      case grid[row][col]
      when '#'
        write_pos = row - 1
      when 'O'
        grid[row][col] = '.'
        grid[write_pos][col] = 'O'
        write_pos -= 1
      end
    end
  end
end

def tilt_west!(grid)
  rows = grid.length
  cols = grid[0].length

  rows.times do |row|
    write_pos = 0
    cols.times do |col|
      case grid[row][col]
      when '#'
        write_pos = col + 1
      when 'O'
        grid[row][col] = '.'
        grid[row][write_pos] = 'O'
        write_pos += 1
      end
    end
  end
end

def tilt_east!(grid)
  rows = grid.length
  cols = grid[0].length

  rows.times do |row|
    write_pos = cols - 1
    (cols - 1).downto(0) do |col|
      case grid[row][col]
      when '#'
        write_pos = col - 1
      when 'O'
        grid[row][col] = '.'
        grid[row][write_pos] = 'O'
        write_pos -= 1
      end
    end
  end
end

def spin_cycle!(grid)
  tilt_north!(grid)
  tilt_west!(grid)
  tilt_south!(grid)
  tilt_east!(grid)
end

def grid_to_key(grid)
  grid.map(&:join).join("\n")
end

def calculate_load(grid)
  rows = grid.length
  grid.each_with_index.sum do |row, row_idx|
    row.count('O') * (rows - row_idx)
  end
end

def part1(grid)
  working_grid = grid.map(&:dup)
  tilt_north!(working_grid)
  calculate_load(working_grid)
end

def part2(grid)
  working_grid = grid.map(&:dup)
  target = 1_000_000_000

  seen = {}
  cycle_num = 0

  while cycle_num < target
    state = grid_to_key(working_grid)
    if seen.key?(state)
      cycle_start = seen[state]
      cycle_length = cycle_num - cycle_start
      remaining = (target - cycle_num) % cycle_length
      remaining.times { spin_cycle!(working_grid) }
      return calculate_load(working_grid)
    end

    seen[state] = cycle_num
    spin_cycle!(working_grid)
    cycle_num += 1
  end

  calculate_load(working_grid)
end

def main
  input_path = File.join(__dir__, '..', 'input.txt')
  text = File.read(input_path)
  grid = parse_input(text)

  puts "Part 1: #{part1(grid)}"
  puts "Part 2: #{part2(grid)}"
end

main if __FILE__ == $PROGRAM_NAME
