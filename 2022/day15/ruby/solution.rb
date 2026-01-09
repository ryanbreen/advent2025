#!/usr/bin/env ruby

def parse_sensors(text)
  pattern = /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/

  text.strip.lines.map do |line|
    match = line.match(pattern)
    sx, sy, bx, by = match.captures.map(&:to_i)
    dist = (sx - bx).abs + (sy - by).abs
    { sx: sx, sy: sy, bx: bx, by: by, dist: dist }
  end
end

def merge_ranges(ranges)
  return [] if ranges.empty?

  sorted = ranges.sort_by(&:first)
  merged = [sorted.first.dup]

  sorted[1..].each do |start_pos, end_pos|
    if start_pos <= merged.last[1] + 1
      merged.last[1] = [merged.last[1], end_pos].max
    else
      merged << [start_pos, end_pos]
    end
  end

  merged
end

def get_coverage_at_row(sensors, row)
  ranges = []

  sensors.each do |sensor|
    row_dist = (sensor[:sy] - row).abs
    next if row_dist > sensor[:dist]

    x_spread = sensor[:dist] - row_dist
    ranges << [sensor[:sx] - x_spread, sensor[:sx] + x_spread]
  end

  merge_ranges(ranges)
end

def part1(text)
  sensors = parse_sensors(text)
  target_row = 2_000_000

  ranges = get_coverage_at_row(sensors, target_row)

  # Count total coverage
  total = ranges.sum { |start_pos, end_pos| end_pos - start_pos + 1 }

  # Subtract beacons that are on this row
  beacons_on_row = sensors
    .select { |s| s[:by] == target_row }
    .map { |s| s[:bx] }
    .uniq

  total - beacons_on_row.size
end

def part2(text)
  sensors = parse_sensors(text)
  max_coord = 4_000_000

  (0..max_coord).each do |row|
    ranges = get_coverage_at_row(sensors, row)

    # Clip ranges to search area
    clipped = ranges.map do |start_pos, end_pos|
      next nil if end_pos < 0 || start_pos > max_coord
      [[0, start_pos].max, [max_coord, end_pos].min]
    end.compact

    clipped = merge_ranges(clipped)

    # Check if full row is covered
    next if clipped.size == 1 && clipped[0][0] == 0 && clipped[0][1] == max_coord

    # Found a gap - the beacon is in the gap
    x = if clipped.size > 1
          clipped[0][1] + 1
        elsif clipped[0][0] > 0
          0
        else
          clipped[0][1] + 1
        end

    return x * 4_000_000 + row
  end

  nil
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')
  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
