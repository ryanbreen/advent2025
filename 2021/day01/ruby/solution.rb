# Day 1: Sonar Sweep

input_path = File.join(__dir__, '..', 'input.txt')
depths = File.read(input_path).strip.split("\n").map(&:to_i)

def part1(depths)
  # Count the number of times a depth measurement increases from the previous
  count = 0
  (1...depths.length).each do |i|
    count += 1 if depths[i] > depths[i - 1]
  end
  count
end

def part2(depths)
  # Count increases in 3-measurement sliding window sums
  window_sums = (0..depths.length - 3).map do |i|
    depths[i] + depths[i + 1] + depths[i + 2]
  end

  # Count how many times the sum increases
  count = 0
  (1...window_sums.length).each do |i|
    count += 1 if window_sums[i] > window_sums[i - 1]
  end
  count
end

puts "Part 1: #{part1(depths)}"
puts "Part 2: #{part2(depths)}"
