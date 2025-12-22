#!/usr/bin/env ruby
# Day 22: Monkey Market - Pseudorandom number generation for market prices

def next_secret(secret)
  # Step 1: multiply by 64, mix, prune
  secret ^= (secret << 6)  # * 64 = << 6
  secret &= 0xFFFFFF       # % 16777216 = & (2^24 - 1)

  # Step 2: divide by 32, mix, prune
  secret ^= (secret >> 5)  # // 32 = >> 5
  secret &= 0xFFFFFF

  # Step 3: multiply by 2048, mix, prune
  secret ^= (secret << 11) # * 2048 = << 11
  secret &= 0xFFFFFF

  secret
end

def generate_secrets(initial, count)
  secrets = [initial]
  secret = initial
  count.times do
    secret = next_secret(secret)
    secrets << secret
  end
  secrets
end

def part1(initial_secrets)
  total = 0
  initial_secrets.each do |initial|
    secret = initial
    2000.times do
      secret = next_secret(secret)
    end
    total += secret
  end
  total
end

def part2(initial_secrets)
  # Map from [change1, change2, change3, change4] -> total bananas
  sequence_totals = Hash.new(0)

  initial_secrets.each do |initial|
    # Generate 2001 secrets (initial + 2000 new)
    secrets = generate_secrets(initial, 2000)
    prices = secrets.map { |s| s % 10 }

    # Calculate changes
    changes = (0...prices.length - 1).map { |i| prices[i + 1] - prices[i] }

    # Track first occurrence of each 4-change sequence for this buyer
    seen = Set.new
    (0..changes.length - 4).each do |i|
      seq = [changes[i], changes[i + 1], changes[i + 2], changes[i + 3]]
      unless seen.include?(seq)
        seen.add(seq)
        # Price we get is after these 4 changes
        sequence_totals[seq] += prices[i + 4]
      end
    end
  end

  sequence_totals.values.max
end

# Main execution
require 'set'

input_text = File.read(File.join(__dir__, '..', 'input.txt')).strip
initial_secrets = input_text.split("\n").map(&:to_i)

puts "Part 1: #{part1(initial_secrets)}"
puts "Part 2: #{part2(initial_secrets)}"
