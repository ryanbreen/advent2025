#!/usr/bin/env ruby

# Read input file
input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
lines = File.read(input_path).strip.split("\n")

# Word-to-digit mapping for Part 2
WORDS = {
  'one' => '1', 'two' => '2', 'three' => '3', 'four' => '4', 'five' => '5',
  'six' => '6', 'seven' => '7', 'eight' => '8', 'nine' => '9'
}

def part1(lines)
  lines.sum do |line|
    digits = line.scan(/\d/)
    digits.empty? ? 0 : (digits.first + digits.last).to_i
  end
end

def part2(lines)
  lines.sum do |line|
    digits = []
    line.each_char.with_index do |char, i|
      if char =~ /\d/
        digits << char
      else
        WORDS.each do |word, digit|
          if line[i..].start_with?(word)
            digits << digit
            break
          end
        end
      end
    end
    digits.empty? ? 0 : (digits.first + digits.last).to_i
  end
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
