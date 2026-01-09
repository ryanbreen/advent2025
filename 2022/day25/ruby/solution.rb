#!/usr/bin/env ruby

def snafu_to_decimal(s)
  digit_values = { '2' => 2, '1' => 1, '0' => 0, '-' => -1, '=' => -2 }
  result = 0
  s.each_char do |char|
    result = result * 5 + digit_values[char]
  end
  result
end

def decimal_to_snafu(n)
  return '0' if n == 0

  digits = []
  while n != 0
    remainder = n % 5
    if remainder <= 2
      digits << remainder.to_s
      n /= 5
    elsif remainder == 3
      digits << '='
      n = n / 5 + 1
    else # remainder == 4
      digits << '-'
      n = n / 5 + 1
    end
  end

  digits.reverse.join
end

def part1(text)
  lines = text.strip.split("\n")
  total = lines.sum { |line| snafu_to_decimal(line) }
  decimal_to_snafu(total)
end

def main
  script_dir = File.dirname(File.expand_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: No Part 2 on Day 25!"
end

main if __FILE__ == $PROGRAM_NAME
