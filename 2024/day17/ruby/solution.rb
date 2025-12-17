#!/usr/bin/env ruby
# Day 17: Chronospatial Computer - 3-bit VM emulator

def parse_input(text)
  lines = text.strip.split("\n")
  a = lines[0].match(/Register A: (\d+)/)[1].to_i
  b = lines[1].match(/Register B: (\d+)/)[1].to_i
  c = lines[2].match(/Register C: (\d+)/)[1].to_i
  program = lines[4].match(/Program: ([\d,]+)/)[1].split(',').map(&:to_i)
  [a, b, c, program]
end

def run_program(a, b, c, program)
  ip = 0
  output = []

  combo = lambda do |operand|
    case operand
    when 0..3 then operand
    when 4 then a
    when 5 then b
    when 6 then c
    else raise "Invalid combo operand: #{operand}"
    end
  end

  while ip < program.length
    opcode = program[ip]
    operand = program[ip + 1]

    case opcode
    when 0  # adv - A = A >> combo
      a = a >> combo.call(operand)
    when 1  # bxl - B = B XOR literal
      b = b ^ operand
    when 2  # bst - B = combo % 8
      b = combo.call(operand) & 7
    when 3  # jnz - jump if A != 0
      if a != 0
        ip = operand
        next
      end
    when 4  # bxc - B = B XOR C
      b = b ^ c
    when 5  # out - output combo % 8
      output << (combo.call(operand) & 7)
    when 6  # bdv - B = A >> combo
      b = a >> combo.call(operand)
    when 7  # cdv - C = A >> combo
      c = a >> combo.call(operand)
    end

    ip += 2
  end

  output
end

def part1(text)
  a, b, c, program = parse_input(text)
  output = run_program(a, b, c, program)
  output.join(',')
end

def part2(text)
  _, b, c, program = parse_input(text)

  # The program loops, outputting one digit per iteration, dividing A by 8 each time.
  # We need to find A such that output == program.
  # Work backwards from the last digit - build A 3 bits at a time.

  search = lambda do |target_idx, current_a|
    return current_a if target_idx < 0

    # Try all 8 possible 3-bit values for this position
    (0..7).each do |bits|
      candidate_a = (current_a << 3) | bits
      # A can't be 0 at start (would halt immediately without output)
      next if candidate_a == 0 && target_idx == program.length - 1

      output = run_program(candidate_a, b, c, program)

      # Check if output matches the suffix of the program
      expected = program[target_idx..-1]
      if output == expected
        result = search.call(target_idx - 1, candidate_a)
        return result unless result.nil?
      end
    end

    nil
  end

  search.call(program.length - 1, 0)
end

if __FILE__ == $0
  input_path = File.join(File.dirname(__FILE__), '..', 'input.txt')
  text = File.read(input_path)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end
