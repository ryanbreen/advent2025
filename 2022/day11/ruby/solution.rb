#!/usr/bin/env ruby
# frozen_string_literal: true

def parse_monkeys(text)
  text.strip.split("\n\n").map do |block|
    lines = block.strip.split("\n")
    items = lines[1].scan(/\d+/).map(&:to_i)

    # Parse operation
    op_match = lines[2].match(/new = old ([+*]) (\w+)/)
    operator = op_match[1]
    operand = op_match[2]

    divisor = lines[3].match(/\d+/)[0].to_i
    if_true = lines[4].match(/\d+/)[0].to_i
    if_false = lines[5].match(/\d+/)[0].to_i

    {
      items: items,
      operator: operator,
      operand: operand,
      divisor: divisor,
      if_true: if_true,
      if_false: if_false,
      inspections: 0
    }
  end
end

def apply_operation(old, operator, operand)
  val = operand == 'old' ? old : operand.to_i
  case operator
  when '+'
    old + val
  when '*'
    old * val
  end
end

def simulate(monkeys, rounds, relief_divisor: 3, use_modulo: false)
  # For part 2, use product of all divisors to keep numbers manageable
  mod_value = use_modulo ? monkeys.map { |m| m[:divisor] }.reduce(:*) : nil

  rounds.times do
    monkeys.each do |monkey|
      while monkey[:items].any?
        item = monkey[:items].shift
        monkey[:inspections] += 1

        # Apply operation
        new_val = apply_operation(item, monkey[:operator], monkey[:operand])

        # Apply relief
        new_val /= relief_divisor if relief_divisor > 1

        # Apply modulo to prevent overflow
        new_val %= mod_value if mod_value

        # Test and throw
        target = (new_val % monkey[:divisor]).zero? ? monkey[:if_true] : monkey[:if_false]
        monkeys[target][:items] << new_val
      end
    end
  end

  monkeys
end

def monkey_business(monkeys)
  inspections = monkeys.map { |m| m[:inspections] }.sort.reverse
  inspections[0] * inspections[1]
end

def part1(text)
  monkeys = parse_monkeys(text)
  simulate(monkeys, 20, relief_divisor: 3)
  monkey_business(monkeys)
end

def part2(text)
  monkeys = parse_monkeys(text)
  simulate(monkeys, 10_000, relief_divisor: 1, use_modulo: true)
  monkey_business(monkeys)
end

def main
  script_dir = File.dirname(File.absolute_path(__FILE__))
  input_file = File.join(script_dir, '..', 'input.txt')

  text = File.read(input_file)

  puts "Part 1: #{part1(text)}"
  puts "Part 2: #{part2(text)}"
end

main if __FILE__ == $PROGRAM_NAME
