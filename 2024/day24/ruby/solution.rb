#!/usr/bin/env ruby

require 'set'

def parse_input(filename)
  content = File.read(filename)
  parts = content.strip.split("\n\n")

  # Parse initial wire values
  wires = {}
  parts[0].split("\n").each do |line|
    name, val = line.split(': ')
    wires[name] = val.to_i
  end

  # Parse gates
  gates = []
  parts[1].split("\n").each do |line|
    # Format: "x00 AND y00 -> z00"
    parts_line = line.split(' ')
    in1 = parts_line[0]
    op = parts_line[1]
    in2 = parts_line[2]
    out = parts_line[4]
    gates << [in1, op, in2, out]
  end

  [wires, gates]
end

def simulate(wires, gates)
  wires = wires.dup
  remaining = gates.dup

  until remaining.empty?
    made_progress = false
    new_remaining = []

    remaining.each do |in1, op, in2, out|
      if wires.key?(in1) && wires.key?(in2)
        v1 = wires[in1]
        v2 = wires[in2]

        wires[out] = case op
        when 'AND'
          v1 & v2
        when 'OR'
          v1 | v2
        when 'XOR'
          v1 ^ v2
        end

        made_progress = true
      else
        new_remaining << [in1, op, in2, out]
      end
    end

    remaining = new_remaining

    if !made_progress && !remaining.empty?
      raise "Circuit stuck - missing inputs"
    end
  end

  wires
end

def get_z_value(wires)
  z_wires = wires.keys.select { |k| k.start_with?('z') }.sort.reverse
  result = 0
  z_wires.each do |z|
    result = (result << 1) | wires[z]
  end
  result
end

def part1(wires, gates)
  final_wires = simulate(wires, gates)
  get_z_value(final_wires)
end

def part2(gates)
  swapped = Set.new

  # Build lookup: output -> (in1, op, in2)
  gate_by_output = {}
  gates.each do |in1, op, in2, out|
    gate_by_output[out] = [in1, op, in2]
  end

  # Build lookup: (inputs_set, op) -> output
  gate_by_inputs_op = {}
  gates.each do |in1, op, in2, out|
    key = [Set.new([in1, in2]), op]
    gate_by_inputs_op[key] = out
  end

  # Find the highest bit number
  max_bit = gates.select { |g| g[3].start_with?('z') }.map { |g| g[3][1..-1].to_i }.max

  # Rules for a correct adder:
  # 1. XOR gates should output to z (except first-level XOR of x,y)
  # 2. z outputs (except last) must come from XOR gates
  # 3. AND gates should not output directly to z (except possibly carry-out)
  # 4. OR gates should not output directly to z (except carry-out to z45)

  gates.each do |in1, op, in2, out|
    # Rule: XOR gates that don't take x,y as input should output to z
    if op == 'XOR'
      is_xy_xor = (in1.start_with?('x') || in1.start_with?('y')) &&
                  (in2.start_with?('x') || in2.start_with?('y'))

      if !is_xy_xor
        # This is a second-level XOR (sum XOR carry), should output to z
        if !out.start_with?('z')
          swapped.add(out)
        end
      end
    end

    # Rule: z outputs (except z{max_bit}) should come from XOR
    if out.start_with?('z') && out != sprintf('z%02d', max_bit)
      if op != 'XOR'
        swapped.add(out)
      end
    end

    # Rule: AND gates (except x00 AND y00) should feed into OR
    if op == 'AND'
      is_first_bit = [in1, in2].include?('x00') && [in1, in2].include?('y00')

      if !is_first_bit
        # This AND output should be input to an OR gate
        used_by_or = gates.any? do |in1b, op2, in2b, out2|
          op2 == 'OR' && [in1b, in2b].include?(out)
        end

        if !used_by_or
          swapped.add(out)
        end
      end
    end

    # Rule: XOR of x,y should feed into another XOR (for z output) or AND (for carry)
    if op == 'XOR'
      is_xy_xor = (in1.start_with?('x') || in1.start_with?('y')) &&
                  (in2.start_with?('x') || in2.start_with?('y'))
      # Skip z00 which is x00 XOR y00 directly
      is_z00 = [in1, in2].include?('x00') && [in1, in2].include?('y00')

      if is_xy_xor && !is_z00
        # Should be used by XOR and AND
        used_by_xor = false
        used_by_and = false

        gates.each do |in1b, op2, in2b, out2|
          if [in1b, in2b].include?(out)
            used_by_xor = true if op2 == 'XOR'
            used_by_and = true if op2 == 'AND'
          end
        end

        if !(used_by_xor && used_by_and)
          swapped.add(out)
        end
      end
    end
  end

  swapped.to_a.sort.join(',')
end

def main
  wires, gates = parse_input('../input.txt')

  puts "Part 1: #{part1(wires, gates)}"
  puts "Part 2: #{part2(gates)}"
end

if __FILE__ == $0
  main
end
