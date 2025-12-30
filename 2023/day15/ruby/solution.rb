#!/usr/bin/env ruby
# frozen_string_literal: true

# HASH algorithm: for each char, current = ((current + ASCII) * 17) % 256
def hash_algorithm(str)
  str.each_byte.reduce(0) { |current, byte| (current + byte) * 17 % 256 }
end

# Part 1: Sum of HASH values for all comma-separated steps
def part1(steps)
  steps.sum { |step| hash_algorithm(step) }
end

# Part 2: HASHMAP procedure with 256 boxes containing ordered lens lists
def part2(steps)
  boxes = Array.new(256) { [] }

  steps.each do |step|
    if step.include?('=')
      label, focal = step.split('=')
      focal = focal.to_i
      box_num = hash_algorithm(label)

      # Replace existing lens or add new one
      existing_index = boxes[box_num].index { |l, _| l == label }
      if existing_index
        boxes[box_num][existing_index] = [label, focal]
      else
        boxes[box_num] << [label, focal]
      end
    else
      # Remove lens (step ends with '-')
      label = step.chop
      box_num = hash_algorithm(label)
      boxes[box_num].reject! { |l, _| l == label }
    end
  end

  # Calculate focusing power: (box+1) * slot * focal
  boxes.each_with_index.sum do |box, box_num|
    box.each_with_index.sum do |(_, focal), slot|
      (box_num + 1) * (slot + 1) * focal
    end
  end
end

def main
  input_path = File.join(__dir__, '..', 'input.txt')
  text = File.read(input_path).strip.gsub("\n", '')
  steps = text.split(',')

  puts "Part 1: #{part1(steps)}"
  puts "Part 2: #{part2(steps)}"
end

main if __FILE__ == $PROGRAM_NAME
