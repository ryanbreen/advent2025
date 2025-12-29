# frozen_string_literal: true

def parse_input(text)
  text.split("\n").map { |line| line.split.map(&:to_i) }
end

def differences(seq)
  seq.each_cons(2).map { |a, b| b - a }
end

def extrapolate_next(seq)
  return 0 if seq.all?(&:zero?)

  seq.last + extrapolate_next(differences(seq))
end

def part1(histories)
  histories.sum { |h| extrapolate_next(h) }
end

def part2(histories)
  histories.sum { |h| extrapolate_next(h.reverse) }
end

input_path = File.join(__dir__, '..', 'input.txt')
histories = parse_input(File.read(input_path).strip)

puts "Part 1: #{part1(histories)}"
puts "Part 2: #{part2(histories)}"
