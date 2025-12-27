# frozen_string_literal: true

# Day 5: If You Give A Seed A Fertilizer
#
# This solution handles the Almanac mapping problem where seeds must be traced
# through a series of category transformations (seed -> soil -> fertilizer ->
# water -> light -> temperature -> humidity -> location).
#
# Part 1: Trace individual seed values through all maps to find minimum location.
# Part 2: Treat seed numbers as ranges and use interval arithmetic to efficiently
#         map entire ranges through the transformation pipeline.

# Represents a mapping rule that transforms values from one category to another.
# Each rule defines a source range and how it maps to a destination range.
#
# @example A rule mapping source [50, 98) to destination [52, 100)
#   MappingRule.new(52, 50, 48)  # dst_start=52, src_start=50, length=48
#
MappingRule = Struct.new(:dst_start, :src_start, :length) do
  # @return [Integer] the exclusive end of the source range
  def src_end
    src_start + length
  end

  # @return [Integer] the offset to add when mapping a value
  def offset
    dst_start - src_start
  end

  # Checks if a value falls within this rule's source range
  #
  # @param value [Integer] the value to check
  # @return [Boolean] true if value is in [src_start, src_end)
  def covers?(value)
    value >= src_start && value < src_end
  end

  # Transforms a value using this rule's mapping
  #
  # @param value [Integer] the value to transform (must be within range)
  # @return [Integer] the mapped destination value
  def apply(value)
    dst_start + (value - src_start)
  end
end

# Parses the almanac input into seeds and transformation maps.
#
# The input format is:
#   seeds: <space-separated numbers>
#
#   <category>-to-<category> map:
#   <dst_start> <src_start> <length>
#   ...
#
# @param text [String] the raw almanac input
# @return [Array<(Array<Integer>, Array<Array<MappingRule>>)>]
#   a tuple of [seeds, maps] where maps is an array of transformation stages
def parse_input(text)
  sections = text.strip.split("\n\n")

  # Parse seeds from "seeds: 79 14 55 13" format
  seeds = sections.first
    .split(': ')
    .last
    .split
    .map(&:to_i)

  # Parse each mapping section into an array of MappingRule objects
  maps = sections.drop(1).map do |section|
    section
      .lines
      .drop(1)  # Skip the "X-to-Y map:" header line
      .map { |line| MappingRule.new(*line.split.map(&:to_i)) }
  end

  [seeds, maps]
end

# Applies a single transformation map to a value.
#
# If the value falls within any rule's source range, it gets transformed.
# Otherwise, the value passes through unchanged (identity mapping).
#
# @param value [Integer] the value to transform
# @param rules [Array<MappingRule>] the mapping rules for this stage
# @return [Integer] the transformed value
def apply_map(value, rules)
  rules.find { |rule| rule.covers?(value) }
    &.apply(value) || value
end

# Traces a seed through all transformation stages to find its location.
#
# Applies each map in sequence: seed -> soil -> fertilizer -> water ->
# light -> temperature -> humidity -> location
#
# @param seed [Integer] the initial seed value
# @param maps [Array<Array<MappingRule>>] all transformation stages
# @return [Integer] the final location value
def seed_to_location(seed, maps)
  maps.reduce(seed) { |value, rules| apply_map(value, rules) }
end

# Solves Part 1: Find the minimum location for any individual seed.
#
# @param seeds [Array<Integer>] the list of seed values
# @param maps [Array<Array<MappingRule>>] all transformation stages
# @return [Integer] the minimum location value
def part1(seeds, maps)
  seeds.map { |seed| seed_to_location(seed, maps) }.min
end

# Applies transformation rules to a set of ranges using interval arithmetic.
#
# For each input range and each mapping rule, we compute:
# 1. The portion BEFORE the rule's range (unmapped, try next rule)
# 2. The portion WITHIN the rule's range (mapped, goes to result)
# 3. The portion AFTER the rule's range (unmapped, try next rule)
#
# This interval splitting approach handles the complexity of ranges that
# may overlap with zero, one, or multiple mapping rules.
#
# @param input_ranges [Array<Array<(Integer, Integer)>>] ranges as [start, end) pairs
# @param rules [Array<MappingRule>] the mapping rules for this stage
# @return [Array<Array<(Integer, Integer)>>] transformed ranges
def apply_map_to_ranges(input_ranges, rules)
  input_ranges.flat_map do |range_start, range_end|
    # Track portions of this range that haven't been mapped yet
    remaining = [[range_start, range_end]]
    mapped = []

    rules.each do |rule|
      remaining = remaining.flat_map do |r_start, r_end|
        unmapped = []

        # Portion before the rule's source range (stays unmapped for now)
        unmapped << [r_start, [r_end, rule.src_start].min] if r_start < rule.src_start

        # Portion within the rule's source range (apply the mapping)
        overlap_start = [r_start, rule.src_start].max
        overlap_end = [r_end, rule.src_end].min
        if overlap_start < overlap_end
          mapped << [overlap_start + rule.offset, overlap_end + rule.offset]
        end

        # Portion after the rule's source range (stays unmapped for now)
        unmapped << [[r_start, rule.src_end].max, r_end] if r_end > rule.src_end

        unmapped
      end
    end

    # Any remaining portions pass through unchanged (identity mapping)
    mapped + remaining
  end
end

# Solves Part 2: Find the minimum location treating seeds as range pairs.
#
# The seeds list is interpreted as pairs: [start, length, start, length, ...]
# Each pair defines a range of seed values. We track these ranges through
# all transformation stages using interval arithmetic.
#
# @param seeds [Array<Integer>] seed values as [start, length] pairs
# @param maps [Array<Array<MappingRule>>] all transformation stages
# @return [Integer] the minimum location value from any seed range
def part2(seeds, maps)
  # Convert seed pairs to [start, end) ranges
  initial_ranges = seeds
    .each_slice(2)
    .map { |start, length| [start, start + length] }

  # Thread ranges through all transformation stages
  final_ranges = maps.reduce(initial_ranges) do |ranges, rules|
    apply_map_to_ranges(ranges, rules)
  end

  # The answer is the minimum starting point of any output range
  final_ranges.map(&:first).min
end

# Main entry point
def main
  input_path = File.join(__dir__, '..', 'input.txt')
  seeds, maps = File.read(input_path).then { |text| parse_input(text) }

  puts "Part 1: #{part1(seeds, maps)}"
  puts "Part 2: #{part2(seeds, maps)}"
end

main
