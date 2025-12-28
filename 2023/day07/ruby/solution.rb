# frozen_string_literal: true

# Day 7: Camel Cards

input_path = File.join(__dir__, '..', 'input.txt')
lines = File.read(input_path).strip.split("\n")

# Card strength order (higher index = stronger)
CARD_STRENGTH = '23456789TJQKA'
CARD_STRENGTH_JOKER = 'J23456789TQKA' # J is weakest in Part 2

def count_chars(chars)
  # Ruby 2.6 compatible tally alternative
  chars.each_with_object(Hash.new(0)) { |c, h| h[c] += 1 }
end

def get_hand_type(hand)
  # Return hand type as integer (higher = stronger)
  counts = count_chars(hand.chars).values.sort.reverse
  case counts
  when [5]          then 6  # Five of a kind
  when [4, 1]       then 5  # Four of a kind
  when [3, 2]       then 4  # Full house
  when [3, 1, 1]    then 3  # Three of a kind
  when [2, 2, 1]    then 2  # Two pair
  when [2, 1, 1, 1] then 1  # One pair
  else                   0  # High card
  end
end

def hand_key(hand)
  # Return sort key for a hand (type, then card strengths)
  hand_type = get_hand_type(hand)
  card_values = hand.chars.map { |c| CARD_STRENGTH.index(c) }
  [hand_type, card_values]
end

def get_hand_type_with_jokers(hand)
  # Return hand type with J as wildcards (higher = stronger)
  joker_count = hand.count('J')
  return get_hand_type(hand) if joker_count.zero?
  return 6 if joker_count == 5 # Five of a kind

  # Count non-joker cards
  non_jokers = hand.chars.reject { |c| c == 'J' }
  counts = count_chars(non_jokers).values.sort.reverse

  # Add jokers to the highest count
  counts[0] += joker_count

  case counts
  when [5]          then 6  # Five of a kind
  when [4, 1]       then 5  # Four of a kind
  when [3, 2]       then 4  # Full house
  when [3, 1, 1]    then 3  # Three of a kind
  when [2, 2, 1]    then 2  # Two pair
  when [2, 1, 1, 1] then 1  # One pair
  else                   0  # High card
  end
end

def hand_key_with_jokers(hand)
  # Return sort key for a hand with joker rules
  hand_type = get_hand_type_with_jokers(hand)
  card_values = hand.chars.map { |c| CARD_STRENGTH_JOKER.index(c) }
  [hand_type, card_values]
end

def part1(lines)
  hands = lines.map do |line|
    parts = line.split
    [parts[0], parts[1].to_i]
  end

  # Sort by hand strength
  sorted_hands = hands.sort_by { |hand, _bid| hand_key(hand) }

  # Calculate total winnings
  sorted_hands.each_with_index.sum { |(_, bid), index| (index + 1) * bid }
end

def part2(lines)
  hands = lines.map do |line|
    parts = line.split
    [parts[0], parts[1].to_i]
  end

  # Sort by hand strength with joker rules
  sorted_hands = hands.sort_by { |hand, _bid| hand_key_with_jokers(hand) }

  # Calculate total winnings
  sorted_hands.each_with_index.sum { |(_, bid), index| (index + 1) * bid }
end

puts "Part 1: #{part1(lines)}"
puts "Part 2: #{part2(lines)}"
