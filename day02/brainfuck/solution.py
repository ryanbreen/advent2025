#!/usr/bin/env python3
"""
Advent of Code 2025 Day 2
Find invalid product IDs
Part 1: pattern repeated EXACTLY twice
Part 2: pattern repeated AT LEAST twice
"""

def is_invalid_id_part1(n):
    """Check if a number is invalid (pattern repeated EXACTLY twice)"""
    s = str(n)
    length = len(s)

    # Must be even length to be repeated exactly twice
    if length % 2 != 0:
        return False

    half = length // 2
    first_half = s[:half]
    second_half = s[half:]

    return first_half == second_half

def is_invalid_id_part2(n):
    """Check if a number is invalid (pattern repeated at least twice)"""
    s = str(n)
    length = len(s)

    # Try all possible pattern lengths (from 1 to length//2)
    # If the string is a pattern repeated at least twice,
    # the pattern length must be at most half the string length
    for pattern_len in range(1, length // 2 + 1):
        # Check if the string length is divisible by the pattern length
        if length % pattern_len == 0:
            pattern = s[:pattern_len]
            # Check if the entire string is this pattern repeated
            if pattern * (length // pattern_len) == s:
                return True

    return False

def solve(input_text):
    """Solve the puzzle"""
    # Parse the ranges
    ranges_str = input_text.strip()
    ranges = []

    for range_str in ranges_str.split(','):
        range_str = range_str.strip()
        if not range_str:
            continue
        start, end = map(int, range_str.split('-'))
        ranges.append((start, end))

    # Find all invalid IDs for both parts
    part1_total = 0
    part2_total = 0

    for start, end in ranges:
        for num in range(start, end + 1):
            if is_invalid_id_part1(num):
                part1_total += num
            if is_invalid_id_part2(num):
                part2_total += num

    return part1_total, part2_total

def main():
    # Read input
    with open('/Users/wrb/fun/code/advent2025/day02/input.txt', 'r') as f:
        input_text = f.read()

    # Solve
    part1, part2 = solve(input_text)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")

if __name__ == '__main__':
    main()
