#!/usr/bin/env python3
import os

def parse_input():
    input_path = os.path.join(os.path.dirname(__file__), '..', 'input.txt')
    with open(input_path) as f:
        return [line.strip() for line in f if line.strip()]

def part1(numbers):
    num_bits = len(numbers[0])
    gamma = 0

    for pos in range(num_bits):
        ones = sum(1 for n in numbers if n[pos] == '1')
        zeros = len(numbers) - ones

        if ones >= zeros:
            gamma |= (1 << (num_bits - 1 - pos))

    # epsilon is bitwise NOT of gamma (within num_bits)
    epsilon = gamma ^ ((1 << num_bits) - 1)

    return gamma * epsilon

def find_rating(numbers, use_most_common):
    num_bits = len(numbers[0])
    candidates = numbers[:]

    for pos in range(num_bits):
        if len(candidates) == 1:
            break

        ones = sum(1 for n in candidates if n[pos] == '1')
        zeros = len(candidates) - ones

        if use_most_common:
            target = '1' if ones >= zeros else '0'
        else:
            target = '0' if zeros <= ones else '1'

        candidates = [n for n in candidates if n[pos] == target]

    return int(candidates[0], 2)

def part2(numbers):
    oxygen = find_rating(numbers, use_most_common=True)
    co2 = find_rating(numbers, use_most_common=False)
    return oxygen * co2

if __name__ == '__main__':
    numbers = parse_input()
    print(f"Part 1: {part1(numbers)}")
    print(f"Part 2: {part2(numbers)}")
