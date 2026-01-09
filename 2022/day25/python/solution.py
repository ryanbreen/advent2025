#!/usr/bin/env python3
import os

def snafu_to_decimal(s):
    """Convert SNAFU number to decimal."""
    digit_values = {'2': 2, '1': 1, '0': 0, '-': -1, '=': -2}
    result = 0
    for char in s:
        result = result * 5 + digit_values[char]
    return result

def decimal_to_snafu(n):
    """Convert decimal number to SNAFU."""
    if n == 0:
        return '0'

    digits = []
    while n:
        remainder = n % 5
        if remainder <= 2:
            digits.append(str(remainder))
            n //= 5
        elif remainder == 3:
            digits.append('=')
            n = n // 5 + 1
        else:  # remainder == 4
            digits.append('-')
            n = n // 5 + 1

    return ''.join(reversed(digits))

def part1(text):
    """Sum all SNAFU numbers and return result as SNAFU."""
    lines = text.strip().split('\n')
    total = sum(snafu_to_decimal(line) for line in lines)
    return decimal_to_snafu(total)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2: No Part 2 on Day 25!')

if __name__ == '__main__':
    main()
