#!/usr/bin/env python3
import os

def find_marker(data, window_size):
    """Find first position where last window_size characters are all unique."""
    for i in range(window_size, len(data) + 1):
        window = data[i - window_size:i]
        if len(set(window)) == window_size:
            return i
    return -1

def part1(data):
    return find_marker(data, 4)

def part2(data):
    return find_marker(data, 14)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        data = f.read().strip()

    print('Part 1:', part1(data))
    print('Part 2:', part2(data))

if __name__ == '__main__':
    main()
