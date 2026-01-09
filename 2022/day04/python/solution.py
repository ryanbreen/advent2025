#!/usr/bin/env python3
import os

def parse_input(filename):
    pairs = []
    with open(filename) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            left, right = line.split(',')
            a1, b1 = map(int, left.split('-'))
            a2, b2 = map(int, right.split('-'))
            pairs.append((a1, b1, a2, b2))
    return pairs

def fully_contains(a1, b1, a2, b2):
    """Check if one range fully contains the other."""
    return (a1 <= a2 and b1 >= b2) or (a2 <= a1 and b2 >= b1)

def overlaps(a1, b1, a2, b2):
    """Check if ranges overlap at all."""
    return a1 <= b2 and a2 <= b1

def part1(pairs):
    return sum(1 for a1, b1, a2, b2 in pairs if fully_contains(a1, b1, a2, b2))

def part2(pairs):
    return sum(1 for a1, b1, a2, b2 in pairs if overlaps(a1, b1, a2, b2))

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    pairs = parse_input(input_file)

    print('Part 1:', part1(pairs))
    print('Part 2:', part2(pairs))

if __name__ == '__main__':
    main()
