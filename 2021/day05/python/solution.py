#!/usr/bin/env python3
import os
from collections import defaultdict

def parse_input():
    input_path = os.path.join(os.path.dirname(__file__), '..', 'input.txt')
    with open(input_path) as f:
        lines = []
        for line in f:
            line = line.strip()
            if not line:
                continue
            parts = line.split(' -> ')
            x1, y1 = map(int, parts[0].split(','))
            x2, y2 = map(int, parts[1].split(','))
            lines.append((x1, y1, x2, y2))
        return lines

def sign(x):
    if x > 0:
        return 1
    elif x < 0:
        return -1
    return 0

def count_overlaps(lines, include_diagonals=False):
    grid = defaultdict(int)

    for x1, y1, x2, y2 in lines:
        dx = sign(x2 - x1)
        dy = sign(y2 - y1)

        # Skip diagonals in part 1
        if not include_diagonals and dx != 0 and dy != 0:
            continue

        x, y = x1, y1
        while True:
            grid[(x, y)] += 1
            if x == x2 and y == y2:
                break
            x += dx
            y += dy

    return sum(1 for v in grid.values() if v >= 2)

def part1(lines):
    return count_overlaps(lines, include_diagonals=False)

def part2(lines):
    return count_overlaps(lines, include_diagonals=True)

if __name__ == '__main__':
    lines = parse_input()
    print(f"Part 1: {part1(lines)}")
    print(f"Part 2: {part2(lines)}")
