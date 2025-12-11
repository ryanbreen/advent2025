#!/usr/bin/env python3
"""
Advent of Code 2024 Day 8: Resonant Collinearity

Finds antinodes created by antenna pairs through geometric collinearity.
Part 1: Specific 2:1 distance ratio points
Part 2: All collinear grid positions
"""
from collections import defaultdict
from itertools import combinations

def parse_input(filename: str) -> tuple[int, int, dict[str, list[tuple[int, int]]]]:
    """Parse grid and group antenna positions by frequency."""
    with open(filename) as f:
        grid = [line.rstrip('\n') for line in f]

    rows = len(grid)
    cols = len(grid[0]) if grid else 0

    # Group antenna positions by frequency
    antennas = defaultdict(list)
    for r, row in enumerate(grid):
        for c, ch in enumerate(row):
            if ch != '.':
                antennas[ch].append((r, c))

    return rows, cols, antennas

def part1() -> int:
    """Count antinodes at 2:1 distance ratio from antenna pairs."""
    rows, cols, antennas = parse_input('../input.txt')
    antinodes = set()

    for positions in antennas.values():
        # For each pair of antennas with same frequency
        for (r1, c1), (r2, c2) in combinations(positions, 2):
            # Calculate the two antinodes (2:1 distance ratio)
            # Antinode beyond antenna 1 (away from antenna 2)
            ar1, ac1 = 2 * r1 - r2, 2 * c1 - c2
            # Antinode beyond antenna 2 (away from antenna 1)
            ar2, ac2 = 2 * r2 - r1, 2 * c2 - c1

            # Add if within bounds
            if 0 <= ar1 < rows and 0 <= ac1 < cols:
                antinodes.add((ar1, ac1))
            if 0 <= ar2 < rows and 0 <= ac2 < cols:
                antinodes.add((ar2, ac2))

    return len(antinodes)

def part2() -> int:
    """Count all grid positions collinear with antenna pairs."""
    rows, cols, antennas = parse_input('../input.txt')
    antinodes = set()

    for positions in antennas.values():
        # For each pair of antennas with same frequency
        for (r1, c1), (r2, c2) in combinations(positions, 2):
            dr, dc = r2 - r1, c2 - c1

            # Extend in both directions along the line
            # Direction 1: from antenna 1 towards and beyond antenna 2
            r, c = r1, c1
            while 0 <= r < rows and 0 <= c < cols:
                antinodes.add((r, c))
                r += dr
                c += dc

            # Direction 2: from antenna 1 away from antenna 2
            # Start at r1-dr to avoid double-counting the antenna positions
            r, c = r1 - dr, c1 - dc
            while 0 <= r < rows and 0 <= c < cols:
                antinodes.add((r, c))
                r -= dr
                c -= dc

    return len(antinodes)

if __name__ == '__main__':
    print('Part 1:', part1())
    print('Part 2:', part2())
