#!/usr/bin/env python3
"""Day 21: Keypad Conundrum - Robot chain control with shortest path optimization"""

from functools import lru_cache
from pathlib import Path

# Keypad layouts - positions as (row, col)
NUMERIC = {
    '7': (0, 0), '8': (0, 1), '9': (0, 2),
    '4': (1, 0), '5': (1, 1), '6': (1, 2),
    '1': (2, 0), '2': (2, 1), '3': (2, 2),
    '0': (3, 1), 'A': (3, 2),
}
NUMERIC_GAP = (3, 0)

DIRECTIONAL = {
    '^': (0, 1), 'A': (0, 2),
    '<': (1, 0), 'v': (1, 1), '>': (1, 2),
}
DIRECTIONAL_GAP = (0, 0)


def shortest_paths(keypad, gap, start, end):
    """Find all shortest paths from start to end, avoiding gap."""
    sr, sc = keypad[start]
    er, ec = keypad[end]

    paths = []

    def dfs(r, c, path):
        if (r, c) == gap:
            return
        if (r, c) == (er, ec):
            paths.append(path)
            return
        # Move vertically toward target
        if r < er:
            dfs(r + 1, c, path + 'v')
        elif r > er:
            dfs(r - 1, c, path + '^')
        # Move horizontally toward target
        if c < ec:
            dfs(r, c + 1, path + '>')
        elif c > ec:
            dfs(r, c - 1, path + '<')

    dfs(sr, sc, '')
    return paths if paths else ['']  # Empty path if start == end


@lru_cache(maxsize=None)
def min_presses_for_move(from_char, to_char, depth, is_numeric):
    """Minimum presses needed to move from from_char to to_char and press, at given depth."""
    if is_numeric:
        keypad, gap = NUMERIC, NUMERIC_GAP
    else:
        keypad, gap = DIRECTIONAL, DIRECTIONAL_GAP

    paths = shortest_paths(keypad, gap, from_char, to_char)

    if depth == 0:
        # At human level, just return path length + 1 for 'A' press
        return min(len(p) for p in paths) + 1

    best = float('inf')
    for path in paths:
        # Need to type path + 'A' on the directional keypad above
        sequence = path + 'A'
        cost = 0
        current = 'A'
        for char in sequence:
            cost += min_presses_for_move(current, char, depth - 1, False)
            current = char
        best = min(best, cost)

    return best


def solve_code(code, depth):
    """Compute minimum presses to type code on numeric keypad with given robot depth."""
    total = 0
    current = 'A'
    for char in code:
        total += min_presses_for_move(current, char, depth, True)
        current = char
    return total


def complexity(code, length):
    """Compute complexity: length * numeric part of code."""
    numeric_part = int(code.rstrip('A'))
    return length * numeric_part


def part1(codes):
    """Part 1: 2 intermediate robots (depth = 2)."""
    total = 0
    for code in codes:
        length = solve_code(code, 2)
        total += complexity(code, length)
    return total


def part2(codes):
    """Part 2: 25 intermediate robots (depth = 25)."""
    total = 0
    for code in codes:
        length = solve_code(code, 25)
        total += complexity(code, length)
    return total


def main():
    input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()
    codes = [line.strip() for line in input_text.split('\n') if line.strip()]

    print('Part 1:', part1(codes))
    print('Part 2:', part2(codes))


if __name__ == '__main__':
    main()
