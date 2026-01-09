#!/usr/bin/env python3
import os

def parse_grid(lines):
    return [[int(c) for c in line] for line in lines]

def is_visible(grid, row, col):
    rows, cols = len(grid), len(grid[0])
    height = grid[row][col]

    # Check from left
    visible_left = all(grid[row][c] < height for c in range(col))
    # Check from right
    visible_right = all(grid[row][c] < height for c in range(col + 1, cols))
    # Check from top
    visible_top = all(grid[r][col] < height for r in range(row))
    # Check from bottom
    visible_bottom = all(grid[r][col] < height for r in range(row + 1, rows))

    return visible_left or visible_right or visible_top or visible_bottom

def scenic_score(grid, row, col):
    rows, cols = len(grid), len(grid[0])
    height = grid[row][col]

    # Count trees visible in each direction
    # Left
    left = 0
    for c in range(col - 1, -1, -1):
        left += 1
        if grid[row][c] >= height:
            break

    # Right
    right = 0
    for c in range(col + 1, cols):
        right += 1
        if grid[row][c] >= height:
            break

    # Up
    up = 0
    for r in range(row - 1, -1, -1):
        up += 1
        if grid[r][col] >= height:
            break

    # Down
    down = 0
    for r in range(row + 1, rows):
        down += 1
        if grid[r][col] >= height:
            break

    return left * right * up * down

def part1(grid):
    rows, cols = len(grid), len(grid[0])
    count = 0
    for r in range(rows):
        for c in range(cols):
            if is_visible(grid, r, c):
                count += 1
    return count

def part2(grid):
    rows, cols = len(grid), len(grid[0])
    max_score = 0
    for r in range(rows):
        for c in range(cols):
            score = scenic_score(grid, r, c)
            max_score = max(max_score, score)
    return max_score

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        lines = f.read().strip().split('\n')

    grid = parse_grid(lines)

    print('Part 1:', part1(grid))
    print('Part 2:', part2(grid))

if __name__ == '__main__':
    main()
