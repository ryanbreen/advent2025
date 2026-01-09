#!/usr/bin/env python3
import os
from collections import deque

def parse_grid(text):
    """Parse grid and find start/end positions."""
    grid = [list(line) for line in text.strip().split('\n')]
    start = end = None

    for r, row in enumerate(grid):
        for c, ch in enumerate(row):
            if ch == 'S':
                start = (r, c)
                grid[r][c] = 'a'
            elif ch == 'E':
                end = (r, c)
                grid[r][c] = 'z'

    return grid, start, end

def bfs(grid, starts, end):
    """BFS to find shortest path from any start to end."""
    rows, cols = len(grid), len(grid[0])
    visited = set()
    queue = deque()

    for start in starts:
        queue.append((start[0], start[1], 0))
        visited.add(start)

    while queue:
        r, c, dist = queue.popleft()

        if (r, c) == end:
            return dist

        current_height = ord(grid[r][c])

        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc

            if 0 <= nr < rows and 0 <= nc < cols and (nr, nc) not in visited:
                next_height = ord(grid[nr][nc])
                # Can move if destination is at most 1 higher
                if next_height <= current_height + 1:
                    visited.add((nr, nc))
                    queue.append((nr, nc, dist + 1))

    return -1  # No path found

def part1(text):
    """Find shortest path from S to E."""
    grid, start, end = parse_grid(text)
    return bfs(grid, [start], end)

def part2(text):
    """Find shortest path from any 'a' to E."""
    grid, _, end = parse_grid(text)

    # Find all cells with elevation 'a'
    starts = []
    for r, row in enumerate(grid):
        for c, ch in enumerate(row):
            if ch == 'a':
                starts.append((r, c))

    return bfs(grid, starts, end)

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
