#!/usr/bin/env python3
from collections import deque

def parse_input(filename):
    """Parse byte positions from input file."""
    with open(filename) as f:
        positions = []
        for line in f:
            line = line.strip()
            if line:
                x, y = map(int, line.split(','))
                positions.append((x, y))
        return positions

def bfs(corrupted, size=71):
    """Find shortest path from (0,0) to (size-1, size-1) using BFS."""
    start = (0, 0)
    goal = (size - 1, size - 1)

    if start in corrupted or goal in corrupted:
        return -1

    queue = deque([(start, 0)])
    visited = {start}
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

    while queue:
        (x, y), steps = queue.popleft()

        if (x, y) == goal:
            return steps

        for dx, dy in directions:
            nx, ny = x + dx, y + dy
            if 0 <= nx < size and 0 <= ny < size and (nx, ny) not in visited and (nx, ny) not in corrupted:
                visited.add((nx, ny))
                queue.append(((nx, ny), steps + 1))

    return -1

def part1(positions, num_bytes=1024, size=71):
    """Find shortest path after first num_bytes have fallen."""
    corrupted = set(positions[:num_bytes])
    return bfs(corrupted, size)

def part2(positions, size=71):
    """Find the first byte that blocks all paths."""
    # Binary search to find the first byte that blocks the path
    left, right = 0, len(positions)

    while left < right:
        mid = (left + right) // 2
        corrupted = set(positions[:mid + 1])
        if bfs(corrupted, size) == -1:
            right = mid
        else:
            left = mid + 1

    blocking_pos = positions[left]
    return f"{blocking_pos[0]},{blocking_pos[1]}"

if __name__ == "__main__":
    positions = parse_input("../input.txt")

    print("Part 1:", part1(positions))
    print("Part 2:", part2(positions))
