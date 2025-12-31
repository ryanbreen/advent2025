#!/usr/bin/env python3
"""Day 17: Clumsy Crucible - Dijkstra's shortest path with movement constraints."""

import heapq
from pathlib import Path


def parse_input(filename: str) -> list[list[int]]:
    """Parse the grid of heat loss values."""
    text = Path(filename).read_text().strip()
    return [[int(c) for c in line] for line in text.split('\n')]


def dijkstra(grid: list[list[int]], min_straight: int, max_straight: int) -> int:
    """
    Find minimum heat loss path using Dijkstra's algorithm.

    State: (row, col, direction, consecutive_steps)
    Directions: 0=right, 1=down, 2=left, 3=up
    """
    rows, cols = len(grid), len(grid[0])
    dr = [0, 1, 0, -1]
    dc = [1, 0, -1, 0]

    # Priority queue: (heat_loss, row, col, direction, consecutive)
    # Start with both right and down directions
    pq = [(0, 0, 0, -1, 0)]  # -1 direction means no direction yet
    visited = set()

    while pq:
        heat, r, c, d, consec = heapq.heappop(pq)

        # Check if we reached the goal
        if r == rows - 1 and c == cols - 1:
            if min_straight == 0 or consec >= min_straight:
                return heat

        state = (r, c, d, consec)
        if state in visited:
            continue
        visited.add(state)

        # Try all four directions
        for nd in range(4):
            # Can't reverse direction
            if d != -1 and nd == (d + 2) % 4:
                continue

            nr, nc = r + dr[nd], c + dc[nd]

            # Bounds check
            if nr < 0 or nr >= rows or nc < 0 or nc >= cols:
                continue

            # Check consecutive constraints
            if nd == d:
                # Continuing in same direction
                new_consec = consec + 1
                if new_consec > max_straight:
                    continue
            else:
                # Turning - must have gone min_straight in previous direction first
                if d != -1 and consec < min_straight:
                    continue
                new_consec = 1

            new_heat = heat + grid[nr][nc]
            new_state = (nr, nc, nd, new_consec)

            if new_state not in visited:
                heapq.heappush(pq, (new_heat, nr, nc, nd, new_consec))

    return -1  # No path found


def part1(grid: list[list[int]]) -> int:
    """Part 1: Normal crucible, max 3 consecutive blocks."""
    return dijkstra(grid, 0, 3)


def part2(grid: list[list[int]]) -> int:
    """Part 2: Ultra crucible, min 4 and max 10 consecutive blocks."""
    return dijkstra(grid, 4, 10)


def main():
    grid = parse_input(Path(__file__).parent.parent / "input.txt")
    print(f"Part 1: {part1(grid)}")
    print(f"Part 2: {part2(grid)}")


if __name__ == "__main__":
    main()
