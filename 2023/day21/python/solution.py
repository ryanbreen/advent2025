#!/usr/bin/env python3
"""Day 21: Step Counter - Garden plot reachability."""

from pathlib import Path
from collections import deque


def parse_input(filename: str) -> tuple[list[str], tuple[int, int]]:
    """Parse grid and find starting position."""
    grid = Path(filename).read_text().strip().split('\n')
    start = None
    for r, row in enumerate(grid):
        for c, ch in enumerate(row):
            if ch == 'S':
                start = (r, c)
                break
        if start:
            break
    return grid, start


def count_reachable(grid: list[str], start: tuple[int, int], steps: int) -> int:
    """Count cells reachable in exactly 'steps' steps."""
    rows, cols = len(grid), len(grid[0])

    # BFS to find minimum steps to each cell
    visited = {}
    queue = deque([(start[0], start[1], 0)])
    visited[start] = 0

    while queue:
        r, c, dist = queue.popleft()

        if dist >= steps:
            continue

        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols:
                if grid[nr][nc] != '#' and (nr, nc) not in visited:
                    visited[(nr, nc)] = dist + 1
                    queue.append((nr, nc, dist + 1))

    # Count cells reachable in exactly 'steps' steps
    # A cell reachable in d steps can be reached in d+2, d+4, ... steps
    # So we need cells where d <= steps and d has same parity as steps
    target_parity = steps % 2
    count = 0
    for (r, c), d in visited.items():
        if d <= steps and d % 2 == target_parity:
            count += 1

    return count


def count_reachable_infinite(grid: list[str], start: tuple[int, int], steps: int) -> int:
    """
    Count cells reachable in exactly 'steps' steps on an infinite tiled grid.

    Uses the quadratic pattern that emerges due to the grid structure:
    - Grid is square (131x131) with S at center (65, 65)
    - Clear paths along edges and through center
    - Pattern repeats every 131 steps

    For large step counts, the reachable area forms a diamond shape.
    The count follows a quadratic pattern: f(n) = an² + bn + c
    where n = (steps - offset) / grid_size
    """
    rows, cols = len(grid), len(grid[0])

    # The grid size (should be square)
    size = rows

    # Steps to reach the edge of the first tile from center
    half = size // 2

    # For the quadratic pattern, we need steps = half + k*size for integer k
    # steps = 26501365 = 65 + 202300 * 131

    if steps <= size * 2:
        # For small step counts, use direct BFS on infinite grid
        return count_reachable_infinite_bfs(grid, start, steps)

    # The number of full grid widths we travel
    n = (steps - half) // size

    # We need to find f(0), f(1), f(2) to determine the quadratic
    # f(n) = an² + bn + c

    # Calculate reachable counts for n=0, 1, 2
    y0 = count_reachable_infinite_bfs(grid, start, half)
    y1 = count_reachable_infinite_bfs(grid, start, half + size)
    y2 = count_reachable_infinite_bfs(grid, start, half + 2 * size)

    # Solve for a, b, c using Lagrange interpolation
    # f(x) = y0 + (y1-y0)*x + (y2-2*y1+y0)*x*(x-1)/2
    a = (y2 - 2 * y1 + y0) // 2
    b = y1 - y0 - a
    c = y0

    return a * n * n + b * n + c


def count_reachable_infinite_bfs(grid: list[str], start: tuple[int, int], steps: int) -> int:
    """BFS on infinite tiled grid for small step counts."""
    rows, cols = len(grid), len(grid[0])

    visited = {}
    queue = deque([(start[0], start[1], 0)])
    visited[(start[0], start[1])] = 0

    while queue:
        r, c, dist = queue.popleft()

        if dist >= steps:
            continue

        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            # Map to grid coordinates (infinite tiling)
            gr, gc = nr % rows, nc % cols
            if grid[gr][gc] != '#' and (nr, nc) not in visited:
                visited[(nr, nc)] = dist + 1
                queue.append((nr, nc, dist + 1))

    target_parity = steps % 2
    count = sum(1 for d in visited.values() if d <= steps and d % 2 == target_parity)
    return count


def part1(grid: list[str], start: tuple[int, int]) -> int:
    """Part 1: Count plots reachable in exactly 64 steps."""
    return count_reachable(grid, start, 64)


def part2(grid: list[str], start: tuple[int, int]) -> int:
    """Part 2: Count plots reachable in exactly 26501365 steps on infinite grid."""
    return count_reachable_infinite(grid, start, 26501365)


def main():
    grid, start = parse_input(Path(__file__).parent.parent / "input.txt")
    print(f"Part 1: {part1(grid, start)}")
    print(f"Part 2: {part2(grid, start)}")


if __name__ == "__main__":
    main()
