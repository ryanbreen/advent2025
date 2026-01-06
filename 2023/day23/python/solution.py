#!/usr/bin/env python3
"""Day 23: A Long Walk - Longest path through hiking trails."""

import sys
from pathlib import Path
from collections import defaultdict

# Increase recursion limit for deep DFS
sys.setrecursionlimit(100000)

DIRECTIONS = ((-1, 0), (1, 0), (0, -1), (0, 1))
SLOPE_DIRS = {'^': (-1, 0), 'v': (1, 0), '<': (0, -1), '>': (0, 1)}


def parse_input(filename: str) -> list[str]:
    """Parse the grid from input."""
    return Path(filename).read_text().strip().split('\n')


def find_junctions(grid: list[str]) -> set[tuple[int, int]]:
    """Find all junction points (start, end, and intersections)."""
    rows, cols = len(grid), len(grid[0])

    # Start and end points
    start = (0, grid[0].index('.'))
    end = (rows - 1, grid[rows - 1].index('.'))
    junctions = {start, end}

    # Find intersections (cells with 3+ walkable neighbors)
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '#':
                continue
            neighbors = sum(
                1 for dr, dc in DIRECTIONS
                if 0 <= r + dr < rows and 0 <= c + dc < cols
                and grid[r + dr][c + dc] != '#'
            )
            if neighbors >= 3:
                junctions.add((r, c))

    return junctions


def build_graph(grid: list[str], junctions: set[tuple[int, int]], respect_slopes: bool) -> dict:
    """Build a graph of junctions with edge weights (distances)."""
    rows, cols = len(grid), len(grid[0])
    graph = defaultdict(dict)

    for start_junction in junctions:
        # DFS from each junction to find reachable junctions
        stack = [(start_junction, 0)]
        visited = {start_junction}

        while stack:
            (r, c), dist = stack.pop()

            if dist > 0 and (r, c) in junctions:
                graph[start_junction][(r, c)] = dist
                continue

            for dr, dc in DIRECTIONS:
                nr, nc = r + dr, c + dc
                if not (0 <= nr < rows and 0 <= nc < cols):
                    continue
                if grid[nr][nc] == '#' or (nr, nc) in visited:
                    continue

                # Check slope constraints for Part 1
                if respect_slopes:
                    cell = grid[r][c]
                    if cell in SLOPE_DIRS and (dr, dc) != SLOPE_DIRS[cell]:
                        continue

                visited.add((nr, nc))
                stack.append(((nr, nc), dist + 1))

    return graph


def longest_path_dfs(graph: dict, start: tuple[int, int], end: tuple[int, int]) -> int:
    """Find longest path using recursive DFS with aggressive optimization."""
    # Build node index mapping for bitmasking
    all_nodes = set(graph.keys())
    for neighbors in graph.values():
        all_nodes.update(neighbors.keys())

    nodes = sorted(all_nodes)
    node_to_idx = {node: idx for idx, node in enumerate(nodes)}
    start_idx = node_to_idx[start]
    end_idx = node_to_idx[end]

    # Pre-compute bit masks for each node
    num_nodes = len(nodes)
    bit_masks = [1 << i for i in range(num_nodes)]

    # Convert graph to list of lists for maximum speed
    # Each entry is a list of (neighbor_idx, distance, bit_mask) tuples
    indexed_graph = []
    for node in nodes:
        if node in graph:
            edges = [(node_to_idx[neighbor], dist, bit_masks[node_to_idx[neighbor]])
                     for neighbor, dist in graph[node].items()]
        else:
            edges = []
        indexed_graph.append(edges)

    # Use nonlocal variable (faster than list index)
    max_dist = 0

    # Inline the critical path - avoid any unnecessary operations
    def dfs(node_idx, visited_mask, current_dist):
        nonlocal max_dist
        if node_idx == end_idx:
            if current_dist > max_dist:
                max_dist = current_dist
            return

        # Use pre-computed bit masks
        for neighbor_idx, dist, bit_mask in indexed_graph[node_idx]:
            if not (visited_mask & bit_mask):
                dfs(neighbor_idx, visited_mask | bit_mask, current_dist + dist)

    # Start DFS
    dfs(start_idx, bit_masks[start_idx], 0)
    return max_dist


def solve(grid: list[str], respect_slopes: bool) -> int:
    """Solve for either part."""
    rows = len(grid)
    start = (0, grid[0].index('.'))
    end = (rows - 1, grid[rows - 1].index('.'))

    junctions = find_junctions(grid)
    graph = build_graph(grid, junctions, respect_slopes)

    return longest_path_dfs(graph, start, end)


def part1(grid: list[str]) -> int:
    """Part 1: Respect slope directions."""
    return solve(grid, respect_slopes=True)


def part2(grid: list[str]) -> int:
    """Part 2: Ignore slopes (treat as regular paths)."""
    return solve(grid, respect_slopes=False)


def main():
    input_path = Path(__file__).parent.parent / "input.txt"
    grid = parse_input(input_path)
    print(f"Part 1: {part1(grid)}")
    print(f"Part 2: {part2(grid)}")


if __name__ == "__main__":
    main()
