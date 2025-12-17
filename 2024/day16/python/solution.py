#!/usr/bin/env python3
"""Day 16: Reindeer Maze - Weighted shortest path with turn costs"""

import heapq
from pathlib import Path


def parse_input(text):
    """Parse the maze and find start/end positions."""
    grid = [list(line) for line in text.strip().split('\n')]
    start = end = None

    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == 'S':
                start = (x, y)
            elif cell == 'E':
                end = (x, y)

    return grid, start, end


# Directions: 0=East, 1=South, 2=West, 3=North
DX = [1, 0, -1, 0]
DY = [0, 1, 0, -1]


def dijkstra_forward(grid, start):
    """
    Run Dijkstra from start facing East.
    Returns dict of (x, y, dir) -> minimum cost to reach that state.
    """
    pq = [(0, start[0], start[1], 0)]  # Start facing East
    dist = {}

    while pq:
        cost, x, y, d = heapq.heappop(pq)

        state = (x, y, d)
        if state in dist:
            continue
        dist[state] = cost

        # Move forward
        nx, ny = x + DX[d], y + DY[d]
        if 0 <= ny < len(grid) and 0 <= nx < len(grid[0]) and grid[ny][nx] != '#':
            heapq.heappush(pq, (cost + 1, nx, ny, d))

        # Turn left/right
        heapq.heappush(pq, (cost + 1000, x, y, (d - 1) % 4))
        heapq.heappush(pq, (cost + 1000, x, y, (d + 1) % 4))

    return dist


def dijkstra_backward(grid, end):
    """
    Run Dijkstra backward from end (all directions at end have cost 0).
    Returns dict of (x, y, dir) -> minimum cost from that state to reach end.
    """
    # At end, we can arrive facing any direction
    pq = [(0, end[0], end[1], d) for d in range(4)]
    dist = {}

    while pq:
        cost, x, y, d = heapq.heappop(pq)

        state = (x, y, d)
        if state in dist:
            continue
        dist[state] = cost

        # Reverse of "move forward": come from behind
        # If we're at (x, y) facing d, we came from (x - DX[d], y - DY[d])
        px, py = x - DX[d], y - DY[d]
        if 0 <= py < len(grid) and 0 <= px < len(grid[0]) and grid[py][px] != '#':
            heapq.heappush(pq, (cost + 1, px, py, d))

        # Reverse of turn: came from same position with different direction
        heapq.heappush(pq, (cost + 1000, x, y, (d - 1) % 4))
        heapq.heappush(pq, (cost + 1000, x, y, (d + 1) % 4))

    return dist


def part1(grid, start, end):
    """Find the lowest score path from start to end."""
    dist = dijkstra_forward(grid, start)
    return min(dist.get((end[0], end[1], d), float('inf')) for d in range(4))


def part2(grid, start, end, best_score):
    """Count tiles that are part of any optimal path."""
    dist_from_start = dijkstra_forward(grid, start)
    dist_to_end = dijkstra_backward(grid, end)

    tiles_on_best_path = set()

    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] == '#':
                continue
            # Check if this tile is on any optimal path
            for d in range(4):
                state = (x, y, d)
                from_start = dist_from_start.get(state, float('inf'))
                to_end = dist_to_end.get(state, float('inf'))
                if from_start + to_end == best_score:
                    tiles_on_best_path.add((x, y))
                    break

    return len(tiles_on_best_path)


def main():
    input_path = Path(__file__).parent.parent / "input.txt"
    text = input_path.read_text()
    grid, start, end = parse_input(text)

    answer1 = part1(grid, start, end)
    print(f"Part 1: {answer1}")

    answer2 = part2(grid, start, end, answer1)
    print(f"Part 2: {answer2}")


if __name__ == "__main__":
    main()
