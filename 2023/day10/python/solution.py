from pathlib import Path
from collections import deque

# Pipe connections: each pipe connects to certain directions
# Directions: N=(-1,0), S=(1,0), E=(0,1), W=(0,-1)
PIPE_CONNECTIONS = {
    '|': [(-1, 0), (1, 0)],   # N, S
    '-': [(0, -1), (0, 1)],   # W, E
    'L': [(-1, 0), (0, 1)],   # N, E
    'J': [(-1, 0), (0, -1)],  # N, W
    '7': [(1, 0), (0, -1)],   # S, W
    'F': [(1, 0), (0, 1)],    # S, E
}


def find_start(grid):
    """Find the starting position 'S' in the grid."""
    return next((r, c) for r, row in enumerate(grid) for c, ch in enumerate(row) if ch == 'S')


def connects_back(grid, nr, nc, r, c):
    """Check if the pipe at (nr, nc) connects back to (r, c)."""
    adj_ch = grid[nr][nc]
    return adj_ch in PIPE_CONNECTIONS and any(
        nr + adj_dr == r and nc + adj_dc == c
        for adj_dr, adj_dc in PIPE_CONNECTIONS[adj_ch]
    )


def get_neighbors(grid, pos):
    """Get valid pipe neighbors that connect to this position."""
    r, c = pos
    rows, cols = len(grid), len(grid[0])
    ch = grid[r][c]

    if ch == 'S':
        # S can connect to any adjacent pipe that connects back to it
        return [
            (nr, nc)
            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]
            for nr, nc in [(r + dr, c + dc)]
            if 0 <= nr < rows and 0 <= nc < cols and connects_back(grid, nr, nc, r, c)
        ]
    elif ch in PIPE_CONNECTIONS:
        return [
            (r + dr, c + dc)
            for dr, dc in PIPE_CONNECTIONS[ch]
            if 0 <= r + dr < rows and 0 <= c + dc < cols
        ]
    else:
        return []


def find_loop(grid, start):
    """BFS to find the main loop and distances from start."""
    distances = {start: 0}
    queue = deque([start])

    while queue:
        pos = queue.popleft()
        for neighbor in get_neighbors(grid, pos):
            if neighbor not in distances:
                distances[neighbor] = distances[pos] + 1
                queue.append(neighbor)

    return distances


def determine_start_pipe(grid, start, loop_positions):
    """Determine what pipe type S actually is based on its connections."""
    r, c = start

    connections = {
        (dr, dc)
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]
        if (r + dr, c + dc) in loop_positions and connects_back(grid, r + dr, c + dc, r, c)
    }

    return next((pipe for pipe, dirs in PIPE_CONNECTIONS.items() if set(dirs) == connections), 'S')


def part1(grid):
    """Find the farthest point from the start along the loop."""
    start = find_start(grid)
    distances = find_loop(grid, start)
    return max(distances.values())


def part2(grid):
    """Count tiles enclosed by the loop using ray casting (crossing number)."""
    start = find_start(grid)
    distances = find_loop(grid, start)
    loop_positions = set(distances.keys())

    # Replace S with its actual pipe type
    start_pipe = determine_start_pipe(grid, start, loop_positions)
    grid = [list(row) for row in grid]
    grid[start[0]][start[1]] = start_pipe

    rows, cols = len(grid), len(grid[0])
    enclosed = 0

    for r in range(rows):
        inside = False
        for c in range(cols):
            if (r, c) in loop_positions:
                # Count vertical crossings using "north" rule
                if grid[r][c] in '|LJ':
                    inside = not inside
            elif inside:
                enclosed += 1

    return enclosed


if __name__ == "__main__":
    input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()
    lines = input_text.split("\n")

    print(f"Part 1: {part1(lines)}")
    print(f"Part 2: {part2(lines)}")
