from pathlib import Path
from collections import deque

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()
lines = input_text.split("\n")

# Define pipe connections: each pipe connects to certain directions
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
    for r, row in enumerate(grid):
        for c, ch in enumerate(row):
            if ch == 'S':
                return (r, c)
    return None

def get_neighbors(grid, pos):
    """Get valid pipe neighbors that connect to this position."""
    r, c = pos
    rows, cols = len(grid), len(grid[0])
    ch = grid[r][c]

    if ch == 'S':
        # S can connect to any adjacent pipe that connects back to it
        neighbors = []
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols:
                adj_ch = grid[nr][nc]
                if adj_ch in PIPE_CONNECTIONS:
                    # Check if adjacent pipe connects back to S
                    for adj_dr, adj_dc in PIPE_CONNECTIONS[adj_ch]:
                        if nr + adj_dr == r and nc + adj_dc == c:
                            neighbors.append((nr, nc))
                            break
        return neighbors
    elif ch in PIPE_CONNECTIONS:
        neighbors = []
        for dr, dc in PIPE_CONNECTIONS[ch]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols:
                neighbors.append((nr, nc))
        return neighbors
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
    rows, cols = len(grid), len(grid[0])

    connections = []
    for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        nr, nc = r + dr, c + dc
        if (nr, nc) in loop_positions:
            adj_ch = grid[nr][nc]
            if adj_ch in PIPE_CONNECTIONS:
                # Check if this pipe connects back to S
                for adj_dr, adj_dc in PIPE_CONNECTIONS[adj_ch]:
                    if nr + adj_dr == r and nc + adj_dc == c:
                        connections.append((dr, dc))
                        break

    connections = set(connections)

    for pipe, dirs in PIPE_CONNECTIONS.items():
        if set(dirs) == connections:
            return pipe

    return 'S'


def part1():
    start = find_start(lines)
    distances = find_loop(lines, start)
    return max(distances.values())


def part2():
    """Count tiles enclosed by the loop using ray casting (crossing number)."""
    start = find_start(lines)
    distances = find_loop(lines, start)
    loop_positions = set(distances.keys())

    # Replace S with its actual pipe type
    start_pipe = determine_start_pipe(lines, start, loop_positions)
    grid = [list(row) for row in lines]
    grid[start[0]][start[1]] = start_pipe

    rows, cols = len(grid), len(grid[0])
    enclosed = 0

    for r in range(rows):
        inside = False
        for c in range(cols):
            if (r, c) in loop_positions:
                ch = grid[r][c]
                # Count vertical crossings (|, L, J go "north")
                # Using "north" rule: count pipes that have a north connection
                if ch in '|LJ':
                    inside = not inside
            else:
                if inside:
                    enclosed += 1

    return enclosed


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
