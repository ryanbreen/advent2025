from pathlib import Path
from collections import deque

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")

# Directions for 8 neighbors (including diagonals)
DIRECTIONS = [
    (-1, -1), (-1, 0), (-1, 1),
    (0, -1),           (0, 1),
    (1, -1),  (1, 0),  (1, 1)
]

# ============== PRECOMPUTE ROLL POSITIONS AND NEIGHBORS ==============
grid = lines
rows = len(grid)
cols = len(grid[0]) if rows > 0 else 0

roll_positions = []   # List of (r, c) for each roll
pos_to_index = {}     # Dict: (r, c) => index in roll_positions
roll_neighbors = []   # List of lists: neighbors for each roll

for r in range(rows):
    for c in range(cols):
        if grid[r][c] == '@':
            idx = len(roll_positions)
            roll_positions.append((r, c))
            pos_to_index[(r, c)] = idx

            # Precompute neighbors for this roll
            neighbors = []
            for dr, dc in DIRECTIONS:
                nr, nc = r + dr, c + dc
                if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] == '@':
                    neighbors.append((nr, nc))
            roll_neighbors.append(neighbors)

num_rolls = len(roll_positions)


def part1():
    """Count rolls of paper that can be accessed by a forklift.

    A roll can be accessed if it has fewer than 4 adjacent rolls
    in the 8 surrounding positions (including diagonals).
    """
    accessible_count = 0

    for i in range(num_rolls):
        neighbor_count = len(roll_neighbors[i])
        if neighbor_count < 4:
            accessible_count += 1

    return accessible_count


def part2():
    """Count total rolls removed by iteratively removing accessible rolls.

    A roll can be removed if it has fewer than 4 adjacent rolls.
    After removal, check again for newly accessible rolls.
    Repeat until no more rolls can be removed.
    """
    # Track which rolls are still active
    active = {pos: True for pos in roll_positions}

    # Compute initial neighbor counts
    neighbor_count = [0] * num_rolls
    for i in range(num_rolls):
        count = 0
        for neighbor_pos in roll_neighbors[i]:
            if neighbor_pos in active:
                count += 1
        neighbor_count[i] = count

    # Initialize queue with accessible rolls (neighbor count < 4)
    queue = deque()
    in_queue = set()
    for i in range(num_rolls):
        if neighbor_count[i] < 4:
            queue.append(i)
            in_queue.add(i)

    # Process queue
    total_removed = 0

    while queue:
        next_queue = deque()

        while queue:
            idx = queue.popleft()
            pos = roll_positions[idx]

            # Skip if already removed
            if pos not in active:
                continue

            # Remove this roll
            del active[pos]
            total_removed += 1

            # Update neighbors' counts
            for neighbor_pos in roll_neighbors[idx]:
                if neighbor_pos in active:
                    neighbor_idx = pos_to_index[neighbor_pos]
                    neighbor_count[neighbor_idx] -= 1

                    # Add to queue if now accessible and not already queued
                    if neighbor_count[neighbor_idx] < 4 and neighbor_idx not in in_queue:
                        next_queue.append(neighbor_idx)
                        in_queue.add(neighbor_idx)

        queue = next_queue

    return total_removed


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
