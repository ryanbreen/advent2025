from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
grid = input_text.split("\n")
rows = len(grid)
cols = len(grid[0])

# 8 directions: right, left, down, up, and 4 diagonals
DIRECTIONS = [
    (0, 1),   # right
    (0, -1),  # left
    (1, 0),   # down
    (-1, 0),  # up
    (1, 1),   # down-right
    (1, -1),  # down-left
    (-1, 1),  # up-right
    (-1, -1), # up-left
]


def part1():
    target = "XMAS"
    count = 0

    for r in range(rows):
        for c in range(cols):
            # Try each direction from this position
            for dr, dc in DIRECTIONS:
                # Check if XMAS fits in this direction
                found = True
                for i, ch in enumerate(target):
                    nr, nc = r + dr * i, c + dc * i
                    if nr < 0 or nr >= rows or nc < 0 or nc >= cols:
                        found = False
                        break
                    if grid[nr][nc] != ch:
                        found = False
                        break
                if found:
                    count += 1

    return count


def part2():
    # Find X-MAS patterns: two MAS strings forming an X with A in the center
    # Each diagonal can be MAS or SAM
    count = 0

    # Check each possible center point (A must be in the middle)
    for r in range(1, rows - 1):
        for c in range(1, cols - 1):
            if grid[r][c] != 'A':
                continue

            # Get the four corners
            top_left = grid[r - 1][c - 1]
            top_right = grid[r - 1][c + 1]
            bottom_left = grid[r + 1][c - 1]
            bottom_right = grid[r + 1][c + 1]

            # Check diagonal 1 (top-left to bottom-right): MAS or SAM
            diag1_ok = (top_left == 'M' and bottom_right == 'S') or (top_left == 'S' and bottom_right == 'M')

            # Check diagonal 2 (top-right to bottom-left): MAS or SAM
            diag2_ok = (top_right == 'M' and bottom_left == 'S') or (top_right == 'S' and bottom_left == 'M')

            if diag1_ok and diag2_ok:
                count += 1

    return count


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
