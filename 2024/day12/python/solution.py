from pathlib import Path
from collections import deque

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")
grid = [list(line) for line in lines]
rows, cols = len(grid), len(grid[0])


def find_regions():
    """Find all connected regions in the grid using BFS."""
    visited = set()
    regions = []

    for r in range(rows):
        for c in range(cols):
            if (r, c) in visited:
                continue

            # BFS to find all cells in this region
            plant = grid[r][c]
            region = set()
            queue = deque([(r, c)])

            while queue:
                cr, cc = queue.popleft()
                if (cr, cc) in visited:
                    continue
                if cr < 0 or cr >= rows or cc < 0 or cc >= cols:
                    continue
                if grid[cr][cc] != plant:
                    continue

                visited.add((cr, cc))
                region.add((cr, cc))

                for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                    nr, nc = cr + dr, cc + dc
                    if (nr, nc) not in visited:
                        queue.append((nr, nc))

            regions.append(region)

    return regions


def calculate_perimeter(region):
    """Calculate perimeter of a region (edges not touching same region)."""
    perimeter = 0
    for r, c in region:
        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nr, nc = r + dr, c + dc
            if (nr, nc) not in region:
                perimeter += 1
    return perimeter


def part1():
    """Calculate total fencing cost: sum of area * perimeter for each region."""
    regions = find_regions()
    total = 0
    for region in regions:
        area = len(region)
        perimeter = calculate_perimeter(region)
        total += area * perimeter
    return total


def count_sides(region):
    """Count number of sides (corners) in a region."""
    corners = 0
    for r, c in region:
        # Check all 4 corners of this cell
        # Each corner is defined by checking two orthogonal neighbors and the diagonal
        # Convex: both orthogonal out
        # Concave: both orthogonal in, diagonal out

        # Top-left corner: check UP and LEFT
        up = (r - 1, c) in region
        down = (r + 1, c) in region
        left = (r, c - 1) in region
        right = (r, c + 1) in region
        up_left = (r - 1, c - 1) in region
        up_right = (r - 1, c + 1) in region
        down_left = (r + 1, c - 1) in region
        down_right = (r + 1, c + 1) in region

        # Top-left corner
        if not up and not left:  # convex
            corners += 1
        elif up and left and not up_left:  # concave
            corners += 1

        # Top-right corner
        if not up and not right:  # convex
            corners += 1
        elif up and right and not up_right:  # concave
            corners += 1

        # Bottom-left corner
        if not down and not left:  # convex
            corners += 1
        elif down and left and not down_left:  # concave
            corners += 1

        # Bottom-right corner
        if not down and not right:  # convex
            corners += 1
        elif down and right and not down_right:  # concave
            corners += 1

    return corners


def part2():
    """Calculate total fencing cost using sides instead of perimeter."""
    regions = find_regions()
    total = 0
    for region in regions:
        area = len(region)
        sides = count_sides(region)
        total += area * sides
    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
