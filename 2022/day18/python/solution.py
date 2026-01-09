#!/usr/bin/env python3
import os
from collections import deque

def parse_input(text):
    """Parse cube coordinates."""
    cubes = set()
    for line in text.strip().split('\n'):
        x, y, z = map(int, line.split(','))
        cubes.add((x, y, z))
    return cubes

# 6 directions: +x, -x, +y, -y, +z, -z
DIRECTIONS = [
    (1, 0, 0), (-1, 0, 0),
    (0, 1, 0), (0, -1, 0),
    (0, 0, 1), (0, 0, -1)
]

def part1(text):
    """Count total surface area (all exposed faces)."""
    cubes = parse_input(text)
    surface_area = 0

    for x, y, z in cubes:
        for dx, dy, dz in DIRECTIONS:
            if (x + dx, y + dy, z + dz) not in cubes:
                surface_area += 1

    return surface_area

def part2(text):
    """Count only exterior surface area (excluding trapped air pockets)."""
    cubes = parse_input(text)

    # Find bounding box with 1 unit padding
    min_x = min(c[0] for c in cubes) - 1
    max_x = max(c[0] for c in cubes) + 1
    min_y = min(c[1] for c in cubes) - 1
    max_y = max(c[1] for c in cubes) + 1
    min_z = min(c[2] for c in cubes) - 1
    max_z = max(c[2] for c in cubes) + 1

    # BFS to find all exterior air cells
    exterior = set()
    queue = deque([(min_x, min_y, min_z)])
    exterior.add((min_x, min_y, min_z))

    while queue:
        x, y, z = queue.popleft()

        for dx, dy, dz in DIRECTIONS:
            nx, ny, nz = x + dx, y + dy, z + dz

            # Stay within bounds
            if not (min_x <= nx <= max_x and min_y <= ny <= max_y and min_z <= nz <= max_z):
                continue

            # Skip cubes and already visited
            if (nx, ny, nz) in cubes or (nx, ny, nz) in exterior:
                continue

            exterior.add((nx, ny, nz))
            queue.append((nx, ny, nz))

    # Count faces touching exterior air
    surface_area = 0
    for x, y, z in cubes:
        for dx, dy, dz in DIRECTIONS:
            if (x + dx, y + dy, z + dz) in exterior:
                surface_area += 1

    return surface_area

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
