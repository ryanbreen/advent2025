#!/usr/bin/env python3
import os

def parse_paths(text):
    """Parse rock paths and return set of rock positions."""
    rocks = set()
    for line in text.strip().split('\n'):
        points = line.split(' -> ')
        for i in range(len(points) - 1):
            x1, y1 = map(int, points[i].split(','))
            x2, y2 = map(int, points[i + 1].split(','))

            # Draw line from (x1, y1) to (x2, y2)
            if x1 == x2:  # vertical line
                for y in range(min(y1, y2), max(y1, y2) + 1):
                    rocks.add((x1, y))
            else:  # horizontal line
                for x in range(min(x1, x2), max(x1, x2) + 1):
                    rocks.add((x, y1))
    return rocks

def simulate_sand(blocked, max_y, floor=False):
    """
    Simulate one unit of sand falling.
    Returns the resting position, or None if sand falls into abyss.
    """
    x, y = 500, 0

    while True:
        # Check if sand has fallen below all rocks (into abyss)
        if not floor and y > max_y:
            return None

        # Try to move down
        if floor and y + 1 == max_y + 2:
            # Hit the floor
            return (x, y)
        elif (x, y + 1) not in blocked:
            y += 1
        # Try to move down-left
        elif (x - 1, y + 1) not in blocked:
            x -= 1
            y += 1
        # Try to move down-right
        elif (x + 1, y + 1) not in blocked:
            x += 1
            y += 1
        # Sand comes to rest
        else:
            return (x, y)

def part1(text):
    """Count sand units that come to rest before sand falls into abyss."""
    rocks = parse_paths(text)
    max_y = max(y for x, y in rocks)
    blocked = rocks.copy()
    count = 0

    while True:
        pos = simulate_sand(blocked, max_y)
        if pos is None:
            break
        blocked.add(pos)
        count += 1

    return count

def part2(text):
    """Count sand units until source is blocked (with floor)."""
    rocks = parse_paths(text)
    max_y = max(y for x, y in rocks)
    blocked = rocks.copy()
    count = 0

    while True:
        pos = simulate_sand(blocked, max_y, floor=True)
        blocked.add(pos)
        count += 1
        if pos == (500, 0):
            break

    return count

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
