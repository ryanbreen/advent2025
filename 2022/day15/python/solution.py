#!/usr/bin/env python3
import os
import re

def parse_sensors(text):
    """Parse sensor and beacon positions."""
    sensors = []
    pattern = r'Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)'

    for line in text.strip().split('\n'):
        match = re.match(pattern, line)
        sx, sy, bx, by = map(int, match.groups())
        dist = abs(sx - bx) + abs(sy - by)  # Manhattan distance
        sensors.append((sx, sy, bx, by, dist))

    return sensors

def get_coverage_at_row(sensors, row):
    """Get ranges covered by sensors at a specific row."""
    ranges = []

    for sx, sy, bx, by, dist in sensors:
        # How far is the row from the sensor?
        row_dist = abs(sy - row)
        if row_dist > dist:
            continue  # Sensor doesn't reach this row

        # Calculate x-range covered at this row
        x_spread = dist - row_dist
        ranges.append((sx - x_spread, sx + x_spread))

    return merge_ranges(ranges)

def merge_ranges(ranges):
    """Merge overlapping ranges."""
    if not ranges:
        return []

    ranges.sort()
    merged = [ranges[0]]

    for start, end in ranges[1:]:
        if start <= merged[-1][1] + 1:
            merged[-1] = (merged[-1][0], max(merged[-1][1], end))
        else:
            merged.append((start, end))

    return merged

def part1(text):
    """Count positions that cannot contain a beacon at row y=2000000."""
    sensors = parse_sensors(text)
    target_row = 2000000

    ranges = get_coverage_at_row(sensors, target_row)

    # Count total coverage
    total = sum(end - start + 1 for start, end in ranges)

    # Subtract beacons that are on this row
    beacons_on_row = set()
    for sx, sy, bx, by, dist in sensors:
        if by == target_row:
            beacons_on_row.add(bx)

    return total - len(beacons_on_row)

def part2(text):
    """Find the distress beacon's tuning frequency."""
    sensors = parse_sensors(text)
    max_coord = 4000000

    # For each row, find if there's a gap in coverage
    for row in range(max_coord + 1):
        ranges = get_coverage_at_row(sensors, row)

        # Clip ranges to search area
        clipped = []
        for start, end in ranges:
            if end < 0 or start > max_coord:
                continue
            clipped.append((max(0, start), min(max_coord, end)))

        clipped = merge_ranges(clipped)

        # Check if full row is covered
        if len(clipped) == 1 and clipped[0][0] == 0 and clipped[0][1] == max_coord:
            continue

        # Found a gap - the beacon is in the gap
        if len(clipped) > 1:
            # Gap between ranges
            x = clipped[0][1] + 1
        elif clipped[0][0] > 0:
            x = 0
        else:
            x = clipped[0][1] + 1

        return x * 4000000 + row

    return None

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
