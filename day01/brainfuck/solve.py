#!/usr/bin/env python3
"""
Solve Day 1: Secret Entrance
Part 1: Count how many times dial ends at 0 after each rotation
"""

def solve_part1(input_file):
    """Solve part 1 of the problem"""
    position = 50  # Starting position
    zero_count = 0

    with open(input_file, 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue

            direction = line[0]  # 'L' or 'R'
            amount = int(line[1:])  # The number

            if direction == 'L':
                position = (position - amount) % 100
            else:  # 'R'
                position = (position + amount) % 100

            if position == 0:
                zero_count += 1

    return zero_count


def solve_part2(input_file):
    """Solve part 2: count clicks through 0"""
    position = 50
    zero_count = 0

    with open(input_file, 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue

            direction = line[0]
            amount = int(line[1:])

            if direction == 'L':
                # Count how many times we click to land on 0 going left
                # After click k (1 <= k <= amount), we're at (position - k) % 100
                # We land on 0 when k ≡ position (mod 100)
                # So k = position, position+100, position+200, ...
                if position == 0:
                    # k = 100, 200, ... (we start at 0, so first click goes to 99)
                    hits = amount // 100
                else:
                    # k = position, position+100, ...
                    if amount >= position:
                        hits = (amount - position) // 100 + 1
                    else:
                        hits = 0

                zero_count += hits
                position = (position - amount) % 100
            else:  # 'R'
                # Count how many times we click to land on 0 going right
                # After click k (1 <= k <= amount), we're at (position + k) % 100
                # We land on 0 when k ≡ -position (mod 100)
                # So k = 100-position, 200-position, 300-position, ...
                hits = (position + amount) // 100

                zero_count += hits
                position = (position + amount) % 100

    return zero_count


if __name__ == "__main__":
    import sys

    input_file = "../input.txt"
    if len(sys.argv) > 1:
        input_file = sys.argv[1]

    part1 = solve_part1(input_file)
    print(f"Part 1: {part1}")

    part2 = solve_part2(input_file)
    print(f"Part 2: {part2}")
