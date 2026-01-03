import re
from pathlib import Path
from collections import defaultdict

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()
lines = input_text.split("\n")
rows = len(lines)
cols = len(lines[0]) if rows > 0 else 0

# Pre-compute symbol and gear positions as sets for O(1) lookup
symbols = set()
gears = set()
for r, line in enumerate(lines):
    for c, ch in enumerate(line):
        if ch != '.' and not ch.isdigit():
            symbols.add((r, c))
            if ch == '*':
                gears.add((r, c))

# Pre-extract all numbers with positions using regex
number_data = []
for r, line in enumerate(lines):
    for m in re.finditer(r'\d+', line):
        number_data.append((int(m.group()), r, m.start(), m.end() - 1))


def get_adjacent(row, start_col, end_col):
    """Generate all adjacent positions around a number."""
    # Above and below rows
    for c in range(start_col - 1, end_col + 2):
        yield row - 1, c
        yield row + 1, c
    # Left and right
    yield row, start_col - 1
    yield row, end_col + 1


def part1():
    total = 0
    for value, row, start_col, end_col in number_data:
        for pos in get_adjacent(row, start_col, end_col):
            if pos in symbols:
                total += value
                break
    return total


def part2():
    gear_numbers = defaultdict(list)

    for value, row, start_col, end_col in number_data:
        adjacent_gears = set()
        for pos in get_adjacent(row, start_col, end_col):
            if pos in gears:
                adjacent_gears.add(pos)
        for g in adjacent_gears:
            gear_numbers[g].append(value)

    total = 0
    for nums in gear_numbers.values():
        if len(nums) == 2:
            total += nums[0] * nums[1]
    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
