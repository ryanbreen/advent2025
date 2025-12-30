#!/usr/bin/env python3

from pathlib import Path


def parse_input(text: str) -> list[list[str]]:
    """Parse input into a 2D grid."""
    return [list(line) for line in text.strip().split('\n')]


def tilt_north(grid: list[list[str]]) -> None:
    """Tilt the grid north, moving all round rocks up."""
    rows, cols = len(grid), len(grid[0])
    for col in range(cols):
        write_pos = 0
        for row in range(rows):
            if grid[row][col] == '#':
                write_pos = row + 1
            elif grid[row][col] == 'O':
                grid[row][col] = '.'
                grid[write_pos][col] = 'O'
                write_pos += 1


def tilt_south(grid: list[list[str]]) -> None:
    """Tilt the grid south, moving all round rocks down."""
    rows, cols = len(grid), len(grid[0])
    for col in range(cols):
        write_pos = rows - 1
        for row in range(rows - 1, -1, -1):
            if grid[row][col] == '#':
                write_pos = row - 1
            elif grid[row][col] == 'O':
                grid[row][col] = '.'
                grid[write_pos][col] = 'O'
                write_pos -= 1


def tilt_west(grid: list[list[str]]) -> None:
    """Tilt the grid west, moving all round rocks left."""
    rows, cols = len(grid), len(grid[0])
    for row in range(rows):
        write_pos = 0
        for col in range(cols):
            if grid[row][col] == '#':
                write_pos = col + 1
            elif grid[row][col] == 'O':
                grid[row][col] = '.'
                grid[row][write_pos] = 'O'
                write_pos += 1


def tilt_east(grid: list[list[str]]) -> None:
    """Tilt the grid east, moving all round rocks right."""
    rows, cols = len(grid), len(grid[0])
    for row in range(rows):
        write_pos = cols - 1
        for col in range(cols - 1, -1, -1):
            if grid[row][col] == '#':
                write_pos = col - 1
            elif grid[row][col] == 'O':
                grid[row][col] = '.'
                grid[row][write_pos] = 'O'
                write_pos -= 1


def spin_cycle(grid: list[list[str]]) -> None:
    """Perform one spin cycle: N, W, S, E."""
    tilt_north(grid)
    tilt_west(grid)
    tilt_south(grid)
    tilt_east(grid)


def grid_to_tuple(grid: list[list[str]]) -> tuple:
    """Convert grid to hashable tuple for cycle detection."""
    return tuple(tuple(row) for row in grid)


def calculate_load(grid: list[list[str]]) -> int:
    """Calculate total load on north support beams."""
    rows = len(grid)
    total = 0
    for row in range(rows):
        for cell in grid[row]:
            if cell == 'O':
                total += rows - row
    return total


def part1(grid: list[list[str]]) -> int:
    """Tilt north and calculate load."""
    grid = [row[:] for row in grid]
    tilt_north(grid)
    return calculate_load(grid)


def part2(grid: list[list[str]]) -> int:
    """Run 1 billion spin cycles and calculate load."""
    grid = [row[:] for row in grid]
    target = 1_000_000_000

    seen = {}
    cycle_num = 0

    while cycle_num < target:
        state = grid_to_tuple(grid)
        if state in seen:
            cycle_start = seen[state]
            cycle_length = cycle_num - cycle_start
            remaining = (target - cycle_num) % cycle_length
            for _ in range(remaining):
                spin_cycle(grid)
            return calculate_load(grid)

        seen[state] = cycle_num
        spin_cycle(grid)
        cycle_num += 1

    return calculate_load(grid)


def main():
    input_file = Path(__file__).parent / "../input.txt"
    text = input_file.read_text()
    grid = parse_input(text)

    print(f"Part 1: {part1(grid)}")
    print(f"Part 2: {part2(grid)}")


if __name__ == "__main__":
    main()
