from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    total = 0
    rows = len(lines)
    cols = len(lines[0]) if rows > 0 else 0

    # Helper function to check if a position has a symbol
    def is_symbol(r, c):
        if 0 <= r < rows and 0 <= c < cols:
            char = lines[r][c]
            return char != '.' and not char.isdigit()
        return False

    # Helper function to check if a number is adjacent to any symbol
    def is_adjacent_to_symbol(row, start_col, end_col):
        # Check all positions around the number
        for r in range(row - 1, row + 2):
            for c in range(start_col - 1, end_col + 2):
                if is_symbol(r, c):
                    return True
        return False

    # Find all numbers in the grid
    for row in range(rows):
        col = 0
        while col < cols:
            if lines[row][col].isdigit():
                # Found start of a number
                start_col = col
                num_str = ""
                while col < cols and lines[row][col].isdigit():
                    num_str += lines[row][col]
                    col += 1
                end_col = col - 1

                # Check if this number is adjacent to a symbol
                if is_adjacent_to_symbol(row, start_col, end_col):
                    total += int(num_str)
            else:
                col += 1

    return total


def part2():
    rows = len(lines)
    cols = len(lines[0]) if rows > 0 else 0

    # Map of gear position -> list of adjacent numbers
    gear_candidates = {}

    # Helper to get adjacent positions around a number
    def get_adjacent_positions(row, start_col, end_col):
        positions = []
        # Row above
        for c in range(start_col - 1, end_col + 2):
            positions.append((row - 1, c))
        # Same row (left and right)
        positions.append((row, start_col - 1))
        positions.append((row, end_col + 1))
        # Row below
        for c in range(start_col - 1, end_col + 2):
            positions.append((row + 1, c))
        return positions

    # Find all numbers and track which * positions they're adjacent to
    for row in range(rows):
        col = 0
        while col < cols:
            if lines[row][col].isdigit():
                start_col = col
                num_str = ""
                while col < cols and lines[row][col].isdigit():
                    num_str += lines[row][col]
                    col += 1
                end_col = col - 1
                number = int(num_str)

                # Find adjacent * symbols and add this number to their list
                for r, c in get_adjacent_positions(row, start_col, end_col):
                    if 0 <= r < rows and 0 <= c < cols and lines[r][c] == '*':
                        key = (r, c)
                        if key not in gear_candidates:
                            gear_candidates[key] = []
                        gear_candidates[key].append(number)
            else:
                col += 1

    # Find gears (exactly 2 adjacent numbers) and calculate gear ratios
    total = 0
    for numbers in gear_candidates.values():
        if len(numbers) == 2:
            total += numbers[0] * numbers[1]

    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
