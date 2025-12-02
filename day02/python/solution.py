from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    def is_invalid_id(num: int) -> bool:
        """Check if a number is invalid (a pattern repeated twice)."""
        s = str(num)
        length = len(s)

        # Must have even length to be repeated twice
        if length % 2 != 0:
            return False

        # Check if it starts with 0 (leading zeros not allowed)
        if s[0] == '0':
            return False

        # Split in half and check if both halves are identical
        mid = length // 2
        first_half = s[:mid]
        second_half = s[mid:]

        return first_half == second_half

    # Parse ranges from input
    ranges = []
    for part in input_text.split(','):
        part = part.strip()
        if '-' in part:
            # Split on the last dash to handle negative numbers (though not in this input)
            # Actually, we need to find where the dash is that separates start from end
            # Since ranges are like "11-22", we split on dash
            parts = part.split('-')
            # For ranges like "1188511880-1188511890", there's only one dash
            if len(parts) == 2:
                start, end = int(parts[0]), int(parts[1])
                ranges.append((start, end))

    total = 0
    for start, end in ranges:
        for num in range(start, end + 1):
            if is_invalid_id(num):
                total += num

    return total


def part2():
    def is_invalid_id(num: int) -> bool:
        """Check if a number is invalid (a pattern repeated at least twice)."""
        s = str(num)
        length = len(s)

        # Check if it starts with 0 (leading zeros not allowed)
        if s[0] == '0':
            return False

        # Try all possible pattern lengths from 1 to length//2
        # The pattern must be repeated at least twice, so max pattern length is length//2
        for pattern_length in range(1, length // 2 + 1):
            # Check if the string length is divisible by pattern_length
            if length % pattern_length == 0:
                pattern = s[:pattern_length]
                # Check if repeating the pattern gives us the original string
                if pattern * (length // pattern_length) == s:
                    return True

        return False

    # Parse ranges from input
    ranges = []
    for part in input_text.split(','):
        part = part.strip()
        if '-' in part:
            parts = part.split('-')
            if len(parts) == 2:
                start, end = int(parts[0]), int(parts[1])
                ranges.append((start, end))

    total = 0
    for start, end in ranges:
        for num in range(start, end + 1):
            if is_invalid_id(num):
                total += num

    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
