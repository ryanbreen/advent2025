from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input - convert to integers
depths = [int(line) for line in input_text.split("\n")]


def part1():
    """Count the number of times a depth measurement increases from the previous."""
    count = 0
    for i in range(1, len(depths)):
        if depths[i] > depths[i - 1]:
            count += 1
    return count


def part2():
    """Count increases in 3-measurement sliding window sums."""
    # Create sliding window sums of 3 consecutive measurements
    window_sums = []
    for i in range(len(depths) - 2):
        window_sums.append(depths[i] + depths[i + 1] + depths[i + 2])

    # Count how many times the sum increases
    count = 0
    for i in range(1, len(window_sums)):
        if window_sums[i] > window_sums[i - 1]:
            count += 1
    return count


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
