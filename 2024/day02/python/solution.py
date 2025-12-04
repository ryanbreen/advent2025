from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def is_safe(levels):
    """Check if a report is safe according to Part 1 rules."""
    if len(levels) < 2:
        return True

    # Determine if increasing or decreasing based on first pair
    diffs = [levels[i+1] - levels[i] for i in range(len(levels) - 1)]

    # Check if all differences are in valid range (1-3 or -3 to -1)
    # and all have the same sign (all positive or all negative)
    all_increasing = all(1 <= d <= 3 for d in diffs)
    all_decreasing = all(-3 <= d <= -1 for d in diffs)

    return all_increasing or all_decreasing


def part1():
    """Count reports that are safe."""
    safe_count = 0
    for line in lines:
        levels = list(map(int, line.split()))
        if is_safe(levels):
            safe_count += 1
    return safe_count


def part2():
    """Count reports that are safe or can be made safe by removing one level."""
    safe_count = 0
    for line in lines:
        levels = list(map(int, line.split()))

        # Check if already safe
        if is_safe(levels):
            safe_count += 1
            continue

        # Try removing each level one at a time
        for i in range(len(levels)):
            modified = levels[:i] + levels[i+1:]
            if is_safe(modified):
                safe_count += 1
                break

    return safe_count


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
