"""Day 9: Mirage Maintenance - Sequence extrapolation using difference pyramids."""

from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()


def parse_input(text: str) -> list[list[int]]:
    """Parse input into a list of integer sequences."""
    return [[int(x) for x in line.split()] for line in text.split('\n')]


def differences(seq: list[int]) -> list[int]:
    """Compute pairwise differences of a sequence."""
    return [b - a for a, b in zip(seq, seq[1:])]


def extrapolate_next(seq: list[int]) -> int:
    """
    Extrapolate the next value in the sequence.

    Key insight: The next value equals the sum of all rightmost values
    in the difference pyramid.
    """
    total = 0
    current = seq
    while any(x != 0 for x in current):
        total += current[-1]
        current = differences(current)
    return total


def part1() -> int:
    """Sum of all extrapolated next values."""
    histories = parse_input(input_text)
    return sum(extrapolate_next(h) for h in histories)


def part2() -> int:
    """
    Sum of all extrapolated previous values.

    Key insight: Extrapolating backward is equivalent to reversing
    the sequence and extrapolating forward.
    """
    histories = parse_input(input_text)
    return sum(extrapolate_next(list(reversed(h))) for h in histories)


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
