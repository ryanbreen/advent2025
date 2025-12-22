#!/usr/bin/env python3
"""Day 22: Monkey Market - Pseudorandom number generation for market prices"""

from pathlib import Path
from collections import defaultdict

def next_secret(secret: int) -> int:
    """Generate the next secret number using mix and prune operations."""
    # Step 1: multiply by 64, mix, prune
    secret ^= (secret << 6)  # * 64 = << 6
    secret &= 0xFFFFFF       # % 16777216 = & (2^24 - 1)

    # Step 2: divide by 32, mix, prune
    secret ^= (secret >> 5)  # // 32 = >> 5
    secret &= 0xFFFFFF

    # Step 3: multiply by 2048, mix, prune
    secret ^= (secret << 11) # * 2048 = << 11
    secret &= 0xFFFFFF

    return secret


def generate_secrets(initial: int, count: int) -> list[int]:
    """Generate a sequence of secret numbers."""
    secrets = [initial]
    secret = initial
    for _ in range(count):
        secret = next_secret(secret)
        secrets.append(secret)
    return secrets


def part1(initial_secrets: list[int]) -> int:
    """Sum of the 2000th secret number for each buyer."""
    total = 0
    for initial in initial_secrets:
        secret = initial
        for _ in range(2000):
            secret = next_secret(secret)
        total += secret
    return total


def part2(initial_secrets: list[int]) -> int:
    """Find the best sequence of 4 price changes to maximize bananas."""
    # For each buyer, track price (last digit) and changes
    # Find the 4-change sequence that gives maximum total bananas

    # Map from (change1, change2, change3, change4) -> total bananas
    sequence_totals = defaultdict(int)

    for initial in initial_secrets:
        # Generate 2001 secrets (initial + 2000 new)
        secrets = generate_secrets(initial, 2000)
        prices = [s % 10 for s in secrets]

        # Calculate changes
        changes = [prices[i+1] - prices[i] for i in range(len(prices)-1)]

        # Track first occurrence of each 4-change sequence for this buyer
        seen = set()
        for i in range(len(changes) - 3):
            seq = (changes[i], changes[i+1], changes[i+2], changes[i+3])
            if seq not in seen:
                seen.add(seq)
                # Price we get is after these 4 changes
                sequence_totals[seq] += prices[i + 4]

    return max(sequence_totals.values())


def main():
    input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()
    initial_secrets = [int(line) for line in input_text.split('\n') if line.strip()]

    print(f"Part 1: {part1(initial_secrets)}")
    print(f"Part 2: {part2(initial_secrets)}")


if __name__ == "__main__":
    main()
