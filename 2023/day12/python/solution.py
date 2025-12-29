#!/usr/bin/env python3
"""Advent of Code 2023 Day 12: Hot Springs"""

from functools import lru_cache
from pathlib import Path


def count_arrangements(pattern: str, groups: tuple[int, ...]) -> int:
    """
    Count the number of valid arrangements for a given pattern and groups.
    Uses memoization for efficiency.
    """
    @lru_cache(maxsize=None)
    def dp(pos: int, group_idx: int, current_run: int) -> int:
        """
        DP state: current position in pattern, current group index,
        and length of current run of damaged springs.
        """
        # Base case: reached end of pattern
        if pos == len(pattern):
            # Valid if we've matched all groups and no partial run
            if group_idx == len(groups) and current_run == 0:
                return 1
            # Or if we're on the last group and the run matches
            if group_idx == len(groups) - 1 and groups[group_idx] == current_run:
                return 1
            return 0

        result = 0
        char = pattern[pos]

        # Option 1: Place operational spring (.)
        if char in '.?':
            if current_run == 0:
                # No active run, just move forward
                result += dp(pos + 1, group_idx, 0)
            elif group_idx < len(groups) and groups[group_idx] == current_run:
                # End current run if it matches expected group size
                result += dp(pos + 1, group_idx + 1, 0)
            # Otherwise invalid (run doesn't match group)

        # Option 2: Place damaged spring (#)
        if char in '#?':
            if group_idx < len(groups) and current_run < groups[group_idx]:
                # Can extend current run
                result += dp(pos + 1, group_idx, current_run + 1)
            # Otherwise invalid (exceeds group size or no more groups)

        return result

    return dp(0, 0, 0)


def parse_line(line: str) -> tuple[str, tuple[int, ...]]:
    """Parse a line into pattern and groups."""
    parts = line.strip().split()
    pattern = parts[0]
    groups = tuple(map(int, parts[1].split(',')))
    return pattern, groups


def part1(lines: list[str]) -> int:
    """Sum of arrangement counts for all rows."""
    total = 0
    for line in lines:
        if not line.strip():
            continue
        pattern, groups = parse_line(line)
        total += count_arrangements(pattern, groups)
    return total


def unfold(pattern: str, groups: tuple[int, ...], times: int = 5) -> tuple[str, tuple[int, ...]]:
    """Unfold pattern and groups by repeating them 'times' times."""
    unfolded_pattern = '?'.join([pattern] * times)
    unfolded_groups = groups * times
    return unfolded_pattern, unfolded_groups


def part2(lines: list[str]) -> int:
    """Sum of arrangement counts for all rows after unfolding."""
    total = 0
    for line in lines:
        if not line.strip():
            continue
        pattern, groups = parse_line(line)
        unfolded_pattern, unfolded_groups = unfold(pattern, groups)
        total += count_arrangements(unfolded_pattern, unfolded_groups)
    return total


def main():
    lines = Path(__file__).parent.joinpath('../input.txt').read_text().splitlines()

    print(f"Part 1: {part1(lines)}")
    print(f"Part 2: {part2(lines)}")


if __name__ == '__main__':
    main()
