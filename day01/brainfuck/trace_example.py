#!/usr/bin/env python3
"""
Trace through the example step by step to show how the algorithm works
"""

def trace_example():
    """Show step-by-step execution for the example"""
    position = 50
    zero_count = 0

    instructions = [
        ("L", 68),
        ("L", 30),
        ("R", 48),
        ("L", 5),
        ("R", 60),
        ("L", 55),
        ("L", 1),
        ("L", 99),
        ("R", 14),
        ("L", 82),
    ]

    print("Day 1: Secret Entrance - Step by Step Trace")
    print("=" * 60)
    print(f"Starting position: {position}")
    print()

    for i, (direction, amount) in enumerate(instructions, 1):
        old_position = position

        if direction == 'L':
            position = (position - amount) % 100
            operation = f"{old_position} - {amount} = {position}"
        else:
            position = (position + amount) % 100
            operation = f"{old_position} + {amount} = {position}"

        if position == 0:
            zero_count += 1
            marker = " ← ZERO! (count = {})".format(zero_count)
        else:
            marker = ""

        print(f"Step {i:2d}: {direction}{amount:3d}  →  {operation:20s}  →  Position: {position:2d}{marker}")

    print()
    print("=" * 60)
    print(f"Total times dial pointed at 0: {zero_count}")
    print(f"Expected: 3")
    print(f"Match: {'✓ PASS' if zero_count == 3 else '✗ FAIL'}")


if __name__ == "__main__":
    trace_example()
