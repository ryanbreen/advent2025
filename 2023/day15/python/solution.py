#!/usr/bin/env python3

from pathlib import Path


def hash_algorithm(s: str) -> int:
    """Run the HASH algorithm on a string."""
    current = 0
    for c in s:
        current = ((current + ord(c)) * 17) % 256
    return current


def part1(steps: list[str]) -> int:
    """Sum of HASH values for all steps."""
    return sum(hash_algorithm(step) for step in steps)


def part2(steps: list[str]) -> int:
    """Run HASHMAP procedure and calculate focusing power."""
    boxes: list[list[tuple[str, int]]] = [[] for _ in range(256)]

    for step in steps:
        if '=' in step:
            label, focal = step.split('=')
            focal = int(focal)
            box_num = hash_algorithm(label)
            # Replace or add lens
            for i, (l, _) in enumerate(boxes[box_num]):
                if l == label:
                    boxes[box_num][i] = (label, focal)
                    break
            else:
                boxes[box_num].append((label, focal))
        else:  # '-' operation
            label = step[:-1]
            box_num = hash_algorithm(label)
            boxes[box_num] = [(l, f) for l, f in boxes[box_num] if l != label]

    # Calculate focusing power
    total = 0
    for box_num, box in enumerate(boxes):
        for slot, (_, focal) in enumerate(box, 1):
            total += (box_num + 1) * slot * focal
    return total


def main():
    input_file = Path(__file__).parent / "../input.txt"
    text = input_file.read_text().strip().replace('\n', '')
    steps = text.split(',')

    print(f"Part 1: {part1(steps)}")
    print(f"Part 2: {part2(steps)}")


if __name__ == "__main__":
    main()
