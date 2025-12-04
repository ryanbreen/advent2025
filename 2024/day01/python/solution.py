from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    left_list = []
    right_list = []

    for line in lines:
        left, right = map(int, line.split())
        left_list.append(left)
        right_list.append(right)

    # Sort both lists
    left_list.sort()
    right_list.sort()

    # Calculate total distance
    total_distance = sum(abs(l - r) for l, r in zip(left_list, right_list))
    return total_distance


def part2():
    left_list = []
    right_list = []

    for line in lines:
        left, right = map(int, line.split())
        left_list.append(left)
        right_list.append(right)

    # Count occurrences in right list
    right_counts = {}
    for num in right_list:
        right_counts[num] = right_counts.get(num, 0) + 1

    # Calculate similarity score
    similarity_score = sum(num * right_counts.get(num, 0) for num in left_list)
    return similarity_score


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
