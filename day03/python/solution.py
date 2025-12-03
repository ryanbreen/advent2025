from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    total = 0
    for line in lines:
        # Precompute max suffix: max_suffix[i] = max digit from position i to end
        n = len(line)
        max_suffix = [0] * n
        max_suffix[-1] = int(line[-1])
        for i in range(n - 2, -1, -1):
            max_suffix[i] = max(int(line[i]), max_suffix[i + 1])

        max_joltage = 0
        # For each possible first battery position
        for i in range(n - 1):
            first_digit = int(line[i])
            # The maximum second digit is the max from position i+1 onwards
            max_second = max_suffix[i + 1]
            joltage = first_digit * 10 + max_second
            max_joltage = max(max_joltage, joltage)

        total += max_joltage

    return total


def part2():
    total = 0
    for line in lines:
        n = len(line)
        k = 12  # Select exactly 12 batteries

        # Greedy algorithm to select k digits that form the maximum number
        result = []
        current_pos = 0

        for i in range(k):
            # How many digits we still need to select after this one
            remaining_needed = k - i - 1
            # Latest position we can start searching from
            search_end = n - remaining_needed

            # Find the maximum digit in the valid range
            max_digit = -1
            max_pos = current_pos
            for j in range(current_pos, search_end):
                digit = int(line[j])
                if digit > max_digit:
                    max_digit = digit
                    max_pos = j

            result.append(str(max_digit))
            current_pos = max_pos + 1

        joltage = int(''.join(result))
        total += joltage

    return total


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
