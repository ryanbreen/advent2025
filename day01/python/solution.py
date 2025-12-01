from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

# Parse input
lines = input_text.split("\n")


def part1():
    position = 50  # Starting position
    zero_count = 0

    for line in lines:
        if not line:  # Skip empty lines
            continue

        direction = line[0]
        distance = int(line[1:])

        if direction == 'L':
            position = (position - distance) % 100
        else:  # direction == 'R'
            position = (position + distance) % 100

        if position == 0:
            zero_count += 1

    return zero_count


def part2():
    position = 50  # Starting position
    zero_count = 0

    for line in lines:
        if not line:  # Skip empty lines
            continue

        direction = line[0]
        distance = int(line[1:])

        if direction == 'L':
            # Moving left (toward lower numbers)
            # We hit 0 after exactly 'position' steps, then every 100 steps after that
            # But if position is 0, we don't count the starting position
            # Count = 1 + floor((distance - position) / 100) if distance >= position and position > 0, else 0
            if position > 0 and distance >= position:
                zero_count += 1 + (distance - position) // 100
            elif position == 0 and distance >= 100:
                # Starting from 0, we hit it again after 100 steps, then every 100 steps
                zero_count += distance // 100

        else:  # direction == 'R'
            # Moving right (toward higher numbers)
            # We hit 0 after (100 - position) steps, then every 100 steps after that
            # But if position is 0, we don't count the starting position
            # Count = 1 + floor((distance - (100 - position)) / 100) if distance >= (100 - position) and position > 0, else 0
            if position > 0:
                steps_to_zero = 100 - position
                if distance >= steps_to_zero:
                    zero_count += 1 + (distance - steps_to_zero) // 100
            else:  # position == 0
                # Starting from 0, we hit it again after 100 steps, then every 100 steps
                if distance >= 100:
                    zero_count += distance // 100

        # Update position
        if direction == 'L':
            position = (position - distance) % 100
        else:
            position = (position + distance) % 100

    return zero_count


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
