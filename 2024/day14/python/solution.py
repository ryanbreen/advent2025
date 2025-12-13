from pathlib import Path
import re

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()

WIDTH = 101
HEIGHT = 103


def parse_robots(text: str) -> list[tuple[int, int, int, int]]:
    """Parse robot positions and velocities."""
    robots = []
    for line in text.split("\n"):
        match = re.match(r"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)", line)
        if match:
            px, py, vx, vy = map(int, match.groups())
            robots.append((px, py, vx, vy))
    return robots


def simulate(robots: list[tuple], seconds: int) -> list[tuple[int, int]]:
    """Simulate robot movement for given seconds."""
    positions = []
    for px, py, vx, vy in robots:
        # Position after 'seconds' time, with wrapping
        new_x = (px + vx * seconds) % WIDTH
        new_y = (py + vy * seconds) % HEIGHT
        positions.append((new_x, new_y))
    return positions


def count_quadrants(positions: list[tuple[int, int]]) -> tuple[int, int, int, int]:
    """Count robots in each quadrant, excluding middle row/column."""
    mid_x = WIDTH // 2   # 50
    mid_y = HEIGHT // 2  # 51

    q1 = q2 = q3 = q4 = 0

    for x, y in positions:
        if x == mid_x or y == mid_y:
            continue  # Skip robots on middle lines

        if x < mid_x and y < mid_y:
            q1 += 1  # Top-left
        elif x > mid_x and y < mid_y:
            q2 += 1  # Top-right
        elif x < mid_x and y > mid_y:
            q3 += 1  # Bottom-left
        else:
            q4 += 1  # Bottom-right

    return q1, q2, q3, q4


def part1() -> int:
    """Part 1: Safety factor after 100 seconds."""
    robots = parse_robots(input_text)
    positions = simulate(robots, 100)
    q1, q2, q3, q4 = count_quadrants(positions)
    return q1 * q2 * q3 * q4


def part2() -> int:
    """Part 2: Find when robots form a Christmas tree pattern."""
    robots = parse_robots(input_text)

    # The Christmas tree appears when robots cluster together
    # Look for a frame with a long horizontal line of robots (tree base/border)
    for seconds in range(1, WIDTH * HEIGHT + 1):
        positions = simulate(robots, seconds)
        pos_set = set(positions)

        # Look for a horizontal line of at least 20 consecutive robots
        for y in range(HEIGHT):
            max_consecutive = 0
            consecutive = 0
            for x in range(WIDTH):
                if (x, y) in pos_set:
                    consecutive += 1
                    max_consecutive = max(max_consecutive, consecutive)
                else:
                    consecutive = 0

            if max_consecutive >= 20:
                return seconds

    return -1


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
