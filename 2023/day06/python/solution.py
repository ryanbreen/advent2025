import math
from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()
lines = input_text.split("\n")

def parse_races():
    times = list(map(int, lines[0].split(':')[1].split()))
    distances = list(map(int, lines[1].split(':')[1].split()))
    return list(zip(times, distances))

def count_ways_to_win(time, record):
    """
    Count the number of ways to beat the record.

    If we hold the button for t ms, we travel t * (time - t) mm.
    We need: t * (time - t) > record
    Solving: -t^2 + time*t - record > 0
    Roots: t = (time ± sqrt(time^2 - 4*record)) / 2
    """
    discriminant = time * time - 4 * record
    if discriminant <= 0:
        return 0

    sqrt_d = math.sqrt(discriminant)
    t_low = (time - sqrt_d) / 2
    t_high = (time + sqrt_d) / 2

    # We need integer values strictly between the roots
    first = math.floor(t_low) + 1
    last = math.ceil(t_high) - 1

    if last < first:
        return 0
    return last - first + 1


def part1():
    races = parse_races()
    result = 1
    for time, record in races:
        ways = count_ways_to_win(time, record)
        result *= ways
    return result


def part2():
    races = parse_races()
    time = int(''.join(str(t) for t, _ in races))
    record = int(''.join(str(d) for _, d in races))
    return count_ways_to_win(time, record)


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
