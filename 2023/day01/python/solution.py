from pathlib import Path

input_text = (Path(__file__).parent.parent / "input.txt").read_text().strip()
lines = input_text.split("\n")

WORDS = {
    "one": "1", "two": "2", "three": "3", "four": "4", "five": "5",
    "six": "6", "seven": "7", "eight": "8", "nine": "9"
}


def part1():
    def get_calibration_value(line: str) -> int:
        digits = [c for c in line if c.isdigit()]
        return int(digits[0] + digits[-1]) if digits else 0

    return sum(get_calibration_value(line) for line in lines)


def part2():
    def get_calibration_value(line: str) -> int:
        digits = []
        for i, char in enumerate(line):
            if char.isdigit():
                digits.append(char)
            else:
                for word, digit in WORDS.items():
                    if line[i:].startswith(word):
                        digits.append(digit)
                        break
        return int(digits[0] + digits[-1]) if digits else 0

    return sum(get_calibration_value(line) for line in lines)


if __name__ == "__main__":
    print(f"Part 1: {part1()}")
    print(f"Part 2: {part2()}")
