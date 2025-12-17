#!/usr/bin/env python3
"""Day 17: Chronospatial Computer - 3-bit VM emulator"""

import re
from pathlib import Path


def parse_input(text: str) -> tuple[int, int, int, list[int]]:
    """Parse registers and program from input."""
    lines = text.strip().split('\n')
    a = int(re.search(r'Register A: (\d+)', lines[0]).group(1))
    b = int(re.search(r'Register B: (\d+)', lines[1]).group(1))
    c = int(re.search(r'Register C: (\d+)', lines[2]).group(1))
    program = list(map(int, re.search(r'Program: ([\d,]+)', lines[4]).group(1).split(',')))
    return a, b, c, program


def run_program(a: int, b: int, c: int, program: list[int]) -> list[int]:
    """Execute the 3-bit computer program and return output."""
    ip = 0
    output = []

    def combo(operand: int) -> int:
        """Get combo operand value."""
        if operand <= 3:
            return operand
        elif operand == 4:
            return a
        elif operand == 5:
            return b
        elif operand == 6:
            return c
        else:
            raise ValueError(f"Invalid combo operand: {operand}")

    while ip < len(program):
        opcode = program[ip]
        operand = program[ip + 1]

        if opcode == 0:  # adv - A = A // 2^combo
            a = a >> combo(operand)
        elif opcode == 1:  # bxl - B = B XOR literal
            b = b ^ operand
        elif opcode == 2:  # bst - B = combo % 8
            b = combo(operand) & 7
        elif opcode == 3:  # jnz - jump if A != 0
            if a != 0:
                ip = operand
                continue
        elif opcode == 4:  # bxc - B = B XOR C
            b = b ^ c
        elif opcode == 5:  # out - output combo % 8
            output.append(combo(operand) & 7)
        elif opcode == 6:  # bdv - B = A // 2^combo
            b = a >> combo(operand)
        elif opcode == 7:  # cdv - C = A // 2^combo
            c = a >> combo(operand)

        ip += 2

    return output


def part1(text: str) -> str:
    """Run the program and return comma-separated output."""
    a, b, c, program = parse_input(text)
    output = run_program(a, b, c, program)
    return ','.join(map(str, output))


def part2(text: str) -> int:
    """Find initial A value that makes program output itself."""
    _, b, c, program = parse_input(text)

    # The program loops, outputting one digit per iteration, dividing A by 8 each time.
    # We need to find A such that output == program.
    # Work backwards from the last digit - build A 3 bits at a time.

    def search(target_idx: int, current_a: int) -> int | None:
        """Recursively search for A value that produces program output."""
        if target_idx < 0:
            return current_a

        # Try all 8 possible 3-bit values for this position
        for bits in range(8):
            candidate_a = (current_a << 3) | bits
            if candidate_a == 0 and target_idx == len(program) - 1:
                continue  # A can't be 0 at start (would halt immediately without output)

            output = run_program(candidate_a, b, c, program)

            # Check if output matches the suffix of the program
            expected = program[target_idx:]
            if output == expected:
                result = search(target_idx - 1, candidate_a)
                if result is not None:
                    return result

        return None

    return search(len(program) - 1, 0)


if __name__ == '__main__':
    input_path = Path(__file__).parent.parent / 'input.txt'
    text = input_path.read_text()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))
