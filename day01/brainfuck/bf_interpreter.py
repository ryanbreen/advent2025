#!/usr/bin/env python3
"""Simple Brainfuck interpreter"""

def brainfuck(code, input_data=""):
    """Execute brainfuck code with optional input"""
    # Remove non-command characters
    code = ''.join(c for c in code if c in '><+-.,[]')

    # Initialize
    tape = [0] * 30000
    ptr = 0
    code_ptr = 0
    input_ptr = 0
    output = []

    # Match brackets
    brackets = {}
    stack = []
    for i, c in enumerate(code):
        if c == '[':
            stack.append(i)
        elif c == ']':
            if not stack:
                raise ValueError(f"Unmatched ] at position {i}")
            left = stack.pop()
            brackets[left] = i
            brackets[i] = left

    if stack:
        raise ValueError(f"Unmatched [ at position {stack[0]}")

    # Execute
    steps = 0
    max_steps = 100_000_000

    while code_ptr < len(code) and steps < max_steps:
        cmd = code[code_ptr]
        steps += 1

        if cmd == '>':
            ptr += 1
            if ptr >= len(tape):
                tape.extend([0] * 10000)
        elif cmd == '<':
            ptr -= 1
            if ptr < 0:
                raise ValueError("Pointer moved below 0")
        elif cmd == '+':
            tape[ptr] = (tape[ptr] + 1) % 256
        elif cmd == '-':
            tape[ptr] = (tape[ptr] - 1) % 256
        elif cmd == '.':
            output.append(chr(tape[ptr]))
        elif cmd == ',':
            if input_ptr < len(input_data):
                tape[ptr] = ord(input_data[input_ptr])
                input_ptr += 1
            else:
                tape[ptr] = 0
        elif cmd == '[':
            if tape[ptr] == 0:
                code_ptr = brackets[code_ptr]
        elif cmd == ']':
            if tape[ptr] != 0:
                code_ptr = brackets[code_ptr]

        code_ptr += 1

    if steps >= max_steps:
        raise ValueError("Maximum steps exceeded")

    return ''.join(output)


if __name__ == "__main__":
    import sys

    if len(sys.argv) < 2:
        print("Usage: bf_interpreter.py <bf_file> [input_file]")
        sys.exit(1)

    with open(sys.argv[1], 'r') as f:
        code = f.read()

    input_data = ""
    if len(sys.argv) > 2:
        with open(sys.argv[2], 'r') as f:
            input_data = f.read()

    output = brainfuck(code, input_data)
    print(output, end='')
