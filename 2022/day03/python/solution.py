#!/usr/bin/env python3
import os

def parse_input(filename):
    with open(filename) as f:
        return [line.strip() for line in f if line.strip()]

def priority(char):
    if char.islower():
        return ord(char) - ord('a') + 1
    else:
        return ord(char) - ord('A') + 27

def part1(rucksacks):
    total = 0
    for rucksack in rucksacks:
        mid = len(rucksack) // 2
        first = set(rucksack[:mid])
        second = set(rucksack[mid:])
        common = first & second
        total += priority(common.pop())
    return total

def part2(rucksacks):
    total = 0
    for i in range(0, len(rucksacks), 3):
        group = rucksacks[i:i+3]
        common = set(group[0]) & set(group[1]) & set(group[2])
        total += priority(common.pop())
    return total

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    rucksacks = parse_input(input_file)

    print('Part 1:', part1(rucksacks))
    print('Part 2:', part2(rucksacks))

if __name__ == '__main__':
    main()
