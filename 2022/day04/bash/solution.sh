#!/bin/bash

# Day 4: Camp Cleanup
# Detect range overlaps in elf cleaning assignments

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Use awk to process both parts in a single pass
awk -F'[-,]' '
{
    a1 = $1; b1 = $2; a2 = $3; b2 = $4

    # Part 1: Check if one range fully contains the other
    if ((a1 <= a2 && b1 >= b2) || (a2 <= a1 && b2 >= b1)) {
        part1++
    }

    # Part 2: Check if ranges overlap at all
    if (a1 <= b2 && a2 <= b1) {
        part2++
    }
}
END {
    print "Part 1:", part1
    print "Part 2:", part2
}
' "$INPUT_FILE"
