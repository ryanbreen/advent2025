#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Part 1: Find common char between two halves of each line
# Part 2: Find common char among groups of 3 lines
# Priority: a-z=1-26, A-Z=27-52

awk '
function priority(c) {
    n = index("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", c)
    return n
}

function find_common_two(s1, s2,    i, c, seen) {
    # Build set from s1
    for (i = 1; i <= length(s1); i++) {
        c = substr(s1, i, 1)
        seen[c] = 1
    }
    # Find first char from s2 that exists in seen
    for (i = 1; i <= length(s2); i++) {
        c = substr(s2, i, 1)
        if (c in seen) return c
    }
    return ""
}

function find_common_three(s1, s2, s3,    i, c, seen1, seen2) {
    # Build set from s1
    for (i = 1; i <= length(s1); i++) {
        c = substr(s1, i, 1)
        seen1[c] = 1
    }
    # Build set from s2
    for (i = 1; i <= length(s2); i++) {
        c = substr(s2, i, 1)
        seen2[c] = 1
    }
    # Find first char from s3 that exists in both
    for (i = 1; i <= length(s3); i++) {
        c = substr(s3, i, 1)
        if ((c in seen1) && (c in seen2)) return c
    }
    return ""
}

{
    line = $0
    if (line == "") next

    # Part 1: split line in half, find common
    len = length(line)
    mid = int(len / 2)
    first = substr(line, 1, mid)
    second = substr(line, mid + 1)
    common = find_common_two(first, second)
    part1_total += priority(common)

    # Part 2: collect lines in groups of 3
    group[NR % 3] = line
    if (NR % 3 == 0) {
        common = find_common_three(group[1], group[2], group[0])
        part2_total += priority(common)
    }
}

END {
    print "Part 1:", part1_total
    print "Part 2:", part2_total
}
' "$INPUT_FILE"
