#!/usr/bin/env bash

# Read input into an array
mapfile -t depths < "../input.txt"

# Part 1: Count increases
part1() {
    local count=0
    local prev=${depths[0]}

    for ((i=1; i<${#depths[@]}; i++)); do
        if (( depths[i] > prev )); then
            ((count++))
        fi
        prev=${depths[i]}
    done

    echo "$count"
}

# Part 2: Count increases in 3-measurement sliding window sums
part2() {
    local count=0
    local len=${#depths[@]}

    # Compare window sums: (a + b + c) vs (b + c + d)
    # This simplifies to comparing a vs d
    for ((i=0; i<len-3; i++)); do
        if (( depths[i+3] > depths[i] )); then
            ((count++))
        fi
    done

    echo "$count"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
