#!/bin/bash

# Day 6: Tuning Trouble
# Find first position where last N characters are all unique

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Read the input data (first line, strip whitespace)
data=$(tr -d '[:space:]' < "$INPUT_FILE")
len=${#data}

# Find the first position where the last window_size characters are all unique
find_marker() {
    local window_size=$1
    local i window char_counts unique_count j char

    for ((i = window_size; i <= len; i++)); do
        window="${data:i-window_size:window_size}"

        # Check if all characters in window are unique by using associative array
        declare -A char_counts=()
        unique_count=0

        for ((j = 0; j < window_size; j++)); do
            char="${window:j:1}"
            if [[ -z "${char_counts[$char]}" ]]; then
                char_counts[$char]=1
                ((unique_count++))
            fi
        done

        if ((unique_count == window_size)); then
            echo "$i"
            return
        fi

        unset char_counts
    done

    echo "-1"
}

part1=$(find_marker 4)
part2=$(find_marker 14)

echo "Part 1: $part1"
echo "Part 2: $part2"
