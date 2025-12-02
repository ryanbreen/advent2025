#!/usr/bin/env bash

# Function to check if a number is invalid (repeated pattern at least twice)
is_invalid() {
    local num="$1"
    local len=${#num}

    # Try all possible pattern lengths (from 1 to len/2)
    for ((pattern_len=1; pattern_len<=len/2; pattern_len++)); do
        # Check if the number length is divisible by pattern length
        if (( len % pattern_len == 0 )); then
            local pattern="${num:0:$pattern_len}"
            local is_match=1

            # Check if the entire number is made of this pattern repeated
            for ((i=pattern_len; i<len; i+=pattern_len)); do
                local segment="${num:$i:$pattern_len}"
                if [[ "$segment" != "$pattern" ]]; then
                    is_match=0
                    break
                fi
            done

            # If we found a matching pattern and it repeats at least twice
            if (( is_match == 1 )); then
                local repeats=$((len / pattern_len))
                if (( repeats >= 2 )); then
                    return 0
                fi
            fi
        fi
    done

    return 1
}

# Read the input file
input=$(cat /Users/wrb/fun/code/advent2025/day02/input.txt)

# Remove newlines and extra spaces
input=$(echo "$input" | tr -d '\n')

# Split by commas
IFS=',' read -ra ranges <<< "$input"

total=0

# Process each range
for range in "${ranges[@]}"; do
    # Skip empty ranges
    [[ -z "$range" ]] && continue

    # Parse the range (format: start-end)
    IFS='-' read -ra parts <<< "$range"
    start="${parts[0]}"
    end="${parts[1]}"

    # Iterate through all numbers in the range
    for ((num=start; num<=end; num++)); do
        if is_invalid "$num"; then
            total=$((total + num))
        fi
    done
done

echo "$total"
