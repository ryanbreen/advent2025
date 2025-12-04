#!/usr/bin/env bash

input_path="$(dirname "$0")/../input.txt"

# Read all lines into an array
lines=()
while IFS= read -r line; do
    lines+=("$line")
done < "$input_path"

# Function to check if a report is safe
is_safe() {
    local -a levels=("$@")
    local len=${#levels[@]}

    if [[ $len -lt 2 ]]; then
        return 1
    fi

    # Calculate diffs
    local -a diffs=()
    local i
    for ((i = 0; i < len - 1; i++)); do
        diffs[i]=$((levels[i+1] - levels[i]))
    done

    # Check if all increasing or all decreasing
    local all_increasing=1
    local all_decreasing=1
    local d

    for d in "${diffs[@]}"; do
        if [[ $d -le 0 ]]; then
            all_increasing=0
        fi
        if [[ $d -ge 0 ]]; then
            all_decreasing=0
        fi
    done

    if [[ $all_increasing -eq 0 && $all_decreasing -eq 0 ]]; then
        return 1
    fi

    # Check if all diffs are 1-3 in absolute value
    for d in "${diffs[@]}"; do
        local abs_d=${d#-}  # Remove negative sign if present
        if [[ $abs_d -lt 1 || $abs_d -gt 3 ]]; then
            return 1
        fi
    done

    return 0
}

# Function to check if a report is safe with dampener
is_safe_with_dampener() {
    local -a levels=("$@")
    local len=${#levels[@]}

    # Check if already safe
    if is_safe "${levels[@]}"; then
        return 0
    fi

    # Try removing each level one at a time
    local i j
    for ((i = 0; i < len; i++)); do
        local -a modified=()
        for ((j = 0; j < len; j++)); do
            if [[ $j -ne $i ]]; then
                modified+=("${levels[j]}")
            fi
        done

        if is_safe "${modified[@]}"; then
            return 0
        fi
    done

    return 1
}

# Part 1
part1_count=0
for line in "${lines[@]}"; do
    read -ra levels <<< "$line"
    if is_safe "${levels[@]}"; then
        ((part1_count++))
    fi
done

# Part 2
part2_count=0
for line in "${lines[@]}"; do
    read -ra levels <<< "$line"
    if is_safe_with_dampener "${levels[@]}"; then
        ((part2_count++))
    fi
done

echo "Part 1: $part1_count"
echo "Part 2: $part2_count"
