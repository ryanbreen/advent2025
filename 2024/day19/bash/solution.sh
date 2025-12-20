#!/usr/bin/env bash

# Day 19: Linen Layout
# Dynamic programming solution with memoization

set -euo pipefail

readonly INPUT_FILE="${1:-../input.txt}"

# Read input
input=$(<"$INPUT_FILE")

# Get first line as patterns
patterns_line=$(echo "$input" | head -n 1)
# Parse comma-separated patterns into array
IFS=', ' read -ra patterns <<< "$patterns_line"

# Get designs (after blank line)
mapfile -t designs < <(echo "$input" | tail -n +3)

# Trim whitespace from patterns (handles tabs, spaces, carriage returns, etc.)
for i in "${!patterns[@]}"; do
    patterns[$i]="${patterns[$i]//[[:space:]]/}"
done

# Part 1: Count designs that can be formed
# Uses dynamic programming with memoization

can_form() {
    local design="$1"
    local len=${#design}

    # dp[i] = 1 if design[i:] can be formed, 0 otherwise
    declare -a dp
    dp[$len]=1  # Empty string can always be formed

    # Fill from end to start
    for ((pos = len - 1; pos >= 0; pos--)); do
        dp[$pos]=0
        for pattern in "${patterns[@]}"; do
            local plen=${#pattern}
            local end=$((pos + plen))
            if ((end <= len)); then
                local substr="${design:pos:plen}"
                if [[ "$substr" == "$pattern" ]] && [[ "${dp[$end]:-0}" -eq 1 ]]; then
                    dp[$pos]=1
                    break
                fi
            fi
        done
    done

    echo "${dp[0]:-0}"
}

# Part 2: Count number of ways to form each design
# Uses dynamic programming, may produce large numbers

count_ways() {
    local design="$1"
    local len=${#design}

    # dp[i] = number of ways to form design[i:]
    # Use bc for arbitrary precision
    declare -a dp
    dp[$len]=1  # One way to form empty string

    # Fill from end to start
    for ((pos = len - 1; pos >= 0; pos--)); do
        dp[$pos]=0
        for pattern in "${patterns[@]}"; do
            local plen=${#pattern}
            local end=$((pos + plen))
            if ((end <= len)); then
                local substr="${design:pos:plen}"
                if [[ "$substr" == "$pattern" ]]; then
                    # Add dp[end] to dp[pos] using bc for large numbers
                    dp[$pos]=$(bc <<< "${dp[$pos]:-0} + ${dp[$end]:-0}")
                fi
            fi
        done
    done

    echo "${dp[0]:-0}"
}

# Part 1
part1_count=0
for design in "${designs[@]}"; do
    [[ -z "$design" ]] && continue
    result=$(can_form "$design")
    ((part1_count += result))
done
echo "Part 1: $part1_count"

# Part 2
part2_sum=0
for design in "${designs[@]}"; do
    [[ -z "$design" ]] && continue
    ways=$(count_ways "$design")
    part2_sum=$(bc <<< "$part2_sum + $ways")
done
echo "Part 2: $part2_sum"
