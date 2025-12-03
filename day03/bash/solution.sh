#!/bin/bash

# Read input file
input_file="../input.txt"

part1() {
    local total=0

    while IFS= read -r line; do
        local n=${#line}
        local max_joltage=0

        # For each possible first battery position
        for ((i=0; i<n-1; i++)); do
            local first_digit=${line:i:1}

            # Find the maximum digit from position i+1 onwards
            local max_second=0
            for ((j=i+1; j<n; j++)); do
                local digit=${line:j:1}
                if [ "$digit" -gt "$max_second" ]; then
                    max_second=$digit
                fi
            done

            # Calculate joltage for this combination
            local joltage=$((first_digit * 10 + max_second))
            if [ "$joltage" -gt "$max_joltage" ]; then
                max_joltage=$joltage
            fi
        done

        total=$((total + max_joltage))
    done < "$input_file"

    echo "$total"
}

part2() {
    local total=0
    local k=12  # Select exactly 12 batteries

    while IFS= read -r line; do
        local n=${#line}
        local result=""
        local current_pos=0

        # Greedy algorithm to select k digits that form the maximum number
        for ((i=0; i<k; i++)); do
            # How many digits we still need to select after this one
            local remaining_needed=$((k - i - 1))
            # Latest position we can start searching from
            local search_end=$((n - remaining_needed))

            # Find the maximum digit in the valid range
            local max_digit=-1
            local max_pos=$current_pos

            for ((j=current_pos; j<search_end; j++)); do
                local digit=${line:j:1}
                if [ "$digit" -gt "$max_digit" ]; then
                    max_digit=$digit
                    max_pos=$j
                fi
            done

            result="${result}${max_digit}"
            current_pos=$((max_pos + 1))
        done

        # Add this bank's joltage to total
        total=$((total + result))
    done < "$input_file"

    echo "$total"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
