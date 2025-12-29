#!/bin/bash
set -euo pipefail

# Day 9: Mirage Maintenance
# Read sequences, compute successive differences until all zeros,
# then extrapolate next (Part 1) and previous (Part 2) values.

# =============================================================================
# Configuration
# =============================================================================

INPUT_FILE="${1:-../input.txt}"

# =============================================================================
# Functions
# =============================================================================

# Build difference pyramid and save first/last values at each level
# Args: sequence array (passed via first_values and last_values arrays)
# Sets: first_values[], last_values[], max_level
build_pyramid() {
    local -n seq_ref=$1
    local n=${#seq_ref[@]}

    # Initialize working array with original sequence
    local -a current=("${seq_ref[@]}")

    # Reset tracking arrays
    first_values=()
    last_values=()
    max_level=0

    # Build pyramid: compute differences until all zeros
    while true; do
        local len=${#current[@]}

        # Save first and last values at this level
        first_values+=("${current[0]}")
        last_values+=("${current[len - 1]}")

        # Check if we should stop (length 1 or all zeros)
        if ((len <= 1)); then
            break
        fi

        # Compute differences for next level
        local -a next=()
        local all_zero=1

        for ((i = 0; i < len - 1; i++)); do
            local diff=$((current[i + 1] - current[i]))
            next+=("$diff")
            if [[ $diff -ne 0 ]]; then
                all_zero=0
            fi
        done

        max_level=$((max_level + 1))

        if ((all_zero)); then
            # Save the all-zeros level's endpoints
            first_values+=("${next[0]}")
            last_values+=("${next[${#next[@]} - 1]}")
            break
        fi

        current=("${next[@]}")
    done
}

# Extrapolate next value (Part 1): sum of last values bottom-up
# Uses: last_values[]
# Returns: result via extrapolated_next variable
extrapolate_next() {
    local carry=0
    local i

    for ((i = ${#last_values[@]} - 1; i >= 0; i--)); do
        carry=$((last_values[i] + carry))
    done

    extrapolated_next=$carry
}

# Extrapolate previous value (Part 2): alternating subtraction of first values
# Uses: first_values[]
# Returns: result via extrapolated_prev variable
extrapolate_prev() {
    local carry=0
    local i

    for ((i = ${#first_values[@]} - 1; i >= 0; i--)); do
        carry=$((first_values[i] - carry))
    done

    extrapolated_prev=$carry
}

# =============================================================================
# Main Processing
# =============================================================================

part1_sum=0
part2_sum=0

# Arrays to store pyramid edge values (set by build_pyramid)
declare -a first_values
declare -a last_values
max_level=0

# Process each sequence
while IFS= read -r line || [[ -n "$line" ]]; do
    # Skip empty lines
    [[ -z "$line" ]] && continue

    # Parse sequence into array
    read -ra sequence <<< "$line"

    # Build difference pyramid, saving first/last at each level
    build_pyramid sequence

    # Part 1: extrapolate next value
    extrapolate_next
    part1_sum=$((part1_sum + extrapolated_next))

    # Part 2: extrapolate previous value
    extrapolate_prev
    part2_sum=$((part2_sum + extrapolated_prev))

done < "$INPUT_FILE"

# =============================================================================
# Output
# =============================================================================

echo "Part 1: $part1_sum"
echo "Part 2: $part2_sum"
