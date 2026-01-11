#!/usr/bin/env bash

# Day 5: Hydrothermal Venture

# Declare associative arrays for grid counts
declare -A grid1
declare -A grid2

# Sign function: returns -1, 0, or 1
sign() {
    local n=$1
    if ((n > 0)); then
        echo 1
    elif ((n < 0)); then
        echo -1
    else
        echo 0
    fi
}

# Parse input and process lines
parse_and_count() {
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue

        # Parse "x1,y1 -> x2,y2"
        local x1 y1 x2 y2
        if [[ $line =~ ^([0-9]+),([0-9]+)\ -\>\ ([0-9]+),([0-9]+)$ ]]; then
            x1=${BASH_REMATCH[1]}
            y1=${BASH_REMATCH[2]}
            x2=${BASH_REMATCH[3]}
            y2=${BASH_REMATCH[4]}
        else
            continue
        fi

        local dx dy
        dx=$(sign $((x2 - x1)))
        dy=$(sign $((y2 - y1)))

        local is_diagonal=0
        if ((dx != 0 && dy != 0)); then
            is_diagonal=1
        fi

        # Trace the line
        local x=$x1
        local y=$y1
        while true; do
            local key="$x,$y"

            # Part 2: all lines
            ((grid2[$key]++))

            # Part 1: only horizontal/vertical
            if ((is_diagonal == 0)); then
                ((grid1[$key]++))
            fi

            # Stop when we reach the end
            if ((x == x2 && y == y2)); then
                break
            fi

            ((x += dx))
            ((y += dy))
        done
    done < "../input.txt"
}

# Count points with overlap (value >= 2)
count_overlaps() {
    local -n arr=$1
    local count=0
    for key in "${!arr[@]}"; do
        if ((arr[$key] >= 2)); then
            ((count++))
        fi
    done
    echo "$count"
}

# Main
parse_and_count

echo "Part 1: $(count_overlaps grid1)"
echo "Part 2: $(count_overlaps grid2)"
