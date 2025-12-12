#!/usr/bin/env bash
# Day 12: Christmas Tree Farm - Polyomino Packing
#
# The solution checks if presents (polyominoes) can fit into rectangular regions.
# For this problem, the constraint is simply: total cells needed <= available cells.

input_file="../input.txt"

# Associative array for shape sizes (cell counts)
declare -A shapes

# Count '#' characters in a string (pure bash)
count_hashes() {
    local str="$1"
    local temp="${str//#/}"
    echo $(( ${#str} - ${#temp} ))
}

# Function to parse input and count regions that fit
solve() {
    local in_shapes=true
    local fitting_count=0
    local shape_idx shape_cell_count
    local dims width height counts
    local total_needed idx count cells_for_shape available

    while IFS= read -r line || [[ -n "$line" ]]; do
        # Skip empty lines
        [[ -z "$line" ]] && continue

        # Check if we're reading shape definitions or region definitions
        if [[ "$line" =~ ^[0-9]+:$ ]]; then
            # Shape definition start - extract shape index
            in_shapes=true
            shape_idx="${line%:}"
            shape_cell_count=0
        elif [[ "$in_shapes" == true && "$line" =~ ^[#.]+$ ]]; then
            # Shape line - count '#' characters (pure bash)
            count=$(count_hashes "$line")
            shape_cell_count=$((shape_cell_count + count))
            shapes[$shape_idx]=$shape_cell_count
        elif [[ "$line" =~ x.*: ]]; then
            # Region definition (e.g., "40x42: 38 37 45 42 54 41")
            in_shapes=false

            # Extract dimensions
            dims="${line%%:*}"
            width="${dims%x*}"
            height="${dims#*x}"

            # Extract counts
            counts="${line#*: }"

            # Calculate total cells needed
            total_needed=0
            idx=0
            # Intentional word splitting on $counts to iterate over space-separated values
            for count in $counts; do
                if [[ -n "${shapes[$idx]}" ]]; then
                    cells_for_shape=$((count * ${shapes[$idx]}))
                    total_needed=$((total_needed + cells_for_shape))
                fi
                ((idx++))
            done

            # Check if it fits
            available=$((width * height))
            if ((total_needed <= available)); then
                ((fitting_count++))
            fi
        fi
    done < "$input_file"

    echo "$fitting_count"
}

# Part 1: Count regions that can fit all their presents
part1=$(solve)
echo "Part 1: $part1"

# Part 2: Just a button click to finish - no computation needed
echo "Part 2: 0"
