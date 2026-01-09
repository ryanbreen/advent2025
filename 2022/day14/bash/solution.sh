#!/usr/bin/env bash

# Day 14: Regolith Reservoir - Falling sand simulation
# Note: This is slow in Bash but functional

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Global associative array for blocked positions
declare -A blocked
max_y=0

# Parse rock paths from input
parse_paths() {
    local -a points
    local x1 y1 x2 y2 x y

    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue

        # Split by ' -> '
        IFS=' -> ' read -ra points <<< "${line// -> / }"

        for ((i = 0; i < ${#points[@]} - 1; i++)); do
            IFS=',' read -r x1 y1 <<< "${points[i]}"
            IFS=',' read -r x2 y2 <<< "${points[i+1]}"

            # Track max y
            (( y1 > max_y )) && max_y=$y1
            (( y2 > max_y )) && max_y=$y2

            # Draw line
            if (( x1 == x2 )); then
                # Vertical line
                if (( y1 <= y2 )); then
                    for ((y = y1; y <= y2; y++)); do
                        blocked["$x1,$y"]=1
                    done
                else
                    for ((y = y2; y <= y1; y++)); do
                        blocked["$x1,$y"]=1
                    done
                fi
            else
                # Horizontal line
                if (( x1 <= x2 )); then
                    for ((x = x1; x <= x2; x++)); do
                        blocked["$x,$y1"]=1
                    done
                else
                    for ((x = x2; x <= x1; x++)); do
                        blocked["$x,$y1"]=1
                    done
                fi
            fi
        done
    done < "$INPUT_FILE"
}

# Simulate one unit of sand falling
# Returns 0 if sand came to rest, 1 if fell into abyss
# Sets global sand_x and sand_y for resting position
simulate_sand() {
    local use_floor=$1
    local floor_y=$((max_y + 2))
    local x=500 y=0

    while true; do
        # Check if sand has fallen below all rocks (into abyss)
        if [[ $use_floor -eq 0 ]] && (( y > max_y )); then
            return 1
        fi

        # Try to move down
        if [[ $use_floor -eq 1 ]] && (( y + 1 == floor_y )); then
            # Hit the floor
            sand_x=$x
            sand_y=$y
            return 0
        elif [[ -z "${blocked["$x,$((y + 1))"]}" ]]; then
            ((y++))
        # Try to move down-left
        elif [[ -z "${blocked["$((x - 1)),$((y + 1))"]}" ]]; then
            ((x--))
            ((y++))
        # Try to move down-right
        elif [[ -z "${blocked["$((x + 1)),$((y + 1))"]}" ]]; then
            ((x++))
            ((y++))
        # Sand comes to rest
        else
            sand_x=$x
            sand_y=$y
            return 0
        fi
    done
}

# Part 1: Count sand until it falls into abyss
part1() {
    # Reset blocked to only rocks
    declare -gA blocked
    max_y=0
    parse_paths

    local count=0
    while simulate_sand 0; do
        blocked["$sand_x,$sand_y"]=1
        ((count++))
    done

    echo "$count"
}

# Part 2: Count sand until source is blocked
part2() {
    # Reset blocked to only rocks
    declare -gA blocked
    max_y=0
    parse_paths

    local count=0
    while true; do
        simulate_sand 1
        blocked["$sand_x,$sand_y"]=1
        ((count++))
        if (( sand_x == 500 && sand_y == 0 )); then
            break
        fi
    done

    echo "$count"
}

# Main
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
