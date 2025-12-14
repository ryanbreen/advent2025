#!/usr/bin/env bash

set -euo pipefail

# Read input
input_file="../input.txt"
mapfile -t grid < "$input_file"

rows=${#grid[@]}
cols=${#grid[0]}

# Global arrays for tracking visited cells and regions
declare -A visited
declare -a regions_cells
declare -a regions_indices

# BFS to find all cells in a region starting from (r, c)
bfs_region() {
    local start_r=$1
    local start_c=$2
    local plant="${grid[$start_r]:$start_c:1}"

    # Queue for BFS: store as "r,c"
    local -a queue=("$start_r,$start_c")
    local -a region=()

    while [ ${#queue[@]} -gt 0 ]; do
        # Dequeue
        local current="${queue[0]}"
        queue=("${queue[@]:1}")

        local cr="${current%,*}"
        local cc="${current#*,}"

        # Skip if already visited
        if [ -n "${visited[$cr,$cc]:-}" ]; then
            continue
        fi

        # Check bounds
        if [ "$cr" -lt 0 ] || [ "$cr" -ge "$rows" ] || [ "$cc" -lt 0 ] || [ "$cc" -ge "$cols" ]; then
            continue
        fi

        # Check if same plant type
        local cell_plant="${grid[$cr]:$cc:1}"
        if [ "$cell_plant" != "$plant" ]; then
            continue
        fi

        # Mark as visited and add to region
        visited[$cr,$cc]=1
        region+=("$cr,$cc")

        # Add neighbors to queue
        local -a directions=(
            "$((cr)),$((cc+1))"  # right
            "$((cr)),$((cc-1))"  # left
            "$((cr+1)),$((cc))"  # down
            "$((cr-1)),$((cc))"  # up
        )

        for dir in "${directions[@]}"; do
            local nr="${dir%,*}"
            local nc="${dir#*,}"
            if [ -z "${visited[$nr,$nc]:-}" ]; then
                queue+=("$nr,$nc")
            fi
        done
    done

    # Store region as space-separated list
    regions_cells+=("${region[*]}")
}

# Find all regions in the grid
find_regions() {
    visited=()
    regions_cells=()

    for ((r = 0; r < rows; r++)); do
        for ((c = 0; c < cols; c++)); do
            if [ -z "${visited[$r,$c]:-}" ]; then
                bfs_region "$r" "$c"
            fi
        done
    done
}

# Calculate perimeter of a region
calculate_perimeter() {
    local region_str="$1"
    local -a region_array=($region_str)

    # Create set for fast lookup
    declare -A region_set
    for cell in "${region_array[@]}"; do
        region_set[$cell]=1
    done

    local perimeter=0

    for cell in "${region_array[@]}"; do
        local r="${cell%,*}"
        local c="${cell#*,}"

        # Check all 4 neighbors
        local -a neighbors=(
            "$((r)),$((c+1))"
            "$((r)),$((c-1))"
            "$((r+1)),$((c))"
            "$((r-1)),$((c))"
        )

        for neighbor in "${neighbors[@]}"; do
            if [ -z "${region_set[$neighbor]:-}" ]; then
                ((perimeter++))
            fi
        done
    done

    echo "$perimeter"
}

# Count corners (sides) in a region
count_corners() {
    local region_str="$1"
    local -a region_array=($region_str)

    # Create set for fast lookup
    declare -A region_set
    for cell in "${region_array[@]}"; do
        region_set[$cell]=1
    done

    local corners=0

    for cell in "${region_array[@]}"; do
        local r="${cell%,*}"
        local c="${cell#*,}"

        # Check all 8 neighbors
        local up_in=0
        local down_in=0
        local left_in=0
        local right_in=0
        local up_left_in=0
        local up_right_in=0
        local down_left_in=0
        local down_right_in=0

        [ -n "${region_set[$((r-1)),$c]:-}" ] && up_in=1
        [ -n "${region_set[$((r+1)),$c]:-}" ] && down_in=1
        [ -n "${region_set[$r,$((c-1))]:-}" ] && left_in=1
        [ -n "${region_set[$r,$((c+1))]:-}" ] && right_in=1
        [ -n "${region_set[$((r-1)),$((c-1))]:-}" ] && up_left_in=1
        [ -n "${region_set[$((r-1)),$((c+1))]:-}" ] && up_right_in=1
        [ -n "${region_set[$((r+1)),$((c-1))]:-}" ] && down_left_in=1
        [ -n "${region_set[$((r+1)),$((c+1))]:-}" ] && down_right_in=1

        # Top-left corner
        if [ $up_in -eq 0 ] && [ $left_in -eq 0 ]; then
            ((corners++))  # convex
        elif [ $up_in -eq 1 ] && [ $left_in -eq 1 ] && [ $up_left_in -eq 0 ]; then
            ((corners++))  # concave
        fi

        # Top-right corner
        if [ $up_in -eq 0 ] && [ $right_in -eq 0 ]; then
            ((corners++))  # convex
        elif [ $up_in -eq 1 ] && [ $right_in -eq 1 ] && [ $up_right_in -eq 0 ]; then
            ((corners++))  # concave
        fi

        # Bottom-left corner
        if [ $down_in -eq 0 ] && [ $left_in -eq 0 ]; then
            ((corners++))  # convex
        elif [ $down_in -eq 1 ] && [ $left_in -eq 1 ] && [ $down_left_in -eq 0 ]; then
            ((corners++))  # concave
        fi

        # Bottom-right corner
        if [ $down_in -eq 0 ] && [ $right_in -eq 0 ]; then
            ((corners++))  # convex
        elif [ $down_in -eq 1 ] && [ $right_in -eq 1 ] && [ $down_right_in -eq 0 ]; then
            ((corners++))  # concave
        fi
    done

    echo "$corners"
}

# Part 1: Calculate total fencing cost (area * perimeter)
part1() {
    find_regions

    local total=0

    for region_str in "${regions_cells[@]}"; do
        local -a region_array=($region_str)
        local area=${#region_array[@]}
        local perimeter=$(calculate_perimeter "$region_str")
        ((total += area * perimeter))
    done

    echo "$total"
}

# Part 2: Calculate total fencing cost (area * sides)
part2() {
    find_regions

    local total=0

    for region_str in "${regions_cells[@]}"; do
        local -a region_array=($region_str)
        local area=${#region_array[@]}
        local sides=$(count_corners "$region_str")
        ((total += area * sides))
    done

    echo "$total"
}

# Main execution
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
