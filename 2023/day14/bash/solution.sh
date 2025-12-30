#!/usr/bin/env bash

# Advent of Code 2023 Day 14: Parabolic Reflector Dish
# Requires bash 4+ for associative arrays
# Optimized using 1D array for grid

set -euo pipefail

# Read input into lines
mapfile -t lines < "../input.txt"

rows=${#lines[@]}
cols=${#lines[0]}

# Store grid as 1D array for faster access
# Index = row * cols + col
declare -a grid

load_grid() {
    local r c
    for ((r = 0; r < rows; r++)); do
        local line="${lines[r]}"
        for ((c = 0; c < cols; c++)); do
            grid[$((r * cols + c))]="${line:c:1}"
        done
    done
}

# Tilt north
tilt_north() {
    local r c dest idx dest_idx
    for ((c = 0; c < cols; c++)); do
        dest=0
        for ((r = 0; r < rows; r++)); do
            idx=$((r * cols + c))
            case "${grid[idx]}" in
                O)
                    if ((r != dest)); then
                        grid[idx]="."
                        dest_idx=$((dest * cols + c))
                        grid[dest_idx]="O"
                    fi
                    dest=$((dest + 1))
                    ;;
                '#')
                    dest=$((r + 1))
                    ;;
            esac
        done
    done
}

# Tilt south
tilt_south() {
    local r c dest idx dest_idx
    for ((c = 0; c < cols; c++)); do
        dest=$((rows - 1))
        for ((r = rows - 1; r >= 0; r--)); do
            idx=$((r * cols + c))
            case "${grid[idx]}" in
                O)
                    if ((r != dest)); then
                        grid[idx]="."
                        dest_idx=$((dest * cols + c))
                        grid[dest_idx]="O"
                    fi
                    dest=$((dest - 1))
                    ;;
                '#')
                    dest=$((r - 1))
                    ;;
            esac
        done
    done
}

# Tilt west
tilt_west() {
    local r c dest idx dest_idx
    for ((r = 0; r < rows; r++)); do
        dest=0
        for ((c = 0; c < cols; c++)); do
            idx=$((r * cols + c))
            case "${grid[idx]}" in
                O)
                    if ((c != dest)); then
                        grid[idx]="."
                        dest_idx=$((r * cols + dest))
                        grid[dest_idx]="O"
                    fi
                    dest=$((dest + 1))
                    ;;
                '#')
                    dest=$((c + 1))
                    ;;
            esac
        done
    done
}

# Tilt east
tilt_east() {
    local r c dest idx dest_idx
    for ((r = 0; r < rows; r++)); do
        dest=$((cols - 1))
        for ((c = cols - 1; c >= 0; c--)); do
            idx=$((r * cols + c))
            case "${grid[idx]}" in
                O)
                    if ((c != dest)); then
                        grid[idx]="."
                        dest_idx=$((r * cols + dest))
                        grid[dest_idx]="O"
                    fi
                    dest=$((dest - 1))
                    ;;
                '#')
                    dest=$((c - 1))
                    ;;
            esac
        done
    done
}

# Calculate total load
calc_load() {
    local total=0 r c idx
    for ((r = 0; r < rows; r++)); do
        for ((c = 0; c < cols; c++)); do
            idx=$((r * cols + c))
            if [[ "${grid[idx]}" == "O" ]]; then
                total=$((total + rows - r))
            fi
        done
    done
    echo "$total"
}

# Get grid state as string for cycle detection
grid_state() {
    local IFS=''
    echo "${grid[*]}"
}

# One spin cycle: N, W, S, E
spin_cycle() {
    tilt_north
    tilt_west
    tilt_south
    tilt_east
}

# Part 1
load_grid
tilt_north
part1=$(calc_load)
echo "Part 1: $part1"

# Part 2: Reset grid and run cycles with cycle detection
load_grid

declare -A seen
target=1000000000
cycle_num=0

while true; do
    state=$(grid_state)
    if [[ -v "seen[$state]" ]]; then
        cycle_start=${seen[$state]}
        cycle_length=$((cycle_num - cycle_start))
        remaining=$(( (target - cycle_num) % cycle_length ))
        for ((i = 0; i < remaining; i++)); do
            spin_cycle
        done
        break
    fi
    seen["$state"]=$cycle_num
    spin_cycle
    cycle_num=$((cycle_num + 1))
done

part2=$(calc_load)
echo "Part 2: $part2"
