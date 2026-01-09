#!/usr/bin/env bash

set -euo pipefail

# Day 12: Hill Climbing Algorithm
# BFS shortest path in a grid

cd "$(dirname "$0")"

# Read grid into array
mapfile -t grid < ../input.txt

rows=${#grid[@]}
cols=${#grid[0]}

# Find start (S), end (E), and convert to heights
declare -a heights
start_r=0
start_c=0
end_r=0
end_c=0
declare -a all_a_positions

for ((r = 0; r < rows; r++)); do
    line="${grid[r]}"
    for ((c = 0; c < cols; c++)); do
        ch="${line:c:1}"
        if [[ "$ch" == "S" ]]; then
            start_r=$r
            start_c=$c
            # S has elevation 'a'
            heights[$((r * cols + c))]=$(printf '%d' "'a")
            all_a_positions+=("$r,$c")
        elif [[ "$ch" == "E" ]]; then
            end_r=$r
            end_c=$c
            # E has elevation 'z'
            heights[$((r * cols + c))]=$(printf '%d' "'z")
        else
            heights[$((r * cols + c))]=$(printf '%d' "'$ch")
            if [[ "$ch" == "a" ]]; then
                all_a_positions+=("$r,$c")
            fi
        fi
    done
done

# BFS function
# Arguments: list of start positions (comma-separated r,c pairs separated by space)
bfs() {
    local starts=("$@")

    declare -A visited

    # Queue format: "r,c,dist"
    local queue=()

    # Add all starts to queue
    for start in "${starts[@]}"; do
        queue+=("$start,0")
        visited["$start"]=1
    done

    local head=0

    while [[ $head -lt ${#queue[@]} ]]; do
        local item="${queue[$head]}"
        ((head++))

        local r="${item%%,*}"
        local rest="${item#*,}"
        local c="${rest%%,*}"
        local dist="${rest##*,}"

        # Check if we reached the end
        if [[ $r -eq $end_r && $c -eq $end_c ]]; then
            echo "$dist"
            return
        fi

        local current_height="${heights[$((r * cols + c))]}"

        # Explore neighbors: up, down, left, right
        local -a dirs=("-1,0" "1,0" "0,-1" "0,1")
        for dir in "${dirs[@]}"; do
            local dr="${dir%%,*}"
            local dc="${dir##*,}"
            local nr=$((r + dr))
            local nc=$((c + dc))

            # Check bounds
            if [[ $nr -ge 0 && $nr -lt $rows && $nc -ge 0 && $nc -lt $cols ]]; then
                local key="$nr,$nc"
                if [[ -z "${visited[$key]:-}" ]]; then
                    local next_height="${heights[$((nr * cols + nc))]}"
                    # Can move if destination is at most 1 higher
                    if [[ $next_height -le $((current_height + 1)) ]]; then
                        visited["$key"]=1
                        queue+=("$nr,$nc,$((dist + 1))")
                    fi
                fi
            fi
        done
    done

    echo "-1"
}

# Part 1: Find shortest path from S to E
part1=$(bfs "$start_r,$start_c")
echo "Part 1: $part1"

# Part 2: Find shortest path from any 'a' to E
part2=$(bfs "${all_a_positions[@]}")
echo "Part 2: $part2"
