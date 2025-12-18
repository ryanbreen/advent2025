#!/usr/bin/env bash
set -euo pipefail

# Day 18: RAM Run - BFS pathfinding with binary search

declare -a POSITIONS_X
declare -a POSITIONS_Y
declare -A CORRUPTED
SIZE=71
GOAL=$((SIZE - 1))

# Parse input file
parse_input() {
    local i=0
    while IFS=',' read -r x y || [[ -n "$x" ]]; do
        [[ -z "$x" ]] && continue
        POSITIONS_X[i]=$x
        POSITIONS_Y[i]=$y
        ((i++)) || true
    done < "../input.txt"
}

# BFS to find shortest path from (0,0) to (SIZE-1,SIZE-1)
# Returns -1 if no path exists
bfs() {
    local -A visited
    local -a queue_x queue_y queue_steps
    local head=0 tail=0
    local x y steps nx ny

    # Check if start or goal is corrupted
    if [[ -n "${CORRUPTED[0,0]:-}" ]] || [[ -n "${CORRUPTED[$GOAL,$GOAL]:-}" ]]; then
        echo -1
        return
    fi

    # Initialize queue with start position
    queue_x[tail]=0
    queue_y[tail]=0
    queue_steps[tail]=0
    ((tail++)) || true
    visited[0,0]=1

    # Directions: up, down, left, right
    local -a dx=( 0 0 -1 1 )
    local -a dy=( -1 1 0 0 )

    while (( head < tail )); do
        x=${queue_x[head]}
        y=${queue_y[head]}
        steps=${queue_steps[head]}
        ((head++)) || true

        # Check if we reached the goal
        if (( x == GOAL && y == GOAL )); then
            echo "$steps"
            return
        fi

        # Explore neighbors
        for i in 0 1 2 3; do
            nx=$((x + dx[i]))
            ny=$((y + dy[i]))

            # Check bounds
            (( nx < 0 || nx >= SIZE || ny < 0 || ny >= SIZE )) && continue

            # Check if already visited
            [[ -n "${visited[$nx,$ny]:-}" ]] && continue

            # Check if corrupted
            [[ -n "${CORRUPTED[$nx,$ny]:-}" ]] && continue

            visited[$nx,$ny]=1
            queue_x[tail]=$nx
            queue_y[tail]=$ny
            queue_steps[tail]=$((steps + 1))
            ((tail++)) || true
        done
    done

    echo -1
}

# Set up corrupted cells for first n bytes
setup_corrupted() {
    local n=$1
    CORRUPTED=()
    local i
    for (( i = 0; i < n; i++ )); do
        CORRUPTED[${POSITIONS_X[i]},${POSITIONS_Y[i]}]=1
    done
}

# Part 1: BFS after first 1024 bytes
part1() {
    setup_corrupted 1024
    bfs
}

# Part 2: Binary search to find first blocking byte
part2() {
    local num_positions=${#POSITIONS_X[@]}
    local left=0
    local right=$num_positions
    local mid result

    while (( left < right )); do
        mid=$(( (left + right) / 2 ))
        setup_corrupted $((mid + 1))
        result=$(bfs)
        if (( result == -1 )); then
            right=$mid
        else
            left=$((mid + 1))
        fi
    done

    echo "${POSITIONS_X[left]},${POSITIONS_Y[left]}"
}

# Main
parse_input

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
