#!/usr/bin/env bash
# Day 16: Proboscidea Volcanium
# This is a hard problem for Bash - uses associative arrays and recursive DFS

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Declare associative arrays
declare -A flow_rate
declare -A tunnels
declare -A dist
declare -a valuable_valves
declare -A valve_index
declare -A memo

# Parse input
parse_input() {
    while IFS= read -r line; do
        # Parse: Valve XX has flow rate=N; tunnels lead to valves YY, ZZ
        valve=$(echo "$line" | sed 's/Valve \([A-Z]*\).*/\1/')
        rate=$(echo "$line" | sed 's/.*flow rate=\([0-9]*\).*/\1/')
        # Extract neighbors after "valve " or "valves "
        neighbors=$(echo "$line" | sed -E 's/.*(valves?|valve) //' | sed 's/,//g')

        flow_rate[$valve]=$rate
        tunnels[$valve]="$neighbors"
    done < "$INPUT_FILE"
}

# BFS to compute distances from a starting valve to all other valves
bfs_distances() {
    local start=$1
    declare -A visited
    declare -a queue
    declare -a distances_q

    queue=("$start")
    distances_q=(0)
    visited[$start]=1

    while [ ${#queue[@]} -gt 0 ]; do
        local curr=${queue[0]}
        local d=${distances_q[0]}
        queue=("${queue[@]:1}")
        distances_q=("${distances_q[@]:1}")

        # Store distance
        dist["$start,$curr"]=$d

        # Visit neighbors
        for neighbor in ${tunnels[$curr]}; do
            if [ -z "${visited[$neighbor]}" ]; then
                visited[$neighbor]=1
                queue+=("$neighbor")
                distances_q+=($((d + 1)))
            fi
        done
    done
}

# Compute distances between all relevant valves
compute_distances() {
    # Find all valuable valves (flow > 0)
    valuable_valves=()
    local idx=0

    for valve in "${!flow_rate[@]}"; do
        if [ "${flow_rate[$valve]}" -gt 0 ]; then
            valuable_valves+=("$valve")
            valve_index[$valve]=$idx
            ((idx++))
        fi
    done

    # Compute distances from AA
    bfs_distances "AA"

    # Compute distances from all valuable valves
    for valve in "${valuable_valves[@]}"; do
        bfs_distances "$valve"
    done
}

# DFS to find maximum pressure (Part 1)
# Arguments: position, time_left, opened_mask
# Returns result via global variable dfs_result
dfs_part1() {
    local pos=$1
    local time_left=$2
    local opened=$3

    if [ $time_left -le 0 ]; then
        dfs_result=0
        return
    fi

    # Check memo
    local key="$pos,$time_left,$opened"
    if [ -n "${memo[$key]}" ]; then
        dfs_result=${memo[$key]}
        return
    fi

    local best=0
    local n=${#valuable_valves[@]}
    local i

    for ((i=0; i<n; i++)); do
        local mask=$((1 << i))
        if [ $((opened & mask)) -eq 0 ]; then
            local next_valve=${valuable_valves[$i]}
            local time_cost=$((${dist["$pos,$next_valve"]} + 1))

            if [ $time_cost -lt $time_left ]; then
                local new_time=$((time_left - time_cost))
                local pressure=$((${flow_rate[$next_valve]} * new_time))

                dfs_part1 "$next_valve" $new_time $((opened | mask))
                local total=$((pressure + dfs_result))

                if [ $total -gt $best ]; then
                    best=$total
                fi
            fi
        fi
    done

    memo[$key]=$best
    dfs_result=$best
}

# DFS that computes max pressure for each final opened set
# Arguments: position, time_left, opened_mask, pressure_so_far
# Stores results in global max_pressure array
dfs_all_subsets() {
    local pos=$1
    local time_left=$2
    local opened=$3
    local pressure_so_far=$4

    # Record the maximum pressure for this opened set
    if [ -z "${max_pressure[$opened]}" ] || [ $pressure_so_far -gt ${max_pressure[$opened]} ]; then
        max_pressure[$opened]=$pressure_so_far
    fi

    if [ $time_left -le 0 ]; then
        return
    fi

    local n=${#valuable_valves[@]}
    local i

    for ((i=0; i<n; i++)); do
        local mask=$((1 << i))
        if [ $((opened & mask)) -eq 0 ]; then
            local next_valve=${valuable_valves[$i]}
            local time_cost=$((${dist["$pos,$next_valve"]} + 1))

            if [ $time_cost -lt $time_left ]; then
                local new_time=$((time_left - time_cost))
                local pressure=$((${flow_rate[$next_valve]} * new_time))

                dfs_all_subsets "$next_valve" $new_time $((opened | mask)) $((pressure_so_far + pressure))
            fi
        fi
    done
}

part1() {
    memo=()
    dfs_part1 "AA" 30 0
    echo $dfs_result
}

part2() {
    # Compute max pressure for all possible opened sets via single DFS
    declare -gA max_pressure
    max_pressure=()
    dfs_all_subsets "AA" 26 0 0

    # Find best partition where you and elephant open disjoint sets
    local best=0
    local my_set elephant_set my_score elephant_score

    for my_set in "${!max_pressure[@]}"; do
        my_score=${max_pressure[$my_set]}
        for elephant_set in "${!max_pressure[@]}"; do
            # Check if sets are disjoint
            if [ $((my_set & elephant_set)) -eq 0 ]; then
                elephant_score=${max_pressure[$elephant_set]}
                local total=$((my_score + elephant_score))
                if [ $total -gt $best ]; then
                    best=$total
                fi
            fi
        done
    done

    echo $best
}

# Main
parse_input
compute_distances

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
