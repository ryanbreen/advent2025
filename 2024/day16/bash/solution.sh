#!/usr/bin/env bash
# Day 16: Reindeer Maze - Weighted shortest path with turn costs
# Note: This Bash implementation is extremely slow (~10+ minutes) due to
# the interpreted nature of Bash and the complexity of Dijkstra's algorithm.

# Directions: 0=East, 1=South, 2=West, 3=North
declare -a DX=(1 0 -1 0)
declare -a DY=(0 1 0 -1)

# Parse the maze
declare -A grid
declare -a start end
height=0
width=0

parse_input() {
    local input_file="$1"
    local y=0
    while IFS= read -r line; do
        [[ -z "$line" ]] && continue
        width=${#line}
        for ((x=0; x<width; x++)); do
            local cell="${line:$x:1}"
            grid[$y,$x]="$cell"

            if [[ "$cell" == "S" ]]; then
                start=("$x" "$y")
            elif [[ "$cell" == "E" ]]; then
                end=("$x" "$y")
            fi
        done
        y=$((y + 1))
    done < "$input_file"
    height=$y
}

# Simple priority queue using array (slow but simple)
declare -a pq

pq_reset() {
    pq=()
}

pq_push() {
    # Format: "cost x y d"
    pq+=("$1")
}

pq_pop_min() {
    [[ ${#pq[@]} -eq 0 ]] && return 1

    # Find minimum
    local min_idx=0
    local min_cost
    read -r min_cost _ _ _ <<< "${pq[0]}"

    for ((i=1; i<${#pq[@]}; i++)); do
        local cost
        read -r cost _ _ _ <<< "${pq[$i]}"
        if ((cost < min_cost)); then
            min_cost=$cost
            min_idx=$i
        fi
    done

    # Output and remove
    echo "${pq[$min_idx]}"
    unset 'pq[$min_idx]'
    pq=("${pq[@]}")
}

# Dijkstra from start
dijkstra_forward() {
    local start_x=${start[0]}
    local start_y=${start[1]}

    pq_reset
    pq_push "0 $start_x $start_y 0"

    declare -gA dist_from_start

    local processed=0
    while [[ ${#pq[@]} -gt 0 ]]; do
        local item cost x y d
        item=$(pq_pop_min) || break
        read -r cost x y d <<< "$item"

        local key="$x,$y,$d"
        [[ -v dist_from_start[$key] ]] && continue
        dist_from_start[$key]=$cost

        processed=$((processed + 1))
        if ((processed % 100 == 0)); then
            echo "Forward: $processed states, queue: ${#pq[@]}" >&2
        fi

        # Move forward
        local nx=$((x + DX[d]))
        local ny=$((y + DY[d]))
        if ((nx >= 0 && nx < width && ny >= 0 && ny < height)); then
            if [[ "${grid[$ny,$nx]}" != "#" ]]; then
                pq_push "$((cost + 1)) $nx $ny $d"
            fi
        fi

        # Turns
        pq_push "$((cost + 1000)) $x $y $(((d + 3) % 4))"
        pq_push "$((cost + 1000)) $x $y $(((d + 1) % 4))"
    done
}

# Dijkstra backward
dijkstra_backward() {
    local end_x=${end[0]}
    local end_y=${end[1]}

    pq_reset
    for d in 0 1 2 3; do
        pq_push "0 $end_x $end_y $d"
    done

    declare -gA dist_to_end

    local processed=0
    while [[ ${#pq[@]} -gt 0 ]]; do
        local item cost x y d
        item=$(pq_pop_min) || break
        read -r cost x y d <<< "$item"

        local key="$x,$y,$d"
        [[ -v dist_to_end[$key] ]] && continue
        dist_to_end[$key]=$cost

        processed=$((processed + 1))
        if ((processed % 100 == 0)); then
            echo "Backward: $processed states, queue: ${#pq[@]}" >&2
        fi

        # Reverse move
        local px=$((x - DX[d]))
        local py=$((y - DY[d]))
        if ((px >= 0 && px < width && py >= 0 && py < height)); then
            if [[ "${grid[$py,$px]}" != "#" ]]; then
                pq_push "$((cost + 1)) $px $py $d"
            fi
        fi

        # Turns
        pq_push "$((cost + 1000)) $x $y $(((d + 3) % 4))"
        pq_push "$((cost + 1000)) $x $y $(((d + 1) % 4))"
    done
}

# Part 1
part1() {
    local min_cost=999999999
    for d in 0 1 2 3; do
        local key="${end[0]},${end[1]},$d"
        if [[ -v dist_from_start[$key] ]]; then
            local cost=${dist_from_start[$key]}
            ((cost < min_cost)) && min_cost=$cost
        fi
    done
    echo "$min_cost"
}

# Part 2
part2() {
    local best=$1
    declare -A on_path

    for ((y=0; y<height; y++)); do
        for ((x=0; x<width; x++)); do
            [[ "${grid[$y,$x]}" == "#" ]] && continue

            for d in 0 1 2 3; do
                local key="$x,$y,$d"
                local fs=999999999
                local te=999999999

                [[ -v dist_from_start[$key] ]] && fs=${dist_from_start[$key]}
                [[ -v dist_to_end[$key] ]] && te=${dist_to_end[$key]}

                if ((fs + te == best)); then
                    on_path[$y,$x]=1
                    break
                fi
            done
        done
    done

    echo "${#on_path[@]}"
}

main() {
    local input_file
    input_file="$(dirname "$0")/../input.txt"

    echo "Parsing..." >&2
    parse_input "$input_file"
    echo "Grid: ${height}x${width}" >&2

    echo "Running Dijkstra forward (this may take 5-10 minutes in Bash)..." >&2
    dijkstra_forward
    echo "Done. States: ${#dist_from_start[@]}" >&2

    local answer1
    answer1=$(part1)
    echo "Part 1: $answer1"

    echo "Running Dijkstra backward..." >&2
    dijkstra_backward
    echo "Done. States: ${#dist_to_end[@]}" >&2

    local answer2
    answer2=$(part2 "$answer1")
    echo "Part 2: $answer2"
}

main
