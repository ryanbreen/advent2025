#!/bin/bash

# Day 10: Pipe Maze
# Read the pipe grid, find the main loop via BFS, then count enclosed tiles

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Read grid into array
mapfile -t grid < "$INPUT_FILE"
rows=${#grid[@]}
cols=${#grid[0]}

# Find start position
start_r=0
start_c=0
for ((r=0; r<rows; r++)); do
    row="${grid[r]}"
    for ((c=0; c<cols; c++)); do
        ch="${row:c:1}"
        if [[ "$ch" == "S" ]]; then
            start_r=$r
            start_c=$c
            break 2
        fi
    done
done

# Associative arrays for distances and loop membership
declare -A distances
declare -A in_loop

# Get pipe character at position
get_char() {
    local r=$1 c=$2
    echo "${grid[r]:c:1}"
}

# Check if a pipe connects in a given direction
# Returns 0 (true) if pipe at (r,c) has connection in direction (dr,dc)
has_connection() {
    local r=$1 c=$2 dr=$3 dc=$4
    local ch
    ch=$(get_char "$r" "$c")

    case "$ch" in
        '|') [[ ($dr == -1 && $dc == 0) || ($dr == 1 && $dc == 0) ]] ;;
        '-') [[ ($dr == 0 && $dc == -1) || ($dr == 0 && $dc == 1) ]] ;;
        'L') [[ ($dr == -1 && $dc == 0) || ($dr == 0 && $dc == 1) ]] ;;
        'J') [[ ($dr == -1 && $dc == 0) || ($dr == 0 && $dc == -1) ]] ;;
        '7') [[ ($dr == 1 && $dc == 0) || ($dr == 0 && $dc == -1) ]] ;;
        'F') [[ ($dr == 1 && $dc == 0) || ($dr == 0 && $dc == 1) ]] ;;
        'S') return 0 ;;  # S connects everywhere for initial search
        *) return 1 ;;
    esac
}

# Check if position (nr,nc) connects back to (r,c)
connects_back() {
    local r=$1 c=$2 nr=$3 nc=$4
    local dr=$((r - nr))
    local dc=$((c - nc))
    has_connection "$nr" "$nc" "$dr" "$dc"
}

# Get neighbors for BFS
# For S: find adjacent pipes that connect back to S
# For pipes: follow the pipe's connections
get_neighbors() {
    local r=$1 c=$2
    local ch
    ch=$(get_char "$r" "$c")
    local result=()

    if [[ "$ch" == "S" ]]; then
        # Check all 4 directions for pipes connecting back
        for dir in "-1,0" "1,0" "0,-1" "0,1"; do
            local dr=${dir%,*}
            local dc=${dir#*,}
            local nr=$((r + dr))
            local nc=$((c + dc))
            if ((nr >= 0 && nr < rows && nc >= 0 && nc < cols)); then
                if connects_back "$r" "$c" "$nr" "$nc"; then
                    result+=("$nr,$nc")
                fi
            fi
        done
    else
        # Get directions based on pipe type
        local dirs=()
        case "$ch" in
            '|') dirs=("-1,0" "1,0") ;;
            '-') dirs=("0,-1" "0,1") ;;
            'L') dirs=("-1,0" "0,1") ;;
            'J') dirs=("-1,0" "0,-1") ;;
            '7') dirs=("1,0" "0,-1") ;;
            'F') dirs=("1,0" "0,1") ;;
        esac

        for dir in "${dirs[@]}"; do
            local dr=${dir%,*}
            local dc=${dir#*,}
            local nr=$((r + dr))
            local nc=$((c + dc))
            if ((nr >= 0 && nr < rows && nc >= 0 && nc < cols)); then
                result+=("$nr,$nc")
            fi
        done
    fi

    echo "${result[@]}"
}

# BFS to find the main loop
bfs_find_loop() {
    local queue=()
    local head=0

    distances["$start_r,$start_c"]=0
    in_loop["$start_r,$start_c"]=1
    queue+=("$start_r,$start_c")

    while ((head < ${#queue[@]})); do
        local pos="${queue[head]}"
        ((head++))

        local r=${pos%,*}
        local c=${pos#*,}
        local dist=${distances["$pos"]}

        for neighbor in $(get_neighbors "$r" "$c"); do
            if [[ -z "${distances[$neighbor]}" ]]; then
                distances["$neighbor"]=$((dist + 1))
                in_loop["$neighbor"]=1
                queue+=("$neighbor")
            fi
        done
    done
}

# Part 1: Find maximum distance in loop
part1() {
    bfs_find_loop

    local max_dist=0
    for pos in "${!distances[@]}"; do
        local d=${distances[$pos]}
        ((d > max_dist)) && max_dist=$d
    done
    echo "$max_dist"
}

# Determine what pipe S actually is
determine_start_pipe() {
    local has_north=0 has_south=0 has_east=0 has_west=0

    # Check north
    local nr=$((start_r - 1))
    if ((nr >= 0)); then
        local key="$nr,$start_c"
        if [[ -n "${in_loop[$key]}" ]]; then
            if connects_back "$start_r" "$start_c" "$nr" "$start_c"; then
                has_north=1
            fi
        fi
    fi

    # Check south
    nr=$((start_r + 1))
    if ((nr < rows)); then
        local key="$nr,$start_c"
        if [[ -n "${in_loop[$key]}" ]]; then
            if connects_back "$start_r" "$start_c" "$nr" "$start_c"; then
                has_south=1
            fi
        fi
    fi

    # Check west
    local nc=$((start_c - 1))
    if ((nc >= 0)); then
        local key="$start_r,$nc"
        if [[ -n "${in_loop[$key]}" ]]; then
            if connects_back "$start_r" "$start_c" "$start_r" "$nc"; then
                has_west=1
            fi
        fi
    fi

    # Check east
    nc=$((start_c + 1))
    if ((nc < cols)); then
        local key="$start_r,$nc"
        if [[ -n "${in_loop[$key]}" ]]; then
            if connects_back "$start_r" "$start_c" "$start_r" "$nc"; then
                has_east=1
            fi
        fi
    fi

    # Determine pipe type
    if ((has_north && has_south)); then echo '|'
    elif ((has_east && has_west)); then echo '-'
    elif ((has_north && has_east)); then echo 'L'
    elif ((has_north && has_west)); then echo 'J'
    elif ((has_south && has_west)); then echo '7'
    elif ((has_south && has_east)); then echo 'F'
    else echo 'S'
    fi
}

# Part 2: Count enclosed tiles using ray casting
part2() {
    # BFS should already be done from part1, but call it if not
    if [[ ${#distances[@]} -eq 0 ]]; then
        bfs_find_loop
    fi

    # Determine what S actually is
    local start_pipe
    start_pipe=$(determine_start_pipe)

    # Replace S in grid
    local row="${grid[start_r]}"
    grid[start_r]="${row:0:start_c}${start_pipe}${row:start_c+1}"

    local enclosed=0

    for ((r=0; r<rows; r++)); do
        local inside=0
        local row="${grid[r]}"

        for ((c=0; c<cols; c++)); do
            local key="$r,$c"
            local ch="${row:c:1}"

            if [[ -n "${in_loop[$key]}" ]]; then
                # Count pipes with north connection: |, L, J
                if [[ "$ch" == "|" || "$ch" == "L" || "$ch" == "J" ]]; then
                    inside=$((1 - inside))
                fi
            else
                if ((inside)); then
                    ((enclosed++))
                fi
            fi
        done
    done

    echo "$enclosed"
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
