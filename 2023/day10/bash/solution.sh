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
for ((r = 0; r < rows; r++)); do
    row="${grid[r]}"
    for ((c = 0; c < cols; c++)); do
        if [[ "${row:c:1}" == "S" ]]; then
            start_r=$r
            start_c=$c
            break 2
        fi
    done
done

# Associative arrays for distances and loop membership
declare -A distances
declare -A in_loop

# Direction definitions: name -> "dr,dc"
declare -A DIRECTIONS=(
    [north]="-1,0"
    [south]="1,0"
    [west]="0,-1"
    [east]="0,1"
)

# Check if a pipe connects in a given direction
# Returns 0 (true) if pipe at (r,c) has connection in direction (dr,dc)
has_connection() {
    local r=$1 c=$2 dr=$3 dc=$4
    local ch="${grid[r]:c:1}"

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
    has_connection "$nr" "$nc" "$((r - nr))" "$((c - nc))"
}

# Get neighbors for BFS (uses nameref to avoid subshell overhead)
# For S: find adjacent pipes that connect back to S
# For pipes: follow the pipe's connections
get_neighbors() {
    local r=$1 c=$2
    local -n result_ref=$3
    local ch="${grid[r]:c:1}"
    local dirs=()

    if [[ "$ch" == "S" ]]; then
        dirs=("-1,0" "1,0" "0,-1" "0,1")
    else
        case "$ch" in
            '|') dirs=("-1,0" "1,0") ;;
            '-') dirs=("0,-1" "0,1") ;;
            'L') dirs=("-1,0" "0,1") ;;
            'J') dirs=("-1,0" "0,-1") ;;
            '7') dirs=("1,0" "0,-1") ;;
            'F') dirs=("1,0" "0,1") ;;
        esac
    fi

    local dir dr dc nr nc
    for dir in "${dirs[@]}"; do
        dr=${dir%,*}
        dc=${dir#*,}
        nr=$((r + dr))
        nc=$((c + dc))
        if ((nr >= 0 && nr < rows && nc >= 0 && nc < cols)); then
            if [[ "$ch" == "S" ]]; then
                # For S, only add neighbors that connect back
                connects_back "$r" "$c" "$nr" "$nc" && result_ref+=("$nr,$nc")
            else
                result_ref+=("$nr,$nc")
            fi
        fi
    done
}

# BFS to find the main loop
bfs_find_loop() {
    local queue=("$start_r,$start_c")
    local head=0
    local pos r c dist neighbor
    local neighbors=()

    distances["$start_r,$start_c"]=0
    in_loop["$start_r,$start_c"]=1

    while ((head < ${#queue[@]})); do
        pos="${queue[head]}"
        ((head++))

        r=${pos%,*}
        c=${pos#*,}
        dist=${distances["$pos"]}

        neighbors=()
        get_neighbors "$r" "$c" neighbors

        for neighbor in "${neighbors[@]}"; do
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

    local max_dist=0 pos d
    for pos in "${!distances[@]}"; do
        d=${distances[$pos]}
        ((d > max_dist)) && max_dist=$d
    done
    echo "$max_dist"
}

# Determine what pipe S actually is
determine_start_pipe() {
    local -A connections=()
    local dir_name dir_val dr dc nr nc key

    # Check each direction for connections
    for dir_name in "${!DIRECTIONS[@]}"; do
        dir_val="${DIRECTIONS[$dir_name]}"
        dr=${dir_val%,*}
        dc=${dir_val#*,}
        nr=$((start_r + dr))
        nc=$((start_c + dc))

        # Bounds check and loop membership check
        if ((nr >= 0 && nr < rows && nc >= 0 && nc < cols)); then
            key="$nr,$nc"
            if [[ -n "${in_loop[$key]}" ]]; then
                if connects_back "$start_r" "$start_c" "$nr" "$nc"; then
                    connections[$dir_name]=1
                fi
            fi
        fi
    done

    # Determine pipe type based on which directions connect
    if [[ -n "${connections[north]}" && -n "${connections[south]}" ]]; then echo '|'
    elif [[ -n "${connections[east]}" && -n "${connections[west]}" ]]; then echo '-'
    elif [[ -n "${connections[north]}" && -n "${connections[east]}" ]]; then echo 'L'
    elif [[ -n "${connections[north]}" && -n "${connections[west]}" ]]; then echo 'J'
    elif [[ -n "${connections[south]}" && -n "${connections[west]}" ]]; then echo '7'
    elif [[ -n "${connections[south]}" && -n "${connections[east]}" ]]; then echo 'F'
    else echo 'S'
    fi
}

# Part 2: Count enclosed tiles using ray casting
part2() {
    # BFS should already be done from part1, but call it if not
    if [[ ${#distances[@]} -eq 0 ]]; then
        bfs_find_loop
    fi

    # Determine what S actually is and replace it in grid
    local start_pipe row
    start_pipe=$(determine_start_pipe)
    row="${grid[start_r]}"
    grid[start_r]="${row:0:start_c}${start_pipe}${row:start_c+1}"

    local enclosed=0 inside key ch

    for ((r = 0; r < rows; r++)); do
        inside=0
        row="${grid[r]}"

        for ((c = 0; c < cols; c++)); do
            key="$r,$c"
            ch="${row:c:1}"

            if [[ -n "${in_loop[$key]}" ]]; then
                # Count pipes with north connection: |, L, J
                if [[ "$ch" == "|" || "$ch" == "L" || "$ch" == "J" ]]; then
                    inside=$((1 - inside))
                fi
            elif ((inside)); then
                ((enclosed++))
            fi
        done
    done

    echo "$enclosed"
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
