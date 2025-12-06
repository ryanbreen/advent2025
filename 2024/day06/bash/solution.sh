#!/bin/bash

INPUT_FILE="../input.txt"

# Read the map into an array
map=()
while IFS= read -r line; do
    map+=("$line")
done < "$INPUT_FILE"

# Get map dimensions
height=${#map[@]}
width=${#map[0]}

# Find starting position and direction
start_row=-1
start_col=-1
start_dir=""

for ((row=0; row<height; row++)); do
    line="${map[$row]}"
    for ((col=0; col<width; col++)); do
        char="${line:$col:1}"
        if [[ "$char" == "^" || "$char" == "v" || "$char" == "<" || "$char" == ">" ]]; then
            start_row=$row
            start_col=$col
            start_dir="$char"
            break 2
        fi
    done
done

# Direction vectors: up, right, down, left
# We'll use numeric directions: 0=up, 1=right, 2=down, 3=left
case "$start_dir" in
    "^") start_dir_num=0 ;;
    ">") start_dir_num=1 ;;
    "v") start_dir_num=2 ;;
    "<") start_dir_num=3 ;;
esac

# Direction deltas
dr=(-1 0 1 0)
dc=(0 1 0 -1)

# Part 1: Simulate normal patrol and collect visited positions
positions_file=$(mktemp)

row=$start_row
col=$start_col
dir=$start_dir_num

while true; do
    # Mark current position
    echo "$row,$col" >> "$positions_file"

    # Calculate next position
    next_row=$((row + dr[dir]))
    next_col=$((col + dc[dir]))

    # Check if next position is out of bounds
    if ((next_row < 0 || next_row >= height || next_col < 0 || next_col >= width)); then
        break
    fi

    # Check if next position has an obstacle
    next_char="${map[$next_row]:$next_col:1}"

    if [[ "$next_char" == "#" ]]; then
        # Turn right 90 degrees
        dir=$(((dir + 1) % 4))
    else
        # Move forward
        row=$next_row
        col=$next_col
    fi
done

# Get unique positions
sort -u "$positions_file" > "${positions_file}.unique"
count=$(wc -l < "${positions_file}.unique" | tr -d ' ')
echo "Part 1: $count"

# Part 2: Try placing obstruction at each visited position
loop_count=0

while IFS=',' read -r obs_row obs_col; do
    # Can't place obstruction at starting position
    if [[ $obs_row -eq $start_row && $obs_col -eq $start_col ]]; then
        continue
    fi

    # Simulate with obstruction - track states with a temporary file
    states_file=$(mktemp)
    row=$start_row
    col=$start_col
    dir=$start_dir_num
    is_loop=0

    # Limit total iterations as a safety net
    max_iterations=$((height * width * 5))
    iterations=0

    while ((iterations < max_iterations)); do
        ((iterations++))

        # Calculate next position
        next_row=$((row + dr[dir]))
        next_col=$((col + dc[dir]))

        # Check if next position is out of bounds
        if ((next_row < 0 || next_row >= height || next_col < 0 || next_col >= width)); then
            # Guard exits - not a loop
            break
        fi

        # Check if next position has an obstacle
        next_char="${map[$next_row]:$next_col:1}"

        # Check for temporary obstruction
        if [[ $next_row -eq $obs_row && $next_col -eq $obs_col ]]; then
            next_char="#"
        fi

        if [[ "$next_char" == "#" ]]; then
            # Turn right 90 degrees - check state after turn
            dir=$(((dir + 1) % 4))

            # After a turn, check if we've been in this exact state before
            state="$row,$col,$dir"
            if grep -Fxq "$state" "$states_file" 2>/dev/null; then
                # Loop detected
                is_loop=1
                break
            fi
            echo "$state" >> "$states_file"
        else
            # Move forward
            row=$next_row
            col=$next_col
        fi
    done

    rm "$states_file"

    if ((is_loop)); then
        ((loop_count++))
    fi
done < "${positions_file}.unique"

echo "Part 2: $loop_count"

# Cleanup
rm "$positions_file" "${positions_file}.unique"
