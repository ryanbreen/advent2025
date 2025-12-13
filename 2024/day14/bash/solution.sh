#!/bin/bash

WIDTH=101
HEIGHT=103
MID_X=$((WIDTH / 2))   # 50
MID_Y=$((HEIGHT / 2))  # 51

# Read input file
input_file="../input.txt"

# Parse robots into arrays
declare -a px py vx vy
robot_count=0

while IFS= read -r line; do
    if [[ $line =~ p=(-?[0-9]+),(-?[0-9]+)\ v=(-?[0-9]+),(-?[0-9]+) ]]; then
        px[$robot_count]=${BASH_REMATCH[1]}
        py[$robot_count]=${BASH_REMATCH[2]}
        vx[$robot_count]=${BASH_REMATCH[3]}
        vy[$robot_count]=${BASH_REMATCH[4]}
        ((robot_count++))
    fi
done < "$input_file"

# Part 1: Simulate 100 seconds
simulate_100() {
    local q1=0 q2=0 q3=0 q4=0
    local seconds=100

    for ((i=0; i<robot_count; i++)); do
        # Calculate new position with wrapping
        local new_x=$(( (px[i] + vx[i] * seconds) % WIDTH ))
        local new_y=$(( (py[i] + vy[i] * seconds) % HEIGHT ))

        # Handle negative modulo
        if [ $new_x -lt 0 ]; then
            new_x=$(( (new_x + WIDTH) % WIDTH ))
        fi
        if [ $new_y -lt 0 ]; then
            new_y=$(( (new_y + HEIGHT) % HEIGHT ))
        fi

        # Skip robots on middle lines
        if [ $new_x -eq $MID_X ] || [ $new_y -eq $MID_Y ]; then
            continue
        fi

        # Count quadrants
        if [ $new_x -lt $MID_X ] && [ $new_y -lt $MID_Y ]; then
            ((q1++))
        elif [ $new_x -gt $MID_X ] && [ $new_y -lt $MID_Y ]; then
            ((q2++))
        elif [ $new_x -lt $MID_X ] && [ $new_y -gt $MID_Y ]; then
            ((q3++))
        else
            ((q4++))
        fi
    done

    echo $((q1 * q2 * q3 * q4))
}

# Part 2: Find Christmas tree pattern
find_tree() {
    local max_iterations=$((WIDTH * HEIGHT))

    for ((seconds=1; seconds<=max_iterations; seconds++)); do
        # Calculate all positions and store as "y x" pairs for sorting
        local positions_str=""

        for ((i=0; i<robot_count; i++)); do
            local new_x=$(( (px[i] + vx[i] * seconds) % WIDTH ))
            local new_y=$(( (py[i] + vy[i] * seconds) % HEIGHT ))

            # Handle negative modulo
            if [ $new_x -lt 0 ]; then
                new_x=$(( (new_x + WIDTH) % WIDTH ))
            fi
            if [ $new_y -lt 0 ]; then
                new_y=$(( (new_y + HEIGHT) % HEIGHT ))
            fi

            positions_str+="$new_y $new_x"$'\n'
        done

        # Sort by y, then x
        local sorted_positions=$(echo "$positions_str" | sort -n)

        # Check for 20 consecutive robots in same row
        local prev_y=-1
        local prev_x=-1
        local consecutive=0
        local max_consecutive=0

        while IFS=' ' read -r y x; do
            if [ "$y" = "$prev_y" ] && [ $x -eq $((prev_x + 1)) ]; then
                ((consecutive++))
            elif [ "$y" = "$prev_y" ]; then
                # Same row but not consecutive
                consecutive=1
            else
                # New row
                consecutive=1
            fi

            if [ $consecutive -gt $max_consecutive ]; then
                max_consecutive=$consecutive
            fi

            prev_y=$y
            prev_x=$x
        done <<< "$sorted_positions"

        if [ $max_consecutive -ge 20 ]; then
            echo $seconds
            return
        fi
    done

    echo -1
}

# Run both parts
part1=$(simulate_100)
part2=$(find_tree)

echo "Part 1: $part1"
echo "Part 2: $part2"
