#!/bin/bash

# Read input file
input_file="../input.txt"

# Starting position
pos=50
part1_count=0
part2_count=0

# Process each line
while IFS= read -r line; do
    # Skip empty lines
    [[ -z "$line" ]] && continue

    # Extract direction and distance
    dir="${line:0:1}"
    dist="${line:1}"

    # Calculate new position and count zero crossings
    if [[ "$dir" == "L" ]]; then
        new_pos=$(( (pos - dist) % 100 ))
        # Handle negative modulo
        if (( new_pos < 0 )); then
            new_pos=$(( new_pos + 100 ))
        fi

        # Part 2: Count clicks through 0 during rotation (LEFT)
        # Going left by dist from pos, we hit 0 at steps: pos, pos+100, pos+200, ...
        # (where step is how many clicks we've made)
        if (( pos == 0 )); then
            # Special case: starting at 0 doesn't count
            crosses=$(( dist / 100 ))
        elif (( dist >= pos )); then
            # We hit 0 at step pos, and possibly more times
            crosses=$(( (dist - pos) / 100 + 1 ))
        else
            # dist < pos, we don't reach 0
            crosses=0
        fi
        part2_count=$(( part2_count + crosses ))

    else  # dir == "R"
        new_pos=$(( (pos + dist) % 100 ))

        # Part 2: Count clicks through 0 during rotation (RIGHT)
        # Going right, we hit 0 when (pos + steps) is divisible by 100
        # Count multiples of 100 in range (pos, pos+dist]
        # This is: floor((pos + dist) / 100) - floor(pos / 100)
        crosses=$(( (pos + dist) / 100 - pos / 100 ))
        part2_count=$(( part2_count + crosses ))
    fi

    # Part 1: Check if we ended at 0
    if (( new_pos == 0 )); then
        part1_count=$(( part1_count + 1 ))
    fi

    # Update position
    pos=$new_pos

done < "$input_file"

echo "Part 1: $part1_count"
echo "Part 2: $part2_count"
