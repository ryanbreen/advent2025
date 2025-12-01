#!/bin/bash

# Test with example from problem
echo "Testing with example..."

# Example input
cat > /tmp/test_input.txt << 'EOF'
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
EOF

# Starting position
pos=50
part1_count=0
part2_count=0

echo "Starting at position: $pos"

# Process each line
while IFS= read -r line; do
    # Skip empty lines
    [[ -z "$line" ]] && continue

    # Extract direction and distance
    dir="${line:0:1}"
    dist="${line:1}"
    old_pos=$pos

    # Calculate new position and count zero crossings
    if [[ "$dir" == "L" ]]; then
        new_pos=$(( (pos - dist) % 100 ))
        # Handle negative modulo
        if (( new_pos < 0 )); then
            new_pos=$(( new_pos + 100 ))
        fi

        # Part 2: Count clicks through 0 during rotation
        if (( new_pos == 0 )); then
            # Landing at 0 - count any full rotations before landing
            if (( dist > pos )); then
                crosses=$(( (dist - pos - 1) / 100 ))
                part2_count=$(( part2_count + crosses ))
                if (( crosses > 0 )); then
                    echo "  During rotation (before landing), crossed 0: $crosses time(s)"
                fi
            fi
        else
            # Not landing at 0 - count passes through 0
            if (( dist > pos )); then
                crosses=$(( (dist - pos - 1) / 100 + 1 ))
                part2_count=$(( part2_count + crosses ))
                if (( crosses > 0 )); then
                    echo "  During rotation, crossed 0: $crosses time(s)"
                fi
            fi
        fi

    else  # dir == "R"
        new_pos=$(( (pos + dist) % 100 ))

        # Part 2: Count clicks through 0 during rotation
        if (( new_pos == 0 )); then
            # Landing at 0 - count any full rotations before landing
            if (( dist > (100 - pos) )); then
                crosses=$(( (dist - (100 - pos) - 1) / 100 ))
                part2_count=$(( part2_count + crosses ))
                if (( crosses > 0 )); then
                    echo "  During rotation (before landing), crossed 0: $crosses time(s)"
                fi
            fi
        else
            # Not landing at 0 - count passes through 0
            if (( pos + dist >= 100 )); then
                crosses=$(( (pos + dist - 1) / 100 ))
                part2_count=$(( part2_count + crosses ))
                if (( crosses > 0 )); then
                    echo "  During rotation, crossed 0: $crosses time(s)"
                fi
            fi
        fi
    fi

    # Part 1: Check if we ended at 0
    echo "After $line: moved from $old_pos to $new_pos"
    if (( new_pos == 0 )); then
        part1_count=$(( part1_count + 1 ))
        part2_count=$(( part2_count + 1 ))
        echo "  ** Ended at 0 **"
    fi

    # Update position
    pos=$new_pos

done < /tmp/test_input.txt

echo ""
echo "Part 1 (endings at 0): $part1_count (expected: 3)"
echo "Part 2 (all passes through 0): $part2_count (expected: 6)"
