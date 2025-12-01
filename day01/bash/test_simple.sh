#!/bin/bash

# Simple test with example
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

pos=50
part1=0
part2=0

echo "Position: $pos"

while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    dir="${line:0:1}"
    dist="${line:1}"

    if [[ "$dir" == "L" ]]; then
        new_pos=$(( (pos - dist) % 100 ))
        if (( new_pos < 0 )); then
            new_pos=$(( new_pos + 100 ))
        fi

        if (( pos == 0 )); then
            crosses=$(( dist / 100 ))
        elif (( dist >= pos )); then
            crosses=$(( (dist - pos) / 100 + 1 ))
        else
            crosses=0
        fi
        part2=$(( part2 + crosses ))
    else
        new_pos=$(( (pos + dist) % 100 ))
        crosses=$(( (pos + dist) / 100 - pos / 100 ))
        part2=$(( part2 + crosses ))
    fi

    printf "%s from %2d -> %2d" "$line" "$pos" "$new_pos"
    if (( crosses > 0 )); then
        printf " (crossed 0: %d times)" "$crosses"
    fi
    if (( new_pos == 0 )); then
        part1=$(( part1 + 1 ))
        printf " **AT 0**"
    fi
    echo

    pos=$new_pos
done < /tmp/test_input.txt

echo
echo "Part 1: $part1 (expected 3)"
echo "Part 2: $part2 (expected 6)"
