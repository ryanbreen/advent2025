#!/usr/bin/env bash

# Read input file into array
input_file="../input.txt"
grid=()
while IFS= read -r line; do
    grid+=("$line")
done < "$input_file"

rows=${#grid[@]}
cols=${#grid[0]}

# Part 1: Find all occurrences of XMAS in 8 directions
part1() {
    local target="XMAS"
    local count=0

    # 8 directions: right, left, down, up, down-right, down-left, up-right, up-left
    local -a directions=(
        "0 1"    # right
        "0 -1"   # left
        "1 0"    # down
        "-1 0"   # up
        "1 1"    # down-right
        "1 -1"   # down-left
        "-1 1"   # up-right
        "-1 -1"  # up-left
    )

    for ((r = 0; r < rows; r++)); do
        for ((c = 0; c < cols; c++)); do
            # Try each direction from this position
            for dir in "${directions[@]}"; do
                read -r dr dc <<< "$dir"

                # Check if XMAS fits in this direction
                local found=1
                for ((i = 0; i < ${#target}; i++)); do
                    local nr=$((r + dr * i))
                    local nc=$((c + dc * i))

                    # Check bounds
                    if ((nr < 0 || nr >= rows || nc < 0 || nc >= cols)); then
                        found=0
                        break
                    fi

                    # Check character match
                    local char="${grid[nr]:nc:1}"
                    local target_char="${target:i:1}"
                    if [[ "$char" != "$target_char" ]]; then
                        found=0
                        break
                    fi
                done

                if ((found)); then
                    ((count++))
                fi
            done
        done
    done

    echo "$count"
}

# Part 2: Find X-MAS patterns (two MAS forming an X)
part2() {
    local count=0

    # Check each possible center point (A must be in the middle)
    for ((r = 1; r < rows - 1; r++)); do
        for ((c = 1; c < cols - 1; c++)); do
            # Center must be 'A'
            if [[ "${grid[r]:c:1}" != "A" ]]; then
                continue
            fi

            # Get the four corners
            local top_left="${grid[$((r-1))]:$((c-1)):1}"
            local top_right="${grid[$((r-1))]:$((c+1)):1}"
            local bottom_left="${grid[$((r+1))]:$((c-1)):1}"
            local bottom_right="${grid[$((r+1))]:$((c+1)):1}"

            # Check diagonal 1 (top-left to bottom-right): MAS or SAM
            local diag1_ok=0
            if [[ ("$top_left" == "M" && "$bottom_right" == "S") ||
                  ("$top_left" == "S" && "$bottom_right" == "M") ]]; then
                diag1_ok=1
            fi

            # Check diagonal 2 (top-right to bottom-left): MAS or SAM
            local diag2_ok=0
            if [[ ("$top_right" == "M" && "$bottom_left" == "S") ||
                  ("$top_right" == "S" && "$bottom_left" == "M") ]]; then
                diag2_ok=1
            fi

            if ((diag1_ok && diag2_ok)); then
                ((count++))
            fi
        done
    done

    echo "$count"
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
