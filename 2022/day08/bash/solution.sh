#!/usr/bin/env bash

# Day 8: Treetop Tree House

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Read grid into array
declare -a grid
rows=0
cols=0

while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ -n "$line" ]]; then
        grid[$rows]="$line"
        cols=${#line}
        ((rows++))
    fi
done < "$INPUT_FILE"

# Function to get tree height at position
get_height() {
    local r=$1 c=$2
    echo "${grid[$r]:$c:1}"
}

# Check if tree at (row, col) is visible from outside
is_visible() {
    local row=$1 col=$2
    local height
    height=$(get_height "$row" "$col")

    # Edge trees are always visible
    if (( row == 0 || row == rows - 1 || col == 0 || col == cols - 1 )); then
        echo 1
        return
    fi

    # Check from left
    local visible=1
    for ((c = 0; c < col; c++)); do
        local h
        h=$(get_height "$row" "$c")
        if (( h >= height )); then
            visible=0
            break
        fi
    done
    if (( visible == 1 )); then
        echo 1
        return
    fi

    # Check from right
    visible=1
    for ((c = col + 1; c < cols; c++)); do
        local h
        h=$(get_height "$row" "$c")
        if (( h >= height )); then
            visible=0
            break
        fi
    done
    if (( visible == 1 )); then
        echo 1
        return
    fi

    # Check from top
    visible=1
    for ((r = 0; r < row; r++)); do
        local h
        h=$(get_height "$r" "$col")
        if (( h >= height )); then
            visible=0
            break
        fi
    done
    if (( visible == 1 )); then
        echo 1
        return
    fi

    # Check from bottom
    visible=1
    for ((r = row + 1; r < rows; r++)); do
        local h
        h=$(get_height "$r" "$col")
        if (( h >= height )); then
            visible=0
            break
        fi
    done
    if (( visible == 1 )); then
        echo 1
        return
    fi

    echo 0
}

# Calculate scenic score for tree at (row, col)
scenic_score() {
    local row=$1 col=$2
    local height
    height=$(get_height "$row" "$col")

    # Left
    local left=0
    for ((c = col - 1; c >= 0; c--)); do
        ((left++))
        local h
        h=$(get_height "$row" "$c")
        if (( h >= height )); then
            break
        fi
    done

    # Right
    local right=0
    for ((c = col + 1; c < cols; c++)); do
        ((right++))
        local h
        h=$(get_height "$row" "$c")
        if (( h >= height )); then
            break
        fi
    done

    # Up
    local up=0
    for ((r = row - 1; r >= 0; r--)); do
        ((up++))
        local h
        h=$(get_height "$r" "$col")
        if (( h >= height )); then
            break
        fi
    done

    # Down
    local down=0
    for ((r = row + 1; r < rows; r++)); do
        ((down++))
        local h
        h=$(get_height "$r" "$col")
        if (( h >= height )); then
            break
        fi
    done

    echo $((left * right * up * down))
}

# Part 1: Count visible trees
part1() {
    local count=0
    for ((r = 0; r < rows; r++)); do
        for ((c = 0; c < cols; c++)); do
            local v
            v=$(is_visible "$r" "$c")
            ((count += v))
        done
    done
    echo "$count"
}

# Part 2: Find maximum scenic score
part2() {
    local max_score=0
    for ((r = 0; r < rows; r++)); do
        for ((c = 0; c < cols; c++)); do
            local score
            score=$(scenic_score "$r" "$c")
            if (( score > max_score )); then
                max_score=$score
            fi
        done
    done
    echo "$max_score"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
