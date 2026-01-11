#!/bin/bash

# Day 3: Binary Diagnostic
# Read input
INPUT_FILE="${BASH_SOURCE%/*}/../input.txt"

# Read all numbers into an array (compatible with older bash)
numbers=()
while IFS= read -r line || [[ -n "$line" ]]; do
    [[ -n "$line" ]] && numbers+=("$line")
done < "$INPUT_FILE"

# Get the number of bits (length of first number)
num_bits=${#numbers[0]}

# Part 1: Calculate gamma and epsilon rates
part1() {
    local gamma=0
    local total=${#numbers[@]}

    for ((pos=0; pos<num_bits; pos++)); do
        local ones=0
        for num in "${numbers[@]}"; do
            if [[ "${num:$pos:1}" == "1" ]]; then
                ((ones++))
            fi
        done

        local zeros=$((total - ones))

        if ((ones >= zeros)); then
            gamma=$((gamma | (1 << (num_bits - 1 - pos))))
        fi
    done

    # epsilon is bitwise NOT of gamma (within num_bits)
    local mask=$(( (1 << num_bits) - 1 ))
    local epsilon=$((gamma ^ mask))

    echo $((gamma * epsilon))
}

# Find rating (oxygen or CO2)
find_rating() {
    local use_most_common=$1

    # Copy array to candidates
    local candidates=("${numbers[@]}")

    for ((pos=0; pos<num_bits; pos++)); do
        if ((${#candidates[@]} == 1)); then
            break
        fi

        local ones=0
        for num in "${candidates[@]}"; do
            if [[ "${num:$pos:1}" == "1" ]]; then
                ((ones++))
            fi
        done

        local zeros=$((${#candidates[@]} - ones))
        local target

        if ((use_most_common)); then
            if ((ones >= zeros)); then
                target="1"
            else
                target="0"
            fi
        else
            if ((zeros <= ones)); then
                target="0"
            else
                target="1"
            fi
        fi

        # Filter candidates
        local new_candidates=()
        for num in "${candidates[@]}"; do
            if [[ "${num:$pos:1}" == "$target" ]]; then
                new_candidates+=("$num")
            fi
        done
        candidates=("${new_candidates[@]}")
    done

    # Convert binary to decimal
    echo $((2#${candidates[0]}))
}

# Part 2: Calculate life support rating
part2() {
    local oxygen=$(find_rating 1)
    local co2=$(find_rating 0)
    echo $((oxygen * co2))
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
