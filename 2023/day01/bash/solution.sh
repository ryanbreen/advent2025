#!/usr/bin/env bash

# Constants
readonly INPUT_FILE="$(dirname "$0")/../input.txt"
readonly -a WORD_NAMES=(one two three four five six seven eight nine)

# Convert word to digit using case statement
word_to_digit() {
    case "$1" in
        one)   echo 1 ;;
        two)   echo 2 ;;
        three) echo 3 ;;
        four)  echo 4 ;;
        five)  echo 5 ;;
        six)   echo 6 ;;
        seven) echo 7 ;;
        eight) echo 8 ;;
        nine)  echo 9 ;;
        *)     echo "" ;;
    esac
}

# Part 1: Extract first and last digit from each line (pure bash)
part1() {
    local total=0
    local line first last char

    while IFS= read -r line; do
        first=""
        last=""

        # Iterate through each character
        for ((i = 0; i < ${#line}; i++)); do
            char="${line:i:1}"
            if [[ "$char" =~ ^[0-9]$ ]]; then
                [[ -z "$first" ]] && first="$char"
                last="$char"
            fi
        done

        # Add to total if we found digits
        [[ -n "$first" ]] && total=$((total + first * 10 + last))
    done < "$INPUT_FILE"

    echo "$total"
}

# Part 2: Also recognize spelled-out words (pure bash, no external commands)
part2() {
    local total=0
    local line first last char digit word substring

    while IFS= read -r line; do
        first=""
        last=""

        # Check each position for digit or word
        for ((i = 0; i < ${#line}; i++)); do
            char="${line:i:1}"
            digit=""

            # Check if character is a digit
            if [[ "$char" =~ ^[0-9]$ ]]; then
                digit="$char"
            else
                # Check if a word starts at this position
                for word in "${WORD_NAMES[@]}"; do
                    substring="${line:i:${#word}}"
                    if [[ "$substring" == "$word" ]]; then
                        digit=$(word_to_digit "$word")
                        break
                    fi
                done
            fi

            # Track first and last digit found
            if [[ -n "$digit" ]]; then
                [[ -z "$first" ]] && first="$digit"
                last="$digit"
            fi
        done

        # Add to total if we found any digits
        [[ -n "$first" ]] && total=$((total + first * 10 + last))
    done < "$INPUT_FILE"

    echo "$total"
}

# Run both parts
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
