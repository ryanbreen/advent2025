#!/bin/bash

# Day 7: Camel Cards
# Bash solution for Advent of Code 2023

INPUT_FILE="../input.txt"

# Convert card to numeric value (Part 1)
# Card strength order: 2 3 4 5 6 7 8 9 T J Q K A
card_value_p1() {
    local card="$1"
    case "$card" in
        2) echo 0 ;;
        3) echo 1 ;;
        4) echo 2 ;;
        5) echo 3 ;;
        6) echo 4 ;;
        7) echo 5 ;;
        8) echo 6 ;;
        9) echo 7 ;;
        T) echo 8 ;;
        J) echo 9 ;;
        Q) echo 10 ;;
        K) echo 11 ;;
        A) echo 12 ;;
    esac
}

# Convert card to numeric value (Part 2 - J is weakest)
card_value_p2() {
    local card="$1"
    case "$card" in
        J) echo 0 ;;
        2) echo 1 ;;
        3) echo 2 ;;
        4) echo 3 ;;
        5) echo 4 ;;
        6) echo 5 ;;
        7) echo 6 ;;
        8) echo 7 ;;
        9) echo 8 ;;
        T) echo 9 ;;
        Q) echo 10 ;;
        K) echo 11 ;;
        A) echo 12 ;;
    esac
}

# Count occurrences of each character and return sorted counts
# Uses grep/sed/sort instead of associative arrays for compatibility
count_cards() {
    local hand="$1"
    # Split hand into individual characters, count each, sort counts descending
    echo "$hand" | fold -w1 | sort | uniq -c | awk '{print $1}' | sort -rn | tr '\n' ' ' | sed 's/ *$//'
}

# Count non-joker cards and return sorted counts
count_cards_no_jokers() {
    local hand="$1"
    # Remove all J's, then count
    local no_j
    no_j=$(echo "$hand" | tr -d 'J')
    if [[ -z "$no_j" ]]; then
        echo ""
        return
    fi
    echo "$no_j" | fold -w1 | sort | uniq -c | awk '{print $1}' | sort -rn | tr '\n' ' ' | sed 's/ *$//'
}

# Get hand type from sorted counts (higher = stronger)
# 6 = Five of a kind, 5 = Four of a kind, 4 = Full house
# 3 = Three of a kind, 2 = Two pair, 1 = One pair, 0 = High card
get_type_from_counts() {
    local counts="$1"
    case "$counts" in
        "5") echo 6 ;;          # Five of a kind
        "4 1") echo 5 ;;        # Four of a kind
        "3 2") echo 4 ;;        # Full house
        "3 1 1") echo 3 ;;      # Three of a kind
        "2 2 1") echo 2 ;;      # Two pair
        "2 1 1 1") echo 1 ;;    # One pair
        "1 1 1 1 1") echo 0 ;;  # High card
        *) echo 0 ;;            # Default: High card
    esac
}

# Get hand type (Part 1)
get_hand_type() {
    local hand="$1"
    local counts
    counts=$(count_cards "$hand")
    get_type_from_counts "$counts"
}

# Count jokers in hand
count_jokers() {
    local hand="$1"
    echo "$hand" | tr -cd 'J' | wc -c | tr -d ' '
}

# Get hand type with jokers (Part 2)
get_hand_type_jokers() {
    local hand="$1"
    local joker_count
    joker_count=$(count_jokers "$hand")

    # If no jokers, use regular hand type
    if [[ "$joker_count" -eq 0 ]]; then
        get_hand_type "$hand"
        return
    fi

    # If all jokers, it's five of a kind
    if [[ "$joker_count" -eq 5 ]]; then
        echo 6
        return
    fi

    # Get counts without jokers
    local counts_str
    counts_str=$(count_cards_no_jokers "$hand")

    # Parse counts into array
    read -ra counts <<< "$counts_str"

    # Add jokers to the highest count
    counts[0]=$((counts[0] + joker_count))

    # Rebuild counts string
    local new_counts="${counts[*]}"

    get_type_from_counts "$new_counts"
}

# Create a sortable key for a hand (Part 1)
# Format: type + 5 two-digit card values (total 11 digits, zero-padded)
hand_sort_key_p1() {
    local hand="$1"
    local type card_vals i card val

    type=$(get_hand_type "$hand")

    card_vals=""
    for ((i = 0; i < 5; i++)); do
        card="${hand:i:1}"
        val=$(card_value_p1 "$card")
        card_vals+=$(printf "%02d" "$val")
    done

    echo "${type}${card_vals}"
}

# Create a sortable key for a hand (Part 2)
hand_sort_key_p2() {
    local hand="$1"
    local type card_vals i card val

    type=$(get_hand_type_jokers "$hand")

    card_vals=""
    for ((i = 0; i < 5; i++)); do
        card="${hand:i:1}"
        val=$(card_value_p2 "$card")
        card_vals+=$(printf "%02d" "$val")
    done

    echo "${type}${card_vals}"
}

# Part 1: Standard rules
part1() {
    local total=0
    local rank=1
    local lines_with_keys=""

    # Read all hands and compute sort keys
    while IFS=' ' read -r hand bid || [[ -n "$hand" ]]; do
        [[ -z "$hand" ]] && continue
        local key
        key=$(hand_sort_key_p1 "$hand")
        lines_with_keys+="${key}:${bid}"$'\n'
    done < "$INPUT_FILE"

    # Sort by key and calculate winnings
    while IFS=':' read -r key bid; do
        [[ -z "$key" ]] && continue
        total=$((total + rank * bid))
        ((rank++))
    done < <(echo "$lines_with_keys" | sort)

    echo "$total"
}

# Part 2: Joker rules
part2() {
    local total=0
    local rank=1
    local lines_with_keys=""

    # Read all hands and compute sort keys
    while IFS=' ' read -r hand bid || [[ -n "$hand" ]]; do
        [[ -z "$hand" ]] && continue
        local key
        key=$(hand_sort_key_p2 "$hand")
        lines_with_keys+="${key}:${bid}"$'\n'
    done < "$INPUT_FILE"

    # Sort by key and calculate winnings
    while IFS=':' read -r key bid; do
        [[ -z "$key" ]] && continue
        total=$((total + rank * bid))
        ((rank++))
    done < <(echo "$lines_with_keys" | sort)

    echo "$total"
}

# Main
cd "$(dirname "$0")" || exit 1

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
