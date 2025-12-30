#!/usr/bin/env bash

# Advent of Code 2023 Day 15: Lens Library
# HASH algorithm and HASHMAP procedure

set -euo pipefail

# HASH algorithm: for each char, current = ((current + ASCII) * 17) % 256
hash_algo() {
    local str="$1"
    local current=0
    local i char ascii

    for ((i = 0; i < ${#str}; i++)); do
        char="${str:$i:1}"
        # Get ASCII value using printf
        ascii=$(printf '%d' "'$char")
        current=$(( (current + ascii) * 17 % 256 ))
    done

    echo "$current"
}

part1() {
    local input="$1"
    local sum=0
    local step hash_val

    # Split by comma and process each step
    IFS=',' read -ra steps <<< "$input"
    for step in "${steps[@]}"; do
        hash_val=$(hash_algo "$step")
        sum=$((sum + hash_val))
    done

    echo "$sum"
}

part2() {
    local input="$1"

    # Each box stores lens labels in order (space-separated)
    # We use two arrays: one for labels (order), one for focal lengths
    declare -a box_labels   # box_labels[i] = "label1 label2 label3"
    declare -A box_focals   # box_focals["box_label"] = focal_length

    # Initialize all 256 boxes as empty
    for ((i = 0; i < 256; i++)); do
        box_labels[$i]=""
    done

    # Split by comma and process each step
    IFS=',' read -ra steps <<< "$input"

    for step in "${steps[@]}"; do
        if [[ "$step" == *"="* ]]; then
            # Add or replace lens
            local label="${step%=*}"
            local focal="${step#*=}"
            local box_num
            box_num=$(hash_algo "$label")

            # Check if label exists in box
            local current_labels="${box_labels[$box_num]}"
            local found=0

            if [[ -n "$current_labels" ]]; then
                for existing_label in $current_labels; do
                    if [[ "$existing_label" == "$label" ]]; then
                        found=1
                        break
                    fi
                done
            fi

            if [[ $found -eq 1 ]]; then
                # Replace focal length only
                box_focals["${box_num}_${label}"]="$focal"
            else
                # Add new lens to end
                if [[ -z "$current_labels" ]]; then
                    box_labels[$box_num]="$label"
                else
                    box_labels[$box_num]="$current_labels $label"
                fi
                box_focals["${box_num}_${label}"]="$focal"
            fi
        else
            # Remove lens (step ends with -)
            local label="${step%-}"
            local box_num
            box_num=$(hash_algo "$label")

            local current_labels="${box_labels[$box_num]}"
            if [[ -n "$current_labels" ]]; then
                local new_labels=""
                for existing_label in $current_labels; do
                    if [[ "$existing_label" != "$label" ]]; then
                        if [[ -z "$new_labels" ]]; then
                            new_labels="$existing_label"
                        else
                            new_labels="$new_labels $existing_label"
                        fi
                    fi
                done
                box_labels[$box_num]="$new_labels"
                # Clean up focal entry
                unset "box_focals[${box_num}_${label}]"
            fi
        fi
    done

    # Calculate focusing power
    local total=0
    for ((box_num = 0; box_num < 256; box_num++)); do
        local labels="${box_labels[$box_num]}"
        if [[ -n "$labels" ]]; then
            local slot=1
            for label in $labels; do
                local focal="${box_focals[${box_num}_${label}]}"
                local power=$(( (box_num + 1) * slot * focal ))
                total=$((total + power))
                slot=$((slot + 1))
            done
        fi
    done

    echo "$total"
}

main() {
    local input_file="${1:-../input.txt}"

    # Read input and remove newlines
    local input
    input=$(tr -d '\n' < "$input_file")

    echo "Part 1: $(part1 "$input")"
    echo "Part 2: $(part2 "$input")"
}

main "$@"
