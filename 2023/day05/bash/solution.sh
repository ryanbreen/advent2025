#!/bin/bash
# Advent of Code 2023 Day 5: If You Give A Seed A Fertilizer
#
# This solution converts seeds through a series of category maps
# (seed->soil->fertilizer->water->light->temperature->humidity->location)
# to find the minimum location number.
#
# Part 1: Process individual seed values through all maps
# Part 2: Process seed ranges efficiently by splitting ranges at map boundaries

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Global arrays for seeds and map data
declare -a seeds          # Seed values from input
declare -a map_counts     # Number of entries in each map (0-6)

# Associative arrays for map data
# Keys are "mapIndex:entryIndex" (e.g., "0:3" for map 0, entry 3)
declare -A map_dst        # Destination range start
declare -A map_src        # Source range start
declare -A map_len        # Range length

# Parse the input file into seeds array and map data structures
parse_input() {
    local current_map=-1
    local line_num=0

    while IFS= read -r line || [[ -n "$line" ]]; do
        if (( line_num == 0 )); then
            # First line: "seeds: 79 14 55 13"
            local seeds_str="${line#seeds: }"
            read -ra seeds <<< "$seeds_str"
        elif [[ $line =~ ^[a-z]+-to-[a-z]+\ map: ]]; then
            # Map header line (e.g., "seed-to-soil map:")
            ((current_map++)) || true
            map_counts[$current_map]=0
        elif [[ $line =~ ^[0-9] ]]; then
            # Map entry: "dest_start src_start length"
            local dst src len
            read -r dst src len <<< "$line"
            local idx=${map_counts[$current_map]}
            local key="${current_map}:${idx}"
            map_dst[$key]=$dst
            map_src[$key]=$src
            map_len[$key]=$len
            ((map_counts[$current_map]++)) || true
        fi
        ((line_num++)) || true
    done < "$INPUT_FILE"
}

# Apply a single map to a value
# If the value falls within a source range, it gets translated to the destination range
# Otherwise, the value passes through unchanged
apply_map() {
    local value=$1
    local map_idx=$2
    local count=${map_counts[$map_idx]}

    for ((i = 0; i < count; i++)); do
        local key="${map_idx}:${i}"
        local dst=${map_dst[$key]}
        local src=${map_src[$key]}
        local len=${map_len[$key]}

        # Check if value is in source range [src, src+len)
        if (( value >= src && value < src + len )); then
            echo $((dst + value - src))
            return
        fi
    done

    # No mapping found - value passes through unchanged
    echo "$value"
}

# Convert a seed through all 7 maps to get final location
seed_to_location() {
    local value=$1
    for ((m = 0; m < 7; m++)); do
        value=$(apply_map "$value" "$m")
    done
    echo "$value"
}

# Part 1: Find minimum location for individual seeds
# Simply process each seed through all maps and track the minimum
part1() {
    local min_loc=""

    for seed in "${seeds[@]}"; do
        local loc
        loc=$(seed_to_location "$seed")
        if [[ -z "$min_loc" ]] || (( loc < min_loc )); then
            min_loc=$loc
        fi
    done

    echo "$min_loc"
}

# Part 2: Process seed ranges efficiently
#
# Key insight: Rather than iterating through billions of individual seeds,
# we process ranges and split them at map boundaries.
#
# For each map, a range can be split into up to 3 parts:
#   1. Before the map range - unmapped, stays in "remaining" for other map entries
#   2. Overlapping the map range - mapped using offset (dst - src)
#   3. After the map range - unmapped, stays in "remaining" for other map entries
#
# Ranges that don't match any map entry pass through unchanged.
part2() {
    # Build initial ranges from seeds (pairs of: start, length)
    # Ranges stored as "start:end" where end is exclusive
    local ranges=()
    for ((i = 0; i < ${#seeds[@]}; i += 2)); do
        local start=${seeds[$i]}
        local len=${seeds[$((i + 1))]}
        local end=$((start + len))
        ranges+=("$start:$end")
    done

    # Apply each of the 7 maps in sequence
    for ((m = 0; m < 7; m++)); do
        local new_ranges=()
        local count=${map_counts[$m]}

        # Process each input range
        for range in "${ranges[@]}"; do
            local r_start="${range%%:*}"
            local r_end="${range##*:}"

            # Track unmapped portions that need to check other map entries
            local remaining=("$r_start:$r_end")

            # Try each map entry
            for ((i = 0; i < count; i++)); do
                local key="${m}:${i}"
                local dst=${map_dst[$key]}
                local src=${map_src[$key]}
                local len=${map_len[$key]}
                local src_end=$((src + len))

                local next_remaining=()

                for rem in "${remaining[@]}"; do
                    local rem_start="${rem%%:*}"
                    local rem_end="${rem##*:}"

                    # Split the range into: before, overlap, after

                    # BEFORE: portion before map source range [rem_start, src)
                    # Stays unmapped, goes to next_remaining for other entries
                    if (( rem_start < src )); then
                        local before_end=$rem_end
                        (( before_end > src )) && before_end=$src
                        if (( rem_start < before_end )); then
                            next_remaining+=("$rem_start:$before_end")
                        fi
                    fi

                    # OVERLAP: portion within map source range [src, src_end)
                    # Gets translated by offset (dst - src)
                    local overlap_start=$rem_start
                    (( overlap_start < src )) && overlap_start=$src
                    local overlap_end=$rem_end
                    (( overlap_end > src_end )) && overlap_end=$src_end

                    if (( overlap_start < overlap_end )); then
                        local offset=$((dst - src))
                        local mapped_start=$((overlap_start + offset))
                        local mapped_end=$((overlap_end + offset))
                        new_ranges+=("$mapped_start:$mapped_end")
                    fi

                    # AFTER: portion after map source range [src_end, rem_end)
                    # Stays unmapped, goes to next_remaining for other entries
                    if (( rem_end > src_end )); then
                        local after_start=$rem_start
                        (( after_start < src_end )) && after_start=$src_end
                        if (( after_start < rem_end )); then
                            next_remaining+=("$after_start:$rem_end")
                        fi
                    fi
                done

                remaining=("${next_remaining[@]}")
            done

            # Any remaining unmapped portions pass through unchanged
            for rem in "${remaining[@]}"; do
                new_ranges+=("$rem")
            done
        done

        ranges=("${new_ranges[@]}")
    done

    # Find minimum start of any range (that's the minimum location)
    local min_loc=""
    for range in "${ranges[@]}"; do
        local start="${range%%:*}"
        if [[ -z "$min_loc" ]] || (( start < min_loc )); then
            min_loc=$start
        fi
    done

    echo "$min_loc"
}

# Main execution
parse_input
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
