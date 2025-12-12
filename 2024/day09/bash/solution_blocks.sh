#!/usr/bin/env bash
#
# Advent of Code 2024 Day 9: Disk Fragmenter
#
# Compact a fragmented disk by moving file blocks to fill gaps.
# Part 1: Move blocks one at a time from end to leftmost free space
# Part 2: Move whole files to leftmost span that fits

set -euo pipefail

# Parse disk map into expanded block representation
# Creates array 'blocks' where each element is file ID or -1 for free space
parse_disk_map() {
    local disk_map="$1"
    blocks=()
    local file_id=0
    local is_file=1
    local i

    for (( i=0; i<${#disk_map}; i++ )); do
        local digit="${disk_map:$i:1}"
        local length="$digit"

        if (( is_file )); then
            for (( j=0; j<length; j++ )); do
                blocks+=("$file_id")
            done
            (( file_id++ ))
        else
            for (( j=0; j<length; j++ )); do
                blocks+=("-1")
            done
        fi
        (( is_file = 1 - is_file ))
    done
}

# Compact disk by moving blocks one at a time from end to leftmost free space
compact_blocks() {
    local left=0
    local right=$(( ${#blocks[@]} - 1 ))

    while (( left < right )); do
        # Find leftmost free space
        while (( left < right && blocks[left] != -1 )); do
            (( left++ ))
        done

        # Find rightmost file block
        while (( left < right && blocks[right] == -1 )); do
            (( right-- ))
        done

        if (( left < right )); then
            # Swap
            blocks[left]="${blocks[right]}"
            blocks[right]=-1
            (( left++ ))
            (( right-- ))
        fi
    done
}

# Calculate filesystem checksum: sum of position * file_id for each block
calculate_checksum() {
    local checksum=0
    local pos

    for pos in "${!blocks[@]}"; do
        local file_id="${blocks[pos]}"
        if (( file_id != -1 )); then
            (( checksum += pos * file_id ))
        fi
    done

    echo "$checksum"
}

# Part 1: Compact by moving individual blocks, return checksum
part1() {
    local disk_map
    disk_map=$(cat ../input.txt)

    parse_disk_map "$disk_map"
    compact_blocks
    calculate_checksum
}

# Part 2: Compact by moving whole files (highest ID first), return checksum
part2() {
    local disk_map
    disk_map=$(cat ../input.txt)

    parse_disk_map "$disk_map"

    # Find all files: file_id -> "start,length"
    declare -A files
    local i=0
    local max_file_id=0

    while (( i < ${#blocks[@]} )); do
        if (( blocks[i] != -1 )); then
            local file_id="${blocks[i]}"
            local start=$i
            while (( i < ${#blocks[@]} && blocks[i] == file_id )); do
                (( i++ ))
            done
            local length=$(( i - start ))
            files[$file_id]="$start,$length"
            if (( file_id > max_file_id )); then
                max_file_id=$file_id
            fi
        else
            (( i++ ))
        fi
    done

    # Process files in decreasing order of file ID
    for (( file_id=max_file_id; file_id>=0; file_id-- )); do
        if [[ -z "${files[$file_id]:-}" ]]; then
            continue
        fi

        IFS=',' read -r start length <<< "${files[$file_id]}"

        # Find leftmost span of free space that fits this file
        # Must be to the left of current position
        local free_start=-1
        i=0

        while (( i < start )); do
            if (( blocks[i] == -1 )); then
                # Count consecutive free blocks
                local span_start=$i
                local span_length=0
                while (( i < start && blocks[i] == -1 )); do
                    (( span_length++ ))
                    (( i++ ))
                done
                if (( span_length >= length )); then
                    free_start=$span_start
                    break
                fi
            else
                (( i++ ))
            fi
        done

        # Move file if we found a suitable span
        if (( free_start != -1 )); then
            # Clear old position
            for (( j=start; j<start+length; j++ )); do
                blocks[j]=-1
            done
            # Write to new position
            for (( j=free_start; j<free_start+length; j++ )); do
                blocks[j]=$file_id
            done
            # Update file position
            files[$file_id]="$free_start,$length"
        fi
    done

    calculate_checksum
}

# Main
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
