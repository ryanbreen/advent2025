#!/bin/bash

# Day 7: No Space Left On Device

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Associative array to store directory sizes
declare -A dir_sizes

# Array to track current path
declare -a path

# Parse the filesystem
while IFS= read -r line; do
    if [[ "$line" == '$ cd '* ]]; then
        target="${line:5}"
        if [[ "$target" == "/" ]]; then
            path=("/")
        elif [[ "$target" == ".." ]]; then
            unset 'path[-1]'
        else
            path+=("$target")
        fi
    elif [[ "$line" == '$ ls' ]]; then
        continue
    elif [[ "$line" == 'dir '* ]]; then
        continue
    elif [[ -n "$line" ]]; then
        # It's a file with size
        size="${line%% *}"
        # Add size to current directory and all parent directories
        for ((i=0; i<${#path[@]}; i++)); do
            # Build path string
            if [[ $i -eq 0 ]]; then
                dir_path="/"
            else
                dir_path="/"
                for ((j=1; j<=i; j++)); do
                    dir_path="${dir_path}${path[j]}/"
                done
            fi
            dir_sizes["$dir_path"]=$((${dir_sizes["$dir_path"]:-0} + size))
        done
    fi
done < "$INPUT_FILE"

# Part 1: Sum of sizes of directories with total size <= 100000
part1=0
for size in "${dir_sizes[@]}"; do
    if [[ $size -le 100000 ]]; then
        part1=$((part1 + size))
    fi
done

# Part 2: Find smallest directory to delete to free enough space
total_space=70000000
needed_space=30000000
used_space="${dir_sizes["/"]}"
free_space=$((total_space - used_space))
need_to_free=$((needed_space - free_space))

part2=""
for size in "${dir_sizes[@]}"; do
    if [[ $size -ge $need_to_free ]]; then
        if [[ -z "$part2" ]] || [[ $size -lt $part2 ]]; then
            part2=$size
        fi
    fi
done

echo "Part 1: $part1"
echo "Part 2: $part2"
