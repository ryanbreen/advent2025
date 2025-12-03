#!/usr/bin/env bash

# Read input efficiently
input=$(<"../input.txt")
input="${input//$'\n'/}"

# Split by commas
IFS=',' read -ra ranges <<< "$input"

part1_total=0
part2_total=0

# Process each range
for range in "${ranges[@]}"; do
    [[ -z "$range" ]] && continue

    IFS='-' read -ra parts <<< "$range"
    start="${parts[0]}"
    end="${parts[1]}"

    # Iterate through all numbers in the range
    for ((num=start; num<=end; num++)); do
        numstr="$num"
        len=${#numstr}

        # Part 1: check if pattern repeated exactly twice (even length required)
        if (( len % 2 == 0 )); then
            half=$((len / 2))
            if [[ "${numstr:0:$half}" == "${numstr:$half}" ]]; then
                (( part1_total += num ))
            fi
        fi

        # Part 2: check if pattern repeated at least twice
        for ((pattern_len=1; pattern_len<=len/2; pattern_len++)); do
            (( len % pattern_len != 0 )) && continue

            pattern="${numstr:0:$pattern_len}"
            match=1

            # Check all segments match the pattern
            for ((i=pattern_len; i<len; i+=pattern_len)); do
                [[ "${numstr:$i:$pattern_len}" == "$pattern" ]] || { match=0; break; }
            done

            (( match == 1 )) && { (( part2_total += num )); break; }
        done
    done
done

echo "Part 1: $part1_total"
echo "Part 2: $part2_total"
