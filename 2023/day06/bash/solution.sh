#!/bin/bash

# Day 6: Wait For It
# Read input from ../input.txt

input_file="../input.txt"

# Read input lines
mapfile -t lines < "$input_file"

# Parse times and distances from input
time_line="${lines[0]}"
dist_line="${lines[1]}"

# Extract numbers from lines
read -ra times <<< "$(echo "$time_line" | sed 's/Time://' | tr -s ' ')"
read -ra distances <<< "$(echo "$dist_line" | sed 's/Distance://' | tr -s ' ')"

# Function to count ways to win using quadratic formula
count_ways_to_win() {
    local time="$1"
    local record="$2"

    # We need: t * (time - t) > record
    # Solving: -t^2 + time*t - record > 0
    # Roots: t = (time ± sqrt(time^2 - 4*record)) / 2

    # Calculate discriminant: time^2 - 4*record
    local discriminant=$(echo "$time * $time - 4 * $record" | bc)

    # If discriminant <= 0, no solutions
    if [ "$(echo "$discriminant <= 0" | bc)" -eq 1 ]; then
        echo 0
        return
    fi

    # Calculate sqrt of discriminant using bc
    local sqrt_d=$(echo "scale=20; sqrt($discriminant)" | bc)

    # Calculate roots
    local t_low=$(echo "scale=20; ($time - $sqrt_d) / 2" | bc)
    local t_high=$(echo "scale=20; ($time + $sqrt_d) / 2" | bc)

    # We need integer values strictly between the roots
    # first = floor(t_low) + 1
    # last = ceil(t_high) - 1

    # floor(t_low): truncate toward negative infinity
    local floor_low=$(echo "scale=0; $t_low / 1" | bc)
    # If t_low is negative and has a fractional part, we need to subtract 1
    if [ "$(echo "$t_low < 0 && $t_low != $floor_low" | bc)" -eq 1 ]; then
        floor_low=$((floor_low - 1))
    fi
    local first=$((floor_low + 1))

    # ceil(t_high): if t_high has fractional part, round up
    local trunc_high=$(echo "scale=0; $t_high / 1" | bc)
    local ceil_high
    if [ "$(echo "$t_high > $trunc_high" | bc)" -eq 1 ]; then
        ceil_high=$((trunc_high + 1))
    else
        ceil_high=$trunc_high
    fi
    local last=$((ceil_high - 1))

    if [ "$last" -lt "$first" ]; then
        echo 0
        return
    fi

    echo $((last - first + 1))
}

# Part 1: Multiply counts for all races
part1() {
    local result=1
    local num_races=${#times[@]}

    for ((i=0; i<num_races; i++)); do
        local ways=$(count_ways_to_win "${times[$i]}" "${distances[$i]}")
        result=$((result * ways))
    done

    echo "$result"
}

# Part 2: Concatenate all times and distances
part2() {
    local combined_time=""
    local combined_distance=""

    for t in "${times[@]}"; do
        combined_time="${combined_time}${t}"
    done

    for d in "${distances[@]}"; do
        combined_distance="${combined_distance}${d}"
    done

    count_ways_to_win "$combined_time" "$combined_distance"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
