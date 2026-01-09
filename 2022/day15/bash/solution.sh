#!/bin/bash

# Day 15: Beacon Exclusion Zone
# Parse sensor/beacon positions, compute Manhattan distance coverage

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Parse sensors: extract sx, sy, bx, by, dist
declare -a SENSORS_SX SENSORS_SY SENSORS_DIST
declare -a BEACONS_X BEACONS_Y
SENSOR_COUNT=0

parse_input() {
    while IFS= read -r line || [[ -n "$line" ]]; do
        # Parse: Sensor at x=N, y=N: closest beacon is at x=N, y=N
        if [[ "$line" =~ x=(-?[0-9]+).*y=(-?[0-9]+).*x=(-?[0-9]+).*y=(-?[0-9]+) ]]; then
            sx="${BASH_REMATCH[1]}"
            sy="${BASH_REMATCH[2]}"
            bx="${BASH_REMATCH[3]}"
            by="${BASH_REMATCH[4]}"

            # Manhattan distance
            dx=$((sx - bx))
            dy=$((sy - by))
            [[ $dx -lt 0 ]] && dx=$((-dx))
            [[ $dy -lt 0 ]] && dy=$((-dy))
            dist=$((dx + dy))

            SENSORS_SX[$SENSOR_COUNT]=$sx
            SENSORS_SY[$SENSOR_COUNT]=$sy
            SENSORS_DIST[$SENSOR_COUNT]=$dist
            BEACONS_X[$SENSOR_COUNT]=$bx
            BEACONS_Y[$SENSOR_COUNT]=$by
            ((SENSOR_COUNT++))
        fi
    done < "$INPUT_FILE"
}

# Get coverage ranges at a specific row, output as "start end" lines
get_coverage_at_row() {
    local row=$1
    local i sx sy dist row_dist x_spread

    for ((i = 0; i < SENSOR_COUNT; i++)); do
        sx=${SENSORS_SX[$i]}
        sy=${SENSORS_SY[$i]}
        dist=${SENSORS_DIST[$i]}

        row_dist=$((sy - row))
        [[ $row_dist -lt 0 ]] && row_dist=$((-row_dist))

        if [[ $row_dist -le $dist ]]; then
            x_spread=$((dist - row_dist))
            echo "$((sx - x_spread)) $((sx + x_spread))"
        fi
    done
}

# Merge overlapping ranges (sorted input)
merge_ranges() {
    local first=1
    local merged_start merged_end
    local start end

    while read -r start end; do
        if [[ $first -eq 1 ]]; then
            merged_start=$start
            merged_end=$end
            first=0
        elif [[ $start -le $((merged_end + 1)) ]]; then
            if [[ $end -gt $merged_end ]]; then
                merged_end=$end
            fi
        else
            echo "$merged_start $merged_end"
            merged_start=$start
            merged_end=$end
        fi
    done

    if [[ $first -eq 0 ]]; then
        echo "$merged_start $merged_end"
    fi
}

part1() {
    local target_row=2000000
    local total=0

    # Get merged ranges at target row
    local ranges
    ranges=$(get_coverage_at_row $target_row | sort -n | merge_ranges)

    # Sum up coverage
    while read -r start end; do
        total=$((total + end - start + 1))
    done <<< "$ranges"

    # Count unique beacons on this row using a simple array
    local beacon_count=0
    local seen_beacons=""
    for ((i = 0; i < SENSOR_COUNT; i++)); do
        if [[ ${BEACONS_Y[$i]} -eq $target_row ]]; then
            local key="${BEACONS_X[$i]}"
            if [[ ! " $seen_beacons " =~ " $key " ]]; then
                seen_beacons="$seen_beacons $key"
                ((beacon_count++))
            fi
        fi
    done

    echo $((total - beacon_count))
}

# Check if a point is covered by any sensor
is_covered() {
    local x=$1
    local y=$2
    local i sx sy dist dx dy manhattan

    for ((i = 0; i < SENSOR_COUNT; i++)); do
        sx=${SENSORS_SX[$i]}
        sy=${SENSORS_SY[$i]}
        dist=${SENSORS_DIST[$i]}

        dx=$((x - sx))
        dy=$((y - sy))
        [[ $dx -lt 0 ]] && dx=$((-dx))
        [[ $dy -lt 0 ]] && dy=$((-dy))
        manhattan=$((dx + dy))

        if [[ $manhattan -le $dist ]]; then
            return 0  # covered
        fi
    done

    return 1  # not covered
}

part2() {
    local max_coord=4000000

    # Optimization: The distress beacon must be at distance d+1 from at least one sensor
    # Check boundary points of each sensor's range
    # Use lines: the beacon is at intersection of diagonal boundary lines

    # For each sensor, the boundary is a diamond at distance d+1
    # We can represent boundaries as lines: y = x + b or y = -x + b
    # Collect positive and negative slope intercepts

    local pos_slopes=()  # y - x = b (positive slope lines)
    local neg_slopes=()  # y + x = b (negative slope lines)

    for ((i = 0; i < SENSOR_COUNT; i++)); do
        local sx=${SENSORS_SX[$i]}
        local sy=${SENSORS_SY[$i]}
        local dist=${SENSORS_DIST[$i]}
        local d1=$((dist + 1))

        # Four boundary lines of the diamond
        # Top-left edge: y - x = sy + d1 - sx
        # Bottom-right edge: y - x = sy - d1 - sx
        pos_slopes+=($((sy - sx + d1)))
        pos_slopes+=($((sy - sx - d1)))

        # Top-right edge: y + x = sy + d1 + sx
        # Bottom-left edge: y + x = sy - d1 + sx
        neg_slopes+=($((sy + sx + d1)))
        neg_slopes+=($((sy + sx - d1)))
    done

    # Check intersections of positive and negative slope lines
    for pos in "${pos_slopes[@]}"; do
        for neg in "${neg_slopes[@]}"; do
            # y - x = pos, y + x = neg
            # 2y = pos + neg, 2x = neg - pos
            local sum=$((pos + neg))
            local diff=$((neg - pos))

            # Check if even (integer solution)
            if [[ $((sum % 2)) -eq 0 ]] && [[ $((diff % 2)) -eq 0 ]]; then
                local y=$((sum / 2))
                local x=$((diff / 2))

                # Check bounds
                if [[ $x -ge 0 ]] && [[ $x -le $max_coord ]] && [[ $y -ge 0 ]] && [[ $y -le $max_coord ]]; then
                    # Check if this point is not covered by any sensor
                    if ! is_covered $x $y; then
                        echo $((x * 4000000 + y))
                        return
                    fi
                fi
            fi
        done
    done

    echo "Not found"
}

# Main
parse_input

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
