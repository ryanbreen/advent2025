#!/bin/bash
# Day 17: Clumsy Crucible - Dijkstra's shortest path with movement constraints
# Uses bucket-based priority queue for better performance

# Read input grid
declare -a GRID
ROWS=0
COLS=0

while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ -n "$line" ]]; then
        GRID[$ROWS]="$line"
        COLS=${#line}
        ((ROWS++))
    fi
done < "../input.txt"

# Direction deltas: 0=right, 1=down, 2=left, 3=up
DR=(0 1 0 -1)
DC=(1 0 -1 0)

# Dijkstra's algorithm with movement constraints
# Uses bucket-based priority queue for efficiency
dijkstra() {
    local min_straight=$1
    local max_straight=$2

    # Visited set - using associative array
    declare -A visited
    declare -A dist

    # Bucket-based priority queue
    # bucket[heat] contains space-separated "r:c:d:consec" states
    declare -A bucket
    local min_heat=0
    local max_heat=0

    # Start with initial state (no direction yet, indicated by d=-1)
    bucket[0]="0:0:-1:0"
    dist["0:0:-1:0"]=0

    while [[ $min_heat -le $max_heat ]]; do
        # Find next non-empty bucket
        while [[ $min_heat -le $max_heat && -z "${bucket[$min_heat]}" ]]; do
            ((min_heat++))
        done

        if [[ $min_heat -gt $max_heat ]]; then
            break
        fi

        local heat=$min_heat
        local entries="${bucket[$heat]}"
        unset "bucket[$heat]"

        # Process all entries at this heat level
        for entry in $entries; do
            IFS=':' read -r r c d consec <<< "$entry"

            # Check if already visited
            local state="$r:$c:$d:$consec"
            if [[ -n "${visited[$state]}" ]]; then
                continue
            fi
            visited[$state]=1

            # Check if we reached the goal
            if [[ $r -eq $((ROWS - 1)) && $c -eq $((COLS - 1)) ]]; then
                if [[ $min_straight -eq 0 || $consec -ge $min_straight ]]; then
                    echo "$heat"
                    return
                fi
            fi

            # Try all four directions
            for nd in 0 1 2 3; do
                # Can't reverse direction
                if [[ $d -ne -1 ]]; then
                    local reverse=$(( (d + 2) % 4 ))
                    if [[ $nd -eq $reverse ]]; then
                        continue
                    fi
                fi

                local nr=$((r + DR[nd]))
                local nc=$((c + DC[nd]))

                # Bounds check
                if [[ $nr -lt 0 || $nr -ge $ROWS || $nc -lt 0 || $nc -ge $COLS ]]; then
                    continue
                fi

                local new_consec
                # Check consecutive constraints
                if [[ $nd -eq $d ]]; then
                    # Continuing in same direction
                    new_consec=$((consec + 1))
                    if [[ $new_consec -gt $max_straight ]]; then
                        continue
                    fi
                else
                    # Turning - must have gone min_straight in previous direction first
                    if [[ $d -ne -1 && $consec -lt $min_straight ]]; then
                        continue
                    fi
                    new_consec=1
                fi

                # Get heat loss at new position
                local row="${GRID[$nr]}"
                local cell_heat="${row:$nc:1}"
                local new_heat=$((heat + cell_heat))

                local new_state="$nr:$nc:$nd:$new_consec"

                if [[ -z "${visited[$new_state]}" ]]; then
                    local old_dist="${dist[$new_state]}"
                    if [[ -z "$old_dist" || $new_heat -lt $old_dist ]]; then
                        dist[$new_state]=$new_heat
                        if [[ -z "${bucket[$new_heat]}" ]]; then
                            bucket[$new_heat]="$new_state"
                        else
                            bucket[$new_heat]="${bucket[$new_heat]} $new_state"
                        fi
                        if [[ $new_heat -gt $max_heat ]]; then
                            max_heat=$new_heat
                        fi
                    fi
                fi
            done
        done
    done

    echo "-1"
}

# Part 1: Normal crucible, max 3 consecutive blocks
part1() {
    dijkstra 0 3
}

# Part 2: Ultra crucible, min 4 and max 10 consecutive blocks
part2() {
    dijkstra 4 10
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
