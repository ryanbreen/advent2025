#!/usr/bin/env bash
# Day 8: Resonant Collinearity
# Finds antinode positions created by antenna pairs on a grid
# Part 1: Antinodes at 2:1 distance ratio
# Part 2: All collinear grid points
set -euo pipefail

# Read and parse the input grid
declare -A antennas  # frequency -> "r1,c1 r2,c2 ..." (space-separated row,col pairs)
rows=0
cols=0

while IFS= read -r line || [[ -n "$line" ]]; do
    cols=${#line}
    for ((c=0; c<cols; c++)); do
        ch="${line:c:1}"
        if [[ "$ch" != "." ]]; then
            # Append to existing list or create new entry
            if [[ -v antennas[$ch] ]]; then
                antennas[$ch]+=" $rows,$c"
            else
                antennas[$ch]="$rows,$c"
            fi
        fi
    done
    ((rows++))
done < ../input.txt || true  # Prevent set -e from exiting on final read failure

# Calculate Part 1 antinodes (2:1 distance ratio from antenna pairs)
part1() {
    # Use associative array as set for unique antinode locations
    declare -A antinodes_set

    # For each frequency
    for freq in "${!antennas[@]}"; do
        # Convert positions string to array
        read -ra positions <<< "${antennas[$freq]}"

        # For each pair of antennas with same frequency
        for ((i=0; i<${#positions[@]}; i++)); do
            for ((j=i+1; j<${#positions[@]}; j++)); do
                IFS=',' read -r r1 c1 <<< "${positions[i]}"
                IFS=',' read -r r2 c2 <<< "${positions[j]}"

                # Calculate the two antinodes (Part 1: one on each side, 2x distance)
                # Antinode beyond antenna 1 (away from antenna 2)
                ar1=$((2*r1 - r2))
                ac1=$((2*c1 - c2))
                # Antinode beyond antenna 2 (away from antenna 1)
                ar2=$((2*r2 - r1))
                ac2=$((2*c2 - c1))

                # Add if within bounds
                if ((ar1 >= 0 && ar1 < rows && ac1 >= 0 && ac1 < cols)); then
                    antinodes_set["$ar1,$ac1"]=1
                fi
                if ((ar2 >= 0 && ar2 < rows && ac2 >= 0 && ac2 < cols)); then
                    antinodes_set["$ar2,$ac2"]=1
                fi
            done
        done
    done

    echo "${#antinodes_set[@]}"
}

# Calculate Part 2 antinodes (all collinear grid points)
part2() {
    # Use associative array as set for unique antinode locations
    declare -A antinodes_set

    # For each frequency
    for freq in "${!antennas[@]}"; do
        # Convert positions string to array
        read -ra positions <<< "${antennas[$freq]}"

        # For each pair of antennas with same frequency
        for ((i=0; i<${#positions[@]}; i++)); do
            for ((j=i+1; j<${#positions[@]}; j++)); do
                IFS=',' read -r r1 c1 <<< "${positions[i]}"
                IFS=',' read -r r2 c2 <<< "${positions[j]}"

                # Calculate direction vector
                dr=$((r2 - r1))
                dc=$((c2 - c1))

                # Part 2: Extend line in both directions to cover all grid points
                # Direction 1: from antenna 1 towards and beyond antenna 2
                r=$r1
                c=$c1
                while ((r >= 0 && r < rows && c >= 0 && c < cols)); do
                    antinodes_set["$r,$c"]=1
                    r=$((r + dr))
                    c=$((c + dc))
                done

                # Direction 2: from antenna 1 away from antenna 2
                r=$((r1 - dr))
                c=$((c1 - dc))
                while ((r >= 0 && r < rows && c >= 0 && c < cols)); do
                    antinodes_set["$r,$c"]=1
                    r=$((r - dr))
                    c=$((c - dc))
                done
            done
        done
    done

    echo "${#antinodes_set[@]}"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
