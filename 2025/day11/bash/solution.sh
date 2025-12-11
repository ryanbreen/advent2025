#!/bin/bash

# Bash 3.2 compatible solution using files for graph and cache storage
# Create temporary directory for our data structures
tmpdir=$(mktemp -d)
trap "rm -rf $tmpdir" EXIT

graph_file="$tmpdir/graph.txt"
cache_file="$tmpdir/cache.txt"

# Read input and build graph file
# Format: node|neighbor1 neighbor2 neighbor3
input_file="../input.txt"
while IFS=': ' read -r node neighbors; do
    if [[ -n "$node" ]]; then
        echo "$node|$neighbors" >> "$graph_file"
    fi
done < "$input_file"

# Get neighbors for a node
get_neighbors() {
    local node="$1"
    grep "^${node}|" "$graph_file" | cut -d'|' -f2
}

# Get cached value
get_cache() {
    local key="$1"
    grep "^${key}|" "$cache_file" 2>/dev/null | cut -d'|' -f2
}

# Set cached value
set_cache() {
    local key="$1"
    local value="$2"
    echo "${key}|${value}" >> "$cache_file"
}

# Count paths from node to target with memoization
count_paths() {
    local node="$1"
    local target="$2"

    # Base case: reached target
    if [[ "$node" == "$target" ]]; then
        echo "1"
        return
    fi

    # Check cache
    local cache_key="${node}_${target}"
    local cached=$(get_cache "$cache_key")
    if [[ -n "$cached" ]]; then
        echo "$cached"
        return
    fi

    # Get neighbors
    local neighbors=$(get_neighbors "$node")

    # No neighbors means dead end
    if [[ -z "$neighbors" ]]; then
        set_cache "$cache_key" "0"
        echo "0"
        return
    fi

    # Sum paths through all neighbors
    local total=0
    for neighbor in $neighbors; do
        local paths=$(count_paths "$neighbor" "$target")
        total=$((total + paths))
    done

    # Cache and return
    set_cache "$cache_key" "$total"
    echo "$total"
}

# Part 1: Count paths from 'you' to 'out'
rm -f "$cache_file"
part1=$(count_paths "you" "out")
echo "Part 1: $part1"

# Part 2: Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
# Using the formula:
# paths(svr→dac) × paths(dac→fft) × paths(fft→out) +
# paths(svr→fft) × paths(fft→dac) × paths(dac→out)

# Clear cache once and calculate all needed path counts
# We can reuse cache entries since all paths to same target share computation
rm -f "$cache_file"
svr_to_dac=$(count_paths "svr" "dac")
dac_to_fft=$(count_paths "dac" "fft")
fft_to_out=$(count_paths "fft" "out")
svr_to_fft=$(count_paths "svr" "fft")
fft_to_dac=$(count_paths "fft" "dac")
dac_to_out=$(count_paths "dac" "out")

# Use bc for arbitrary precision arithmetic
part2=$(bc <<EOF
$svr_to_dac * $dac_to_fft * $fft_to_out + $svr_to_fft * $fft_to_dac * $dac_to_out
EOF
)

echo "Part 2: $part2"
