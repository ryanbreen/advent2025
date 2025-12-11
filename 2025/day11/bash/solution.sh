#!/opt/homebrew/bin/bash

# Requires Bash 4.0+ for associative arrays
# macOS ships with Bash 3.2 by default - install modern bash via homebrew:
# brew install bash

# Check bash version
if ((BASH_VERSINFO[0] < 4)); then
    echo "Error: This script requires Bash 4.0 or higher for associative arrays." >&2
    echo "Current version: $BASH_VERSION" >&2
    echo "On macOS, install with: brew install bash" >&2
    echo "Then run with: /opt/homebrew/bin/bash solution.sh" >&2
    exit 1
fi

# Declare associative arrays for graph and cache
declare -A graph
declare -A cache

# Read input and build graph
input_file="../input.txt"
while IFS=': ' read -r node neighbors; do
    if [[ -n "$node" ]]; then
        graph["$node"]="$neighbors"
    fi
done < "$input_file"

# Global variable for return value (avoids subshell overhead)
_result=0

# Count paths from node to target with memoization
count_paths() {
    local node="$1"
    local target="$2"

    # Base case: reached target
    if [[ "$node" == "$target" ]]; then
        _result=1
        return
    fi

    # Check cache
    local cache_key="${node}_${target}"
    if [[ -n "${cache[$cache_key]+x}" ]]; then
        _result="${cache[$cache_key]}"
        return
    fi

    # Get neighbors
    local neighbors="${graph[$node]}"

    # No neighbors means dead end
    if [[ -z "$neighbors" ]]; then
        cache["$cache_key"]=0
        _result=0
        return
    fi

    # Sum paths through all neighbors
    local total=0
    local neighbor_result
    for neighbor in $neighbors; do
        count_paths "$neighbor" "$target"
        neighbor_result=$_result
        total=$((total + neighbor_result))
    done

    # Cache and return
    cache["$cache_key"]=$total
    _result=$total
}

# Part 1: Count paths from 'you' to 'out'
cache=()  # Clear cache
count_paths "you" "out"
part1=$_result
echo "Part 1: $part1"

# Part 2: Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
# Using the formula:
# paths(svr→dac) × paths(dac→fft) × paths(fft→out) +
# paths(svr→fft) × paths(fft→dac) × paths(dac→out)

# Clear cache and calculate all needed path counts
# We can reuse cache entries since all paths to same target share computation
cache=()  # Clear cache
count_paths "svr" "dac"
svr_to_dac=$_result
count_paths "dac" "fft"
dac_to_fft=$_result
count_paths "fft" "out"
fft_to_out=$_result
count_paths "svr" "fft"
svr_to_fft=$_result
count_paths "fft" "dac"
fft_to_dac=$_result
count_paths "dac" "out"
dac_to_out=$_result

# Use bc for arbitrary precision arithmetic
part2=$(bc <<EOF
$svr_to_dac * $dac_to_fft * $fft_to_out + $svr_to_fft * $fft_to_dac * $dac_to_out
EOF
)

echo "Part 2: $part2"
