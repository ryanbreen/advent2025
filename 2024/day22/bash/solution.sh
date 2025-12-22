#!/usr/bin/env bash
set -euo pipefail

# Day 22: Monkey Market - Pseudorandom number generation

# Inline next_secret calculation to avoid subshell overhead
# Sets global variable SECRET
next_secret() {
    # Step 1: multiply by 64 (shift left 6), mix (XOR), prune (& 0xFFFFFF)
    SECRET=$(( SECRET ^ (SECRET << 6) ))
    SECRET=$(( SECRET & 0xFFFFFF ))

    # Step 2: divide by 32 (shift right 5), mix (XOR), prune
    SECRET=$(( SECRET ^ (SECRET >> 5) ))
    SECRET=$(( SECRET & 0xFFFFFF ))

    # Step 3: multiply by 2048 (shift left 11), mix (XOR), prune
    SECRET=$(( SECRET ^ (SECRET << 11) ))
    SECRET=$(( SECRET & 0xFFFFFF ))
}

part1() {
    local -a initial_secrets=("$@")
    local total=0

    for initial in "${initial_secrets[@]}"; do
        SECRET=$initial
        for ((i=0; i<2000; i++)); do
            next_secret
        done
        total=$(( total + SECRET ))
    done

    echo "$total"
}

part2() {
    local -a initial_secrets=("$@")

    # Associative array: sequence -> total bananas
    declare -A sequence_totals

    for initial in "${initial_secrets[@]}"; do
        # Generate prices and changes in a single pass
        SECRET=$initial
        local prev_price=$(( SECRET % 10 ))

        # Keep only last 4 changes (c1, c2, c3, c4)
        local c1=0 c2=0 c3=0 c4=0

        # Track first occurrence of each 4-change sequence for this buyer
        # Explicitly unset and redeclare to ensure clean state
        unset seen 2>/dev/null || true
        declare -A seen

        for ((i=0; i<2000; i++)); do
            next_secret
            local price=$(( SECRET % 10 ))
            local change=$(( price - prev_price ))

            # Shift the window
            c1=$c2
            c2=$c3
            c3=$c4
            c4=$change

            # Once we have 4 changes (i >= 3), process sequence
            if (( i >= 3 )); then
                local seq="$c1,$c2,$c3,$c4"

                if [[ ! -v seen["$seq"] ]]; then
                    seen["$seq"]=1
                    # Price is the current price (after 4 changes)
                    sequence_totals["$seq"]=$(( ${sequence_totals["$seq"]:-0} + price ))
                fi
            fi

            prev_price=$price
        done
    done

    # Find maximum value
    local max=0
    for value in "${sequence_totals[@]}"; do
        if (( value > max )); then
            max=$value
        fi
    done

    echo "$max"
}

main() {
    # Read input file (handle case when script is called with bash solution.sh)
    local script_dir
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local input_file="${script_dir}/../input.txt"

    # Read initial secrets into array
    local -a initial_secrets=()
    while IFS= read -r line; do
        [[ -n "$line" ]] && initial_secrets+=("$line")
    done < "$input_file"

    echo "Part 1: $(part1 "${initial_secrets[@]}")"
    echo "Part 2: $(part2 "${initial_secrets[@]}")"
}

main
