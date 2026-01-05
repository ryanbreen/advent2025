#!/usr/bin/env bash

# Advent of Code 2023 - Day 25: Snowverload
# Find the minimum cut of 3 edges that divides the graph into two components.
#
# Uses edge betweenness centrality: edges that form the cut between two large
# components will have high betweenness (many shortest paths pass through them).

declare -A graph
declare -a all_nodes
declare -A edge_betweenness

# Parse input and build adjacency list
parse_input() {
    local input_file="../input.txt"

    while IFS=': ' read -r left right; do
        [[ -z "$left" ]] && continue

        # Add left node to list if not seen
        if [[ ! -v graph["$left"] ]]; then
            all_nodes+=("$left")
        fi

        # Process neighbors
        for neighbor in $right; do
            # Add neighbor to list if not seen
            if [[ ! -v graph["$neighbor"] ]]; then
                all_nodes+=("$neighbor")
            fi

            # Add bidirectional edges
            if [[ -v graph["$left"] ]]; then
                graph["$left"]="${graph[$left]} $neighbor"
            else
                graph["$left"]="$neighbor"
            fi

            if [[ -v graph["$neighbor"] ]]; then
                graph["$neighbor"]="${graph[$neighbor]} $left"
            else
                graph["$neighbor"]="$left"
            fi
        done
    done < "$input_file"
}

# BFS to find component size, excluding certain edges
bfs_component_size() {
    local start="$1"
    shift
    local -A excluded_edges
    for edge in "$@"; do
        excluded_edges["$edge"]=1
    done

    local -A visited
    local -a queue
    visited["$start"]=1
    queue=("$start")
    local count=1

    while [[ ${#queue[@]} -gt 0 ]]; do
        local node="${queue[0]}"
        queue=("${queue[@]:1}")

        for neighbor in ${graph[$node]}; do
            # Create edge key (sorted)
            local edge
            if [[ "$node" < "$neighbor" ]]; then
                edge="$node-$neighbor"
            else
                edge="$neighbor-$node"
            fi

            if [[ ! -v visited["$neighbor"] ]] && [[ ! -v excluded_edges["$edge"] ]]; then
                visited["$neighbor"]=1
                queue+=("$neighbor")
                ((count++))
            fi
        done
    done

    echo "$count"
}

# Simple BFS to compute edge usage (simplified betweenness)
compute_edge_betweenness() {
    local sample_size=50  # Reduced for Bash performance
    local total_nodes=${#all_nodes[@]}

    # Sample nodes (use evenly spaced samples for better coverage)
    local -a sample_nodes
    local step=$((total_nodes / sample_size))
    [[ $step -eq 0 ]] && step=1

    for ((i = 0; i < total_nodes; i += step)); do
        sample_nodes+=("${all_nodes[$i]}")
        [[ ${#sample_nodes[@]} -ge $sample_size ]] && break
    done

    # For each sampled source node, do BFS and count edge usage
    local idx=0
    for source in "${sample_nodes[@]}"; do
        ((idx++))
        [[ $((idx % 10)) -eq 0 ]] && >&2 echo "Sampling node $idx/${#sample_nodes[@]}..."

        local -A dist
        local -a queue

        dist["$source"]=0
        queue=("$source")

        # BFS
        while [[ ${#queue[@]} -gt 0 ]]; do
            local node="${queue[0]}"
            queue=("${queue[@]:1}")
            local d=${dist[$node]}

            for neighbor in ${graph[$node]}; do
                if [[ ! -v dist["$neighbor"] ]]; then
                    dist["$neighbor"]=$((d + 1))
                    queue+=("$neighbor")

                    # Count edge usage (simplified - just count once per shortest path)
                    local edge
                    if [[ "$node" < "$neighbor" ]]; then
                        edge="$node-$neighbor"
                    else
                        edge="$neighbor-$node"
                    fi

                    if [[ -v edge_betweenness["$edge"] ]]; then
                        edge_betweenness["$edge"]=$((edge_betweenness[$edge] + 1))
                    else
                        edge_betweenness["$edge"]=1
                    fi
                fi
            done
        done
    done
    >&2 echo "Betweenness computation complete."
}

# Find the 3 edges to cut
find_cut_edges() {
    >&2 echo "Computing edge betweenness..."
    # Compute edge betweenness
    compute_edge_betweenness

    >&2 echo "Sorting edges..."
    # Sort edges by betweenness (highest first)
    local -a sorted_edges
    while IFS= read -r line; do
        sorted_edges+=("${line#* }")
    done < <(for edge in "${!edge_betweenness[@]}"; do
        echo "${edge_betweenness[$edge]} $edge"
    done | sort -rn)

    # Take top 25 candidates
    local -a top_edges
    local max_candidates=$((${#sorted_edges[@]} < 25 ? ${#sorted_edges[@]} : 25))
    for ((i = 0; i < max_candidates; i++)); do
        top_edges+=("${sorted_edges[$i]}")
    done

    >&2 echo "Testing ${#top_edges[@]} candidate edges..."
    local total_nodes=${#all_nodes[@]}

    # Try all combinations of 3 edges from top candidates
    local tested=0
    for ((i = 0; i < ${#top_edges[@]}; i++)); do
        for ((j = i + 1; j < ${#top_edges[@]}; j++)); do
            for ((k = j + 1; k < ${#top_edges[@]}; k++)); do
                ((tested++))
                [[ $((tested % 50)) -eq 0 ]] && >&2 echo "Tested $tested combinations..."

                local size1
                size1=$(bfs_component_size "${all_nodes[0]}" "${top_edges[$i]}" "${top_edges[$j]}" "${top_edges[$k]}")

                if [[ $size1 -lt $total_nodes ]]; then
                    # Graph is disconnected!
                    local size2=$((total_nodes - size1))
                    >&2 echo "Found solution! Component sizes: $size1 and $size2"
                    echo $((size1 * size2))
                    return
                fi
            done
        done
    done

    echo "No solution found"
}

part1() {
    find_cut_edges
}

part2() {
    echo "Push the big red button!"
}

# Main
parse_input
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
