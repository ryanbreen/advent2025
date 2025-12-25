#!/usr/bin/env bash

# Parse input and build adjacency lists
declare -A graph
declare -a all_nodes

while IFS='-' read -r a b; do
    # Build adjacency lists
    graph["$a"]+="$b "
    graph["$b"]+="$a "

    # Track all unique nodes
    all_nodes+=("$a" "$b")
done < ../input.txt

# Get unique nodes
all_nodes=($(printf '%s\n' "${all_nodes[@]}" | sort -u))

# Function to check if two nodes are connected
is_connected() {
    local a=$1
    local b=$2
    [[ " ${graph[$a]} " == *" $b "* ]]
}

# Part 1: Find all triangles with at least one node starting with 't'
part1() {
    declare -A triangles
    local count=0

    # Iterate through all nodes
    for a in "${all_nodes[@]}"; do
        # Get neighbors of a
        local neighbors_a=(${graph[$a]})

        for b in "${neighbors_a[@]}"; do
            # Only process each edge once (a < b lexicographically)
            if [[ "$a" < "$b" ]]; then
                # Find common neighbors of a and b
                local neighbors_b=(${graph[$b]})

                for c in "${neighbors_a[@]}"; do
                    # Check if c is also a neighbor of b
                    if [[ " ${graph[$b]} " == *" $c "* ]]; then
                        # We have a triangle: a, b, c
                        # Sort the three nodes to create a unique key
                        local tri=($(echo -e "$a\n$b\n$c" | sort))
                        local key="${tri[0]},${tri[1]},${tri[2]}"

                        # Add to set (avoid duplicates)
                        if [[ -z "${triangles[$key]}" ]]; then
                            triangles[$key]=1

                            # Check if at least one starts with 't'
                            if [[ "${tri[0]}" == t* ]] || [[ "${tri[1]}" == t* ]] || [[ "${tri[2]}" == t* ]]; then
                                ((count++))
                            fi
                        fi
                    fi
                done
            fi
        done
    done

    echo "$count"
}

# Part 2: Find maximum clique using Bron-Kerbosch
# For Bash, we'll use a simpler approach: grow cliques incrementally

part2() {
    # Start with all nodes as potential cliques
    local max_clique=""
    local max_size=0

    # Try starting from each node
    for start in "${all_nodes[@]}"; do
        # Build a clique starting from this node
        local clique=($start)
        local candidates=(${graph[$start]})

        # Keep adding nodes that are connected to all current clique members
        while true; do
            local added=0

            for candidate in "${candidates[@]}"; do
                # Check if candidate is connected to all nodes in clique
                local connected=1
                for member in "${clique[@]}"; do
                    if ! is_connected "$candidate" "$member"; then
                        connected=0
                        break
                    fi
                done

                if [[ $connected -eq 1 ]]; then
                    # Add to clique
                    clique+=("$candidate")

                    # Update candidates: intersection with neighbors of new node
                    local new_candidates=()
                    for c in "${candidates[@]}"; do
                        if [[ "$c" != "$candidate" ]] && [[ " ${graph[$candidate]} " == *" $c "* ]]; then
                            new_candidates+=("$c")
                        fi
                    done
                    candidates=("${new_candidates[@]}")

                    added=1
                    break
                fi
            done

            # If no node was added, we've found a maximal clique
            if [[ $added -eq 0 ]]; then
                break
            fi
        done

        # Check if this is the largest clique found
        if [[ ${#clique[@]} -gt $max_size ]]; then
            max_size=${#clique[@]}
            max_clique=($(printf '%s\n' "${clique[@]}" | sort))
        fi
    done

    # Join with commas
    local result=""
    for node in "${max_clique[@]}"; do
        if [[ -z "$result" ]]; then
            result="$node"
        else
            result="$result,$node"
        fi
    done

    echo "$result"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
