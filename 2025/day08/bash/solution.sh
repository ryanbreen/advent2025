#!/bin/bash

# Day 8: Playground - Bash solution (Optimized with AWK)
# Union-Find algorithm to connect 3D points

INPUT="${1:-../input.txt}"

# Generate sorted pairs using AWK and sort command
sorted_pairs=$(awk -F',' '
{
    X[NR-1] = $1
    Y[NR-1] = $2
    Z[NR-1] = $3
}
END {
    n = NR
    for (i = 0; i < n; i++) {
        for (j = i + 1; j < n; j++) {
            dx = X[i] - X[j]
            dy = Y[i] - Y[j]
            dz = Z[i] - Z[j]
            dist_sq = dx*dx + dy*dy + dz*dz
            print dist_sq, i, j
        }
    }
}
' "$INPUT" | sort -n)

# Process with AWK for both parts
echo "$sorted_pairs" | awk -v input="$INPUT" '
BEGIN {
    # Read points for X coordinates (needed for Part 2)
    while ((getline < input) > 0) {
        split($0, coords, ",")
        X[n++] = coords[1]
    }
    close(input)

    # Initialize Union-Find for Part 1
    for (i = 0; i < n; i++) {
        parent[i] = i
        rank[i] = 0
        size[i] = 1
    }
}

# Process sorted pairs
{
    dist_sq = $1
    i = $2
    j = $3

    if (NR <= 1000) {
        # Part 1: first 1000 connections
        px = find(i, parent)
        py = find(j, parent)

        if (px != py) {
            # Union by rank
            if (rank[px] < rank[py]) {
                tmp = px; px = py; py = tmp
            }
            parent[py] = px
            size[px] += size[py]
            if (rank[px] == rank[py]) {
                rank[px]++
            }
        }

        if (NR == 1000) {
            # Get component sizes
            comp_count = 0
            for (k = 0; k < n; k++) {
                if (parent[k] == k) {
                    comp_sizes[comp_count++] = size[k]
                }
            }

            # Sort component sizes (bubble sort for simplicity)
            for (k = 0; k < comp_count; k++) {
                for (m = k + 1; m < comp_count; m++) {
                    if (comp_sizes[k] < comp_sizes[m]) {
                        tmp = comp_sizes[k]
                        comp_sizes[k] = comp_sizes[m]
                        comp_sizes[m] = tmp
                    }
                }
            }

            part1 = comp_sizes[0] * comp_sizes[1] * comp_sizes[2]
            print "Part 1: " part1

            # Reset for Part 2
            for (k = 0; k < n; k++) {
                parent[k] = k
                rank[k] = 0
                size[k] = 1
            }
            num_components = n
        }
    }

    # Part 2: continue from connection 1001 onwards
    if (NR <= 1000) next

    px = find(i, parent)
    py = find(j, parent)

    if (px != py) {
        # Union
        if (rank[px] < rank[py]) {
            tmp = px; px = py; py = tmp
        }
        parent[py] = px
        size[px] += size[py]
        if (rank[px] == rank[py]) {
            rank[px]++
        }

        num_components--
        if (num_components == 1) {
            part2 = X[i] * X[j]
            print "Part 2: " part2
            exit
        }
    }
}

# Iterative find with path compression
function find(x, parent) {
    root = x
    while (parent[root] != root) {
        root = parent[root]
    }
    # Path compression
    while (parent[x] != root) {
        tmp = parent[x]
        parent[x] = root
        x = tmp
    }
    return root
}
'
