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
        parent1[i] = i
        rank1[i] = 0
        size1[i] = 1
        # Part 2 Union-Find (separate)
        parent2[i] = i
        rank2[i] = 0
        size2[i] = 1
    }
    num_components2 = n
}

# Process sorted pairs
{
    dist_sq = $1
    i = $2
    j = $3

    # Part 1: first 1000 connections
    if (NR <= 1000) {
        px = find1(i)
        py = find1(j)

        if (px != py) {
            # Union by rank
            if (rank1[px] < rank1[py]) {
                tmp = px; px = py; py = tmp
            }
            parent1[py] = px
            size1[px] += size1[py]
            if (rank1[px] == rank1[py]) {
                rank1[px]++
            }
        }

        if (NR == 1000) {
            # Get component sizes
            comp_count = 0
            for (k = 0; k < n; k++) {
                if (parent1[k] == k) {
                    comp_sizes[comp_count++] = size1[k]
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
        }
    }

    # Part 2: process ALL pairs from the beginning with separate Union-Find
    if (num_components2 > 1) {
        px = find2(i)
        py = find2(j)

        if (px != py) {
            # Union
            if (rank2[px] < rank2[py]) {
                tmp = px; px = py; py = tmp
            }
            parent2[py] = px
            size2[px] += size2[py]
            if (rank2[px] == rank2[py]) {
                rank2[px]++
            }

            num_components2--
            if (num_components2 == 1) {
                part2 = X[i] * X[j]
                print "Part 2: " part2
            }
        }
    }
}

# Iterative find with path compression for Part 1
function find1(x) {
    root = x
    while (parent1[root] != root) {
        root = parent1[root]
    }
    while (parent1[x] != root) {
        tmp = parent1[x]
        parent1[x] = root
        x = tmp
    }
    return root
}

# Iterative find with path compression for Part 2
function find2(x) {
    root = x
    while (parent2[root] != root) {
        root = parent2[root]
    }
    while (parent2[x] != root) {
        tmp = parent2[x]
        parent2[x] = root
        x = tmp
    }
    return root
}
'
