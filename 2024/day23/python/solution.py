#!/usr/bin/env python3
from collections import defaultdict

def parse_input(filename):
    """Parse the network connections into an adjacency set."""
    graph = defaultdict(set)
    with open(filename) as f:
        for line in f:
            a, b = line.strip().split('-')
            graph[a].add(b)
            graph[b].add(a)
    return graph

def find_triangles(graph):
    """Find all triangles (sets of 3 interconnected nodes)."""
    triangles = set()
    for a in graph:
        for b in graph[a]:
            if a < b:  # Only process each edge once
                # Find common neighbors
                for c in graph[a] & graph[b]:
                    # Use sorted tuple to avoid duplicate triangles
                    tri = tuple(sorted([a, b, c]))
                    triangles.add(tri)
    return triangles

def part1(graph):
    """Count triangles containing at least one node starting with 't'."""
    triangles = find_triangles(graph)
    count = 0
    for tri in triangles:
        if any(node.startswith('t') for node in tri):
            count += 1
    return count

def bron_kerbosch(graph, r, p, x, cliques):
    """Bron-Kerbosch algorithm to find all maximal cliques."""
    if not p and not x:
        cliques.append(r)
        return

    # Use pivot to reduce branching
    pivot = max(p | x, key=lambda v: len(graph[v] & p))
    for v in p - graph[pivot]:
        bron_kerbosch(
            graph,
            r | {v},
            p & graph[v],
            x & graph[v],
            cliques
        )
        p = p - {v}
        x = x | {v}

def part2(graph):
    """Find the largest clique (fully connected subgraph)."""
    cliques = []
    all_nodes = set(graph.keys())
    bron_kerbosch(graph, set(), all_nodes, set(), cliques)

    # Find the largest clique
    largest = max(cliques, key=len)
    # Return sorted, comma-joined password
    return ','.join(sorted(largest))

def main():
    graph = parse_input('../input.txt')

    print('Part 1:', part1(graph))
    print('Part 2:', part2(graph))

if __name__ == '__main__':
    main()
