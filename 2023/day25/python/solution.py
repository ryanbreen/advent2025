#!/usr/bin/env python3
"""
Advent of Code 2023 - Day 25: Snowverload
Find the minimum cut of 3 edges that divides the graph into two components.

Uses edge betweenness centrality: edges that form the cut between two large
components will have high betweenness (many shortest paths pass through them).
"""
from collections import defaultdict, deque


def parse_input(filename):
    """Parse the input file into an adjacency list representation."""
    graph = defaultdict(set)
    with open(filename) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            left, right = line.split(': ')
            neighbors = right.split()
            for neighbor in neighbors:
                graph[left].add(neighbor)
                graph[neighbor].add(left)
    return dict(graph)


def bfs_component_size(graph, start, excluded_edges):
    """BFS to find component size, ignoring excluded edges."""
    visited = {start}
    queue = deque([start])

    while queue:
        node = queue.popleft()
        for neighbor in graph[node]:
            edge = tuple(sorted([node, neighbor]))
            if neighbor not in visited and edge not in excluded_edges:
                visited.add(neighbor)
                queue.append(neighbor)

    return len(visited)


def compute_edge_betweenness(graph, sample_nodes=None):
    """
    Compute approximate edge betweenness centrality.
    Higher values indicate edges that many shortest paths pass through.
    """
    edge_count = defaultdict(int)
    nodes = list(graph.keys())

    # Sample nodes for efficiency
    if sample_nodes and len(nodes) > sample_nodes:
        import random
        random.seed(42)
        nodes = random.sample(nodes, sample_nodes)

    for source in nodes:
        # BFS to find shortest paths
        dist = {source: 0}
        pred = defaultdict(list)  # Predecessors on shortest paths
        queue = deque([source])

        while queue:
            node = queue.popleft()
            for neighbor in graph[node]:
                if neighbor not in dist:
                    dist[neighbor] = dist[node] + 1
                    pred[neighbor].append(node)
                    queue.append(neighbor)
                elif dist[neighbor] == dist[node] + 1:
                    pred[neighbor].append(node)

        # Backtrack to count edge usage
        # Number of shortest paths to each node
        num_paths = defaultdict(float)
        num_paths[source] = 1.0
        for node in sorted(dist.keys(), key=lambda x: dist[x]):
            for p in pred[node]:
                num_paths[node] += num_paths[p]

        # Accumulate edge betweenness (reverse BFS order)
        dependency = defaultdict(float)
        for node in sorted(dist.keys(), key=lambda x: -dist[x]):
            for p in pred[node]:
                edge = tuple(sorted([p, node]))
                # Weight by path fraction
                frac = num_paths[p] / num_paths[node]
                contrib = frac * (1 + dependency[node])
                edge_count[edge] += contrib
                dependency[p] += contrib

    return edge_count


def find_cut_edges(graph):
    """Find the 3 edges to cut using edge betweenness."""
    # Compute edge betweenness with sampling for speed
    edge_betweenness = compute_edge_betweenness(graph, sample_nodes=100)

    # Sort edges by betweenness (highest first)
    sorted_edges = sorted(edge_betweenness.items(), key=lambda x: -x[1])

    total_nodes = len(graph)

    # Try removing top candidate edges
    # We need to find 3 edges that disconnect the graph
    top_edges = [e for e, _ in sorted_edges[:20]]  # Check top 20 candidates

    for i in range(len(top_edges)):
        for j in range(i + 1, len(top_edges)):
            for k in range(j + 1, len(top_edges)):
                excluded = {top_edges[i], top_edges[j], top_edges[k]}
                start = next(iter(graph.keys()))
                size1 = bfs_component_size(graph, start, excluded)

                if size1 < total_nodes:
                    # Graph is disconnected!
                    size2 = total_nodes - size1
                    return size1 * size2

    return None


def part1(filename):
    """Solve Part 1: Find the 3-edge cut and return product of component sizes."""
    graph = parse_input(filename)
    return find_cut_edges(graph)


def part2(filename):
    """
    Part 2: Day 25 Part 2 is traditionally unlocked by having 49 stars.
    There's no computation needed - just push the button!
    """
    return "Push the big red button!"


if __name__ == '__main__':
    import os
    input_file = os.path.join(os.path.dirname(__file__), '..', 'input.txt')

    print('Part 1:', part1(input_file))
    print('Part 2:', part2(input_file))
