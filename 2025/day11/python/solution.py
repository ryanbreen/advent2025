#!/usr/bin/env python3
import sys
from functools import lru_cache

def parse_input(filename):
    """Parse input into a graph (adjacency list)."""
    graph = {}
    with open(filename) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            parts = line.split(': ')
            node = parts[0]
            neighbors = parts[1].split() if len(parts) > 1 else []
            graph[node] = neighbors
    return graph

def part1(graph):
    """Count all paths from 'you' to 'out' using memoization."""
    @lru_cache(maxsize=None)
    def count_paths(node):
        if node == 'out':
            return 1
        if node not in graph:
            return 0
        return sum(count_paths(neighbor) for neighbor in graph[node])

    return count_paths('you')

def part2(graph):
    """
    Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'.
    Since it's a DAG, paths either visit dac before fft, or fft before dac.
    """
    from functools import cache

    def count_paths_to_target(target):
        """Count paths from each node to target."""
        @cache
        def count(node):
            if node == target:
                return 1
            if node not in graph:
                return 0
            return sum(count(neighbor) for neighbor in graph[node])
        return count

    paths_to_out = count_paths_to_target('out')
    paths_to_dac = count_paths_to_target('dac')
    paths_to_fft = count_paths_to_target('fft')

    # Paths that visit dac before fft: svr -> dac -> fft -> out
    dac_before_fft = paths_to_dac('svr') * paths_to_fft('dac') * paths_to_out('fft')

    # Paths that visit fft before dac: svr -> fft -> dac -> out
    fft_before_dac = paths_to_fft('svr') * paths_to_dac('fft') * paths_to_out('dac')

    return dac_before_fft + fft_before_dac

def main():
    input_file = '../input.txt'
    if len(sys.argv) > 1:
        input_file = sys.argv[1]

    graph = parse_input(input_file)

    print(f"Part 1: {part1(graph)}")
    print(f"Part 2: {part2(graph)}")

if __name__ == '__main__':
    main()
