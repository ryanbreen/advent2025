#!/usr/bin/env python3
import os
import re
from collections import deque
from functools import lru_cache

def parse_input(text):
    """Parse valves and tunnels."""
    valves = {}
    tunnels = {}

    pattern = r'Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)'
    for line in text.strip().split('\n'):
        match = re.match(pattern, line)
        name, rate, neighbors = match.groups()
        valves[name] = int(rate)
        tunnels[name] = neighbors.split(', ')

    return valves, tunnels

def compute_distances(valves, tunnels):
    """Compute shortest distances between all relevant valves using BFS."""
    # Only care about valves with flow > 0 plus starting valve AA
    relevant = ['AA'] + [v for v in valves if valves[v] > 0]
    distances = {}

    for start in relevant:
        distances[start] = {}
        queue = deque([(start, 0)])
        visited = {start}

        while queue:
            curr, dist = queue.popleft()
            if curr in relevant and curr != start:
                distances[start][curr] = dist

            for neighbor in tunnels[curr]:
                if neighbor not in visited:
                    visited.add(neighbor)
                    queue.append((neighbor, dist + 1))

    return distances

def part1(text):
    """Find maximum pressure release in 30 minutes."""
    valves, tunnels = parse_input(text)
    distances = compute_distances(valves, tunnels)

    # Only consider valves with positive flow
    valuable = frozenset(v for v in valves if valves[v] > 0)

    @lru_cache(maxsize=None)
    def dfs(pos, time_left, opened):
        if time_left <= 0:
            return 0

        best = 0
        for next_valve in valuable - opened:
            # Time to move there and open it
            time_cost = distances[pos][next_valve] + 1
            if time_cost < time_left:
                new_time = time_left - time_cost
                pressure = valves[next_valve] * new_time
                best = max(best, pressure + dfs(next_valve, new_time, opened | {next_valve}))

        return best

    return dfs('AA', 30, frozenset())

def part2(text):
    """Find maximum pressure with elephant helper (26 minutes each)."""
    valves, tunnels = parse_input(text)
    distances = compute_distances(valves, tunnels)

    valuable = [v for v in valves if valves[v] > 0]

    # Compute max pressure for each subset of valves opened (by one actor)
    def max_pressure_for_subset(subset):
        subset = frozenset(subset)

        @lru_cache(maxsize=None)
        def dfs(pos, time_left, opened):
            if time_left <= 0:
                return 0

            best = 0
            for next_valve in subset - opened:
                time_cost = distances[pos][next_valve] + 1
                if time_cost < time_left:
                    new_time = time_left - time_cost
                    pressure = valves[next_valve] * new_time
                    best = max(best, pressure + dfs(next_valve, new_time, opened | {next_valve}))

            return best

        return dfs('AA', 26, frozenset())

    # Generate all subsets and compute max pressure
    n = len(valuable)
    max_scores = {}

    for mask in range(1 << n):
        subset = frozenset(valuable[i] for i in range(n) if mask & (1 << i))
        max_scores[mask] = max_pressure_for_subset(subset)

    # Find best partition where you and elephant open disjoint sets
    best = 0
    full_mask = (1 << n) - 1
    for mask in range(1 << n):
        complement = full_mask ^ mask
        if mask <= complement:  # Avoid counting same partition twice
            best = max(best, max_scores[mask] + max_scores[complement])

    return best

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')

    with open(input_file) as f:
        text = f.read()

    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
