#!/usr/bin/env python3
import os
from collections import deque
from math import gcd

def parse_input(text):
    """Parse the map and extract blizzard positions."""
    lines = text.strip().split('\n')
    height = len(lines)
    width = len(lines[0])
    
    # Inner dimensions (excluding walls)
    inner_h = height - 2
    inner_w = width - 2
    
    blizzards = []
    for r, line in enumerate(lines):
        for c, ch in enumerate(line):
            if ch in '^v<>':
                blizzards.append((r, c, ch))
    
    # Find start and end positions
    start = (0, lines[0].index('.'))
    end = (height - 1, lines[-1].index('.'))
    
    return blizzards, height, width, inner_h, inner_w, start, end

def lcm(a, b):
    return a * b // gcd(a, b)

def get_blizzard_positions(blizzards, inner_h, inner_w, time):
    """Get all blizzard positions at a given time."""
    positions = set()
    for r, c, direction in blizzards:
        # Adjust to inner coordinates (subtract 1 for wall)
        ir, ic = r - 1, c - 1
        
        if direction == '^':
            nr = (ir - time) % inner_h
        elif direction == 'v':
            nr = (ir + time) % inner_h
        elif direction == '<':
            nc = (ic - time) % inner_w
            nr = ir
        elif direction == '>':
            nc = (ic + time) % inner_w
            nr = ir
        
        if direction in '^v':
            nc = ic
        
        # Convert back to full coordinates
        positions.add((nr + 1, nc + 1))
    
    return positions

def bfs(blizzards, height, width, inner_h, inner_w, start, end, start_time=0):
    """BFS to find shortest path avoiding blizzards."""
    period = lcm(inner_h, inner_w)
    
    # Precompute blizzard positions for all times in one period
    blizzard_cache = {}
    for t in range(period):
        blizzard_cache[t] = get_blizzard_positions(blizzards, inner_h, inner_w, t)
    
    # BFS: state = (time, row, col)
    queue = deque([(start_time, start[0], start[1])])
    visited = set()
    visited.add((start_time % period, start[0], start[1]))
    
    directions = [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]  # wait, up, down, left, right
    
    while queue:
        time, r, c = queue.popleft()
        
        if (r, c) == end:
            return time
        
        next_time = time + 1
        next_blizzards = blizzard_cache[next_time % period]
        
        for dr, dc in directions:
            nr, nc = r + dr, c + dc
            
            # Check bounds
            if (nr, nc) == start or (nr, nc) == end:
                pass  # Always valid
            elif nr <= 0 or nr >= height - 1 or nc <= 0 or nc >= width - 1:
                continue  # Wall
            
            # Check blizzards
            if (nr, nc) in next_blizzards:
                continue
            
            state = (next_time % period, nr, nc)
            if state not in visited:
                visited.add(state)
                queue.append((next_time, nr, nc))
    
    return -1  # No path found

def part1(text):
    """Find shortest path from start to end."""
    blizzards, height, width, inner_h, inner_w, start, end = parse_input(text)
    return bfs(blizzards, height, width, inner_h, inner_w, start, end, 0)

def part2(text):
    """Find shortest path: start -> end -> start -> end."""
    blizzards, height, width, inner_h, inner_w, start, end = parse_input(text)
    
    # Trip 1: start to end
    t1 = bfs(blizzards, height, width, inner_h, inner_w, start, end, 0)
    
    # Trip 2: end to start
    t2 = bfs(blizzards, height, width, inner_h, inner_w, end, start, t1)
    
    # Trip 3: start to end again
    t3 = bfs(blizzards, height, width, inner_h, inner_w, start, end, t2)
    
    return t3

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_file = os.path.join(script_dir, '..', 'input.txt')
    
    with open(input_file) as f:
        text = f.read()
    
    print('Part 1:', part1(text))
    print('Part 2:', part2(text))

if __name__ == '__main__':
    main()
