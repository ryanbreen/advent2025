# Day 12: Hill Climbing Algorithm

## Problem Summary

Navigate a heightmap grid from start to end using BFS. Elevation is represented by letters a-z, where 'a' is lowest and 'z' is highest.

**Part 1**: Find shortest path from `S` (elevation 'a') to `E` (elevation 'z').

**Part 2**: Find shortest path from ANY cell with elevation 'a' to `E`.

## Input Format

Grid of lowercase letters with special markers:
```
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
```
- `S` = start position (elevation 'a')
- `E` = end position (elevation 'z')

## Algorithmic Approach

### Key Insight

This is a classic BFS (Breadth-First Search) shortest path problem with movement constraints. BFS guarantees the shortest path in an unweighted graph.

### Movement Rules

- Can move up, down, left, right (4 directions)
- Can only move to a cell if its elevation is **at most 1 higher** than current
- Can move to any lower elevation (can go down any amount)

### Part 1 Algorithm

```
queue = [(start_row, start_col, distance=0)]
visited = {start}

while queue not empty:
    r, c, dist = queue.pop_front()
    if (r, c) == end: return dist

    for each neighbor (nr, nc):
        if valid(nr, nc) and not visited and height[nr][nc] <= height[r][c] + 1:
            visited.add((nr, nc))
            queue.push((nr, nc, dist + 1))
```

### Part 2 Optimization

Instead of running BFS from each 'a' cell separately, start with ALL 'a' cells in the initial queue at distance 0. This finds the shortest path from any 'a' to E in a single BFS pass.

### Data Structures

- **Queue**: For BFS frontier (FIFO)
- **Visited Set**: Track explored cells (hash set or 2D boolean array)
- **Grid**: 2D array of characters/heights

### Complexity

- Time: O(rows × cols) - each cell visited at most once
- Space: O(rows × cols) - for visited set and queue

## Programming Techniques Highlighted

- **BFS**: Shortest path in unweighted graph
- **Multi-source BFS**: Part 2 optimization with multiple start points
- **Grid Navigation**: 4-directional movement with bounds checking
- **Set Operations**: Efficient visited tracking

## Language-Specific Notes

### Fast Performers (5-8ms)
- **Zig, ARM64, C, C++, Rust, Go**: Efficient queue and set implementations
- BFS is straightforward to implement efficiently in systems languages

### Mid-Tier (23-75ms)
- **Perl**: Surprisingly fast with native arrays
- **Python**: deque is efficient for BFS
- **Common Lisp**: Good hash table performance
- **Java, Node.js, PHP, Ruby**: Standard performance

### Slow (450ms-2.6s)
- **Clojure**: Immutable data structure overhead
- **Bash**: Array operations are slow, associative arrays for visited
- **ColdFusion**: CFML runtime overhead

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 5.6          | 1.9         |
| ARM64 asm   | 5.7          | 1.9         |
| C           | 6.9          | 1.9         |
| C++         | 6.9          | 1.9         |
| Rust        | 7.3          | 1.9         |
| Go          | 7.7          | 5.1         |
| Perl        | 22.8         | 6.4         |
| Python      | 28.1         | 15.2        |
| Common Lisp | 30.5         | 43.8        |
| Java        | 49.6         | 46.1        |
| Node.js     | 53.1         | 48.1        |
| PHP         | 55.7         | 26.8        |
| Ruby        | 74.5         | 28.5        |
| Clojure     | 450.8        | 143.4       |
| Bash        | 2,531.3      | 7.0         |
| ColdFusion  | 2,653.1      | 1,092.3     |

## Answers

- Part 1: 339
- Part 2: 332
