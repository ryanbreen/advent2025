# Day 17: Clumsy Crucible

## Problem Summary

The Elves need to transport molten lava in top-heavy crucibles through a city grid. Each city block has an associated heat loss value, and the crucibles have movement constraints due to being difficult to steer. The goal is to find the path from the top-left to the bottom-right corner that minimizes total heat loss.

**Input format**: A grid of single-digit numbers representing heat loss values for each city block.

## Part 1: Normal Crucible

The normal crucible can move **at most 3 blocks** in a straight line before it must turn 90 degrees left or right. It cannot reverse direction.

**Goal**: Find the minimum heat loss path from top-left to bottom-right.

## Part 2: Ultra Crucible

The ultra crucible has different constraints:
- Must move **at least 4 blocks** in a straight line before turning
- Can move **at most 10 blocks** before it must turn
- At the destination, must have traveled at least 4 blocks in the final direction

**Goal**: Find the minimum heat loss path with ultra crucible constraints.

## Algorithmic Approach

### Key Insight

This is a **shortest path problem with state-dependent transitions**. Standard Dijkstra's algorithm won't work directly because valid moves depend on how far you've traveled in the current direction. The solution is to expand the state space to include movement history.

### State Representation

Each state is a 4-tuple: `(row, col, direction, consecutive_steps)`

- `row, col`: Current position in the grid
- `direction`: Current direction (0=right, 1=down, 2=left, 3=up, or -1 for initial state)
- `consecutive_steps`: How many steps taken in the current direction

This transforms the problem into a standard shortest path problem on an expanded state graph.

### Algorithm: Modified Dijkstra

1. Start at (0, 0) with direction=-1 and consecutive=0
2. Use a priority queue ordered by accumulated heat loss
3. For each state, try all 4 directions:
   - Skip reverse direction (can't go backwards)
   - If continuing same direction: increment consecutive, check max constraint
   - If turning: check min consecutive constraint, reset to 1
4. Mark visited states to avoid reprocessing
5. Return when reaching destination with valid consecutive count

### Data Structures

- **Priority Queue (Min-Heap)**: For efficient extraction of minimum heat loss state
- **Visited Set**: Hash set of `(row, col, direction, consecutive)` tuples
- **Direction Arrays**: `dr = [0, 1, 0, -1]`, `dc = [1, 0, -1, 0]` for movement

### Complexity

- **Time**: O(R × C × D × K × log(R × C × D × K)) where R=rows, C=cols, D=4 directions, K=max consecutive (3 or 10)
- **Space**: O(R × C × D × K) for visited states and priority queue

### Why Not BFS?

BFS finds shortest paths by number of edges, not edge weights. Since each cell has different heat loss values, we need Dijkstra's algorithm which finds minimum total weight paths.

## Language Notes

- **Fast performers**: Zig, ARM64, C - efficient hash maps and minimal overhead
- **Priority queue implementations**: Most languages have built-in heap/priority queue structures
  - C/Zig/ARM64: Custom min-heap implementation required
  - Python: `heapq` module
  - Java: `PriorityQueue` class
  - C++: `priority_queue` (note: default is max-heap, needs custom comparator)
  - Go: `container/heap` interface
- **State hashing**: String concatenation is convenient but slow; packed integers are faster
- **Memory usage**: Go and Java use more memory due to GC overhead; Clojure's persistent data structures add significant overhead
- **Slow performers**: Ruby and Perl lack efficient priority queue implementations in standard library; ColdFusion's array-based approach is extremely slow

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 49.1         | 2.8         |
| ARM64       | 77.2         | 4.8         |
| C           | 102.8        | 2.6         |
| Rust        | 178.1        | 29.4        |
| C++         | 213.6        | 38.2        |
| Go          | 415.4        | 90.8        |
| Java        | 819.9        | 200.1       |
| Common Lisp | 909.7        | 139.2       |
| PHP         | 1,026.3      | 89.5        |
| Node.js     | 1,206.8      | 142.2       |
| Python      | 1,791.6      | 140.3       |
| Clojure     | 3,055.3      | 1,374.1     |
| Ruby        | 9,760.7      | 168.3       |
| Perl        | 10,167.6     | 140.4       |
| Bash        | ~150,000     | ~10         |
| ColdFusion  | 485,505.0    | 1,319.1     |

## Answers

- Part 1: **953**
- Part 2: **1180**
