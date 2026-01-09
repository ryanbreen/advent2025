# Day 24: Blizzard Basin

## Problem Summary

Navigate through a valley filled with moving blizzards. You start at the entrance (top) and need to reach the exit (bottom) without being hit by any blizzard.

**Part 1**: Find the minimum time to traverse from start to end.

**Part 2**: Find the minimum time to go start → end → start → end (forgot the snacks!).

## Input Format

Grid with `#` for walls, `.` for open ground, and `^v<>` for blizzards moving in those directions:
```
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
```

## Algorithmic Approach

### Key Insight

Blizzards move in a cyclic pattern! After `lcm(height, width)` time steps, all blizzards return to their original positions. This allows us to:

1. **Precompute** all blizzard positions for one full cycle
2. **BFS** through state space `(time % period, row, col)`
3. **Memoize** visited states to avoid revisiting the same position at equivalent times

### Algorithm

1. **Parse Input**: Extract blizzard positions and directions, find start/end positions
2. **Precompute Blizzards**: For each time `t` in `[0, period)`, calculate where each blizzard will be
3. **BFS**:
   - State: `(time, row, col)`
   - Moves: wait, up, down, left, right
   - Valid if: in bounds and no blizzard at that position at that time
   - Visited key: `(time % period, row, col)` to handle cycling

### Blizzard Position Formula

For a blizzard at initial position `(r, c)` with direction `d`:
- `^`: `new_row = ((r - 1 - t) % inner_height) + 1`
- `v`: `new_row = ((r - 1 + t) % inner_height) + 1`
- `<`: `new_col = ((c - 1 - t) % inner_width) + 1`
- `>`: `new_col = ((c - 1 + t) % inner_width) + 1`

### Part 2 Extension

Simply chain three BFS calls:
1. `t1 = bfs(start, end, 0)`
2. `t2 = bfs(end, start, t1)`
3. `t3 = bfs(start, end, t2)`

The key insight is that each trip starts at the time the previous one ended.

### Complexity

- **Time**: O(period × width × height × 5) for BFS, where period = lcm(inner_h, inner_w)
- **Space**: O(period × width × height) for visited states + O(period × blizzards) for cache

## Programming Techniques Highlighted

- **State Space BFS**: Treating time as part of the state
- **Cyclic Pattern Recognition**: Using LCM to identify repeating patterns
- **Precomputation**: Trading memory for time by caching blizzard positions
- **Set Operations**: Fast O(1) lookup for collision detection

## Language-Specific Notes

### Performance Characteristics

This problem is BFS-heavy with many hash set operations. The state space is large (~600 × 100 × 120 states possible).

**Fast (< 500ms)**:
- **ARM64**: 25.5ms - Hand-optimized assembly with efficient memory layout
- **C**: 27.7ms - Simple data structures, no allocation overhead
- **Rust**: 295.0ms - HashSet performance with memory safety

**Moderate (500ms - 2s)**:
- **Go**: 424.1ms - Efficient maps
- **Java**: 617.4ms - HashMap overhead
- **C++**: 663.0ms - unordered_set overhead
- **Node.js**: 1,142.3ms - V8 optimization helps
- **PHP**: 1,171.3ms - Array-based hash tables
- **Zig**: 1,173.3ms - HashMap implementation
- **Common Lisp**: 1,227.7ms - Hash table performance
- **Python**: 1,875.6ms - Set/deque operations

**Slow (> 2s)**:
- **Clojure**: 2,340.0ms - Persistent data structures
- **Perl**: 3,899.5ms - Hash operations
- **Ruby**: 6,866.3ms - Set performance
- **ColdFusion**: 8,388.3ms - JVM overhead
- **Bash**: Very slow (AWK-based BFS)

### Implementation Notes

- Using strings as hash keys (e.g., `"time,row,col"`) is slower than tuple/struct keys
- Precomputing all blizzard positions uses more memory but saves computation
- The BFS queue can grow large (~50,000 states) in worst case

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 25.5         | 6.7         |
| C           | 27.7         | 7.7         |
| Rust        | 295.0        | 68.7        |
| Go          | 424.1        | 139.4       |
| Java        | 617.4        | 438.5       |
| C++         | 663.0        | 61.3        |
| Node.js     | 1,142.3      | 199.4       |
| PHP         | 1,171.3      | 123.6       |
| Zig         | 1,173.3      | 35.8        |
| Common Lisp | 1,227.7      | 396.0       |
| Python      | 1,875.6      | 259.2       |
| Clojure     | 2,340.0      | 1,590.0     |
| Perl        | 3,899.5      | 136.4       |
| Ruby        | 6,866.3      | 196.8       |
| ColdFusion  | 8,388.3      | 1,414.5     |
| Bash        | TBD          | TBD         |

## Answers

- Part 1: 314
- Part 2: 896
