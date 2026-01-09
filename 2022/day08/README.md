# Day 8: Treetop Tree House

## Problem Summary

The Elves want to build a tree house in a grid of trees. Each tree has a height (0-9). You need to analyze visibility from outside the grid and scenic potential from within.

**Part 1**: Count how many trees are visible from outside the grid. A tree is visible if all trees between it and any edge are shorter.

**Part 2**: Find the maximum "scenic score" - the product of viewing distances in all four directions from any tree.

## Input Format

A grid of single-digit numbers representing tree heights:
```
30373
25512
65332
33549
35390
```

## Algorithmic Approach

### Part 1: Visibility Count

For each tree at position (r, c):
1. Check visibility from LEFT: all trees at (r, 0..c-1) are shorter
2. Check visibility from RIGHT: all trees at (r, c+1..cols) are shorter
3. Check visibility from TOP: all trees at (0..r-1, c) are shorter
4. Check visibility from BOTTOM: all trees at (r+1..rows, c) are shorter
5. If visible from ANY direction, count it

Edge trees are automatically visible.

### Part 2: Scenic Score

For each tree at position (r, c) with height h:
1. Look LEFT: count trees until hitting height >= h or edge
2. Look RIGHT: count trees until hitting height >= h or edge
3. Look UP: count trees until hitting height >= h or edge
4. Look DOWN: count trees until hitting height >= h or edge
5. Score = left × right × up × down

Find the maximum score across all trees.

### Time Complexity

- O(n² × m) for both parts where n = rows, m = cols
- Each tree requires O(n + m) checks in worst case
- Total: O(n² × m + n × m²) ≈ O(n³) for square grids

### Space Complexity

- O(n × m) for storing the grid

## Programming Techniques Highlighted

- **2D Grid Traversal**: Classic row/column iteration patterns
- **Early Termination**: Stop checking direction once visibility is blocked
- **Directional Scanning**: Four-direction iteration patterns (−1/0/+1 deltas)
- **Short-circuit Evaluation**: For visibility, return true as soon as any direction is clear

## Language-Specific Notes

### Fast Implementations (6ms)
- **C, C++, Rust, Zig, ARM64**: Simple nested loops with array indexing
- All compiled languages perform nearly identically due to straightforward memory access patterns

### Scripting Languages (35-85ms)
- **Common Lisp**: 2D arrays with `aref` are efficient
- **Java/Python/Node.js/Perl**: Similar performance tier around 44-50ms
- **PHP/Go/Ruby**: Slightly slower at 64-85ms

### Extremely Slow (Bash: ~189 seconds)
- **Bash**: Shell string manipulation is extremely slow for O(n³) algorithms
- Each character access requires substring extraction
- This problem shows Bash's fundamental unsuitability for compute-heavy tasks

### Heavy Runtimes
- **Clojure**: 507ms - immutable data structure overhead significant for grid operations
- **ColdFusion**: 2.6 seconds - typical VM overhead

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.8          | 1.9         |
| C++         | 6.0          | 1.9         |
| Zig         | 6.0          | 1.9         |
| Rust        | 6.0          | 1.9         |
| ARM64 asm   | 6.1          | 1.9         |
| Common Lisp | 35.4         | 44.4        |
| Java        | 43.9         | 47.2        |
| Python      | 47.6         | 15.1        |
| Node.js     | 48.4         | 42.8        |
| Perl        | 49.6         | 6.9         |
| PHP         | 63.8         | 25.6        |
| Go          | 73.7         | 27.1        |
| Ruby        | 84.7         | 28.2        |
| Clojure     | 507.1        | 204.4       |
| ColdFusion  | 2,633.3      | 1,127.9     |
| Bash        | 188,810      | 6.9         |

## Answers

- Part 1: 1798
- Part 2: 259308
