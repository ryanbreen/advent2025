# Day 5: Hydrothermal Venture

## Problem Summary

You encounter hydrothermal vents on the ocean floor that form lines. Given a list of line segments, count points where multiple lines overlap.

**Part 1**: Consider only horizontal and vertical lines. Count points where at least 2 lines overlap.

**Part 2**: Include 45-degree diagonal lines as well. Count points where at least 2 lines overlap.

## Algorithmic Approach

### Core Algorithm

For each line segment (x1,y1) -> (x2,y2):
1. Calculate direction: `dx = sign(x2-x1)`, `dy = sign(y2-y1)`
2. Walk from start to end, incrementing a grid counter at each point
3. Count all points with value >= 2

**Time Complexity**: O(n × L) where n = number of lines, L = average line length
**Space Complexity**: O(W × H) for the grid, or O(points) for hash map approach

### Part 1 vs Part 2

- **Part 1**: Skip lines where both `dx != 0` and `dy != 0` (diagonals)
- **Part 2**: Process all lines including 45-degree diagonals

### Key Insight

Since diagonal lines are always exactly 45 degrees, the step size along x and y axes is always ±1 or 0. This means we can use the same walking algorithm for all line types - just compute the sign of the direction.

## Programming Techniques Highlighted

- **Line Rasterization**: Walking along a line segment point by point
- **Hash Map Counting**: Efficiently tracking point coverage
- **Sign Function**: Computing direction from coordinate differences
- **Grid vs Hash Trade-off**: Fixed array for known bounds vs hash map for sparse grids

## Data Structures Used

- Array of line segment tuples (x1, y1, x2, y2)
- Hash map or 2D array for point coverage counts
- Direction vectors (dx, dy)

## Language-Specific Notes

- **C**: Fastest at 7.3ms using static 1000x1000 grid array
- **ARM64**: 10.6ms with efficient grid access
- **Zig/Rust/C++/Go**: 15-22ms range using hash maps
- **Common Lisp**: Strong 50.6ms with hash tables
- **Python/PHP**: 86-89ms, clean hash-based implementation
- **Node.js/Perl/Java**: 102-112ms range
- **Ruby**: 346ms, hash operations have more overhead
- **Clojure**: 537ms with functional approach
- **ColdFusion**: 2.9s with CommandBox overhead
- **Bash**: 3.4s - associative arrays slow for this workload

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 7.3          | 5.1         |
| ARM64 asm   | 10.6         | 2.2         |
| Zig         | 15.4         | 5.6         |
| Rust        | 17.8         | 11.1        |
| C++         | 21.9         | 10.7        |
| Go          | 22.2         | 13.8        |
| Common Lisp | 50.6         | 65.9        |
| Python      | 85.9         | 46.2        |
| PHP         | 88.9         | 38.8        |
| Node.js     | 102.3        | 72.3        |
| Perl        | 111.0        | 30.5        |
| Java        | 111.5        | 76.5        |
| Ruby        | 346.2        | 58.5        |
| Clojure     | 537.2        | 229.6       |
| ColdFusion  | 2,958.7      | 1,010.4     |
| Bash        | 3,392.6      | 41.4        |

## Answers

- **Part 1**: 4993
- **Part 2**: 21101
