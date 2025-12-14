# Day 12: Garden Groups

## Problem Summary

You're helping Elves calculate fencing costs for garden plots. The garden is represented as a grid where each letter represents a type of plant. Connected plots of the same plant type form a **region**.

**Part 1**: Calculate total fencing cost as the sum of `area * perimeter` for each region. The perimeter is the number of edges between a region and cells outside it.

**Part 2**: Under a bulk discount, use the **number of sides** instead of perimeter. Each straight section of fence counts as one side, regardless of length.

### Input Format
A 2D grid of characters where each character represents a plant type:
```
AAAA
BBCD
BBCC
EEEC
```

## Algorithmic Approach

### Part 1: Region Finding + Perimeter

1. **Flood Fill (BFS)**: Starting from each unvisited cell, perform BFS to find all connected cells with the same plant type
2. **Perimeter Calculation**: For each cell in a region, count edges adjacent to cells outside the region (including grid boundaries)
3. Sum `area * perimeter` for all regions

### Part 2: Counting Sides (Corners)

The key insight is that **number of sides = number of corners** for any polygon.

For each cell in a region, check all 4 corners:
- **Convex corner**: Both orthogonal neighbors are outside the region
- **Concave corner**: Both orthogonal neighbors are inside the region, but the diagonal neighbor is outside

Example for top-left corner of a cell at (r, c):
```
Convex:      Concave:
  X X          A ?
  X A          ? A
(UP and LEFT   (UP and LEFT in region,
 not in region) UP-LEFT not in region)
```

### Complexity

- **Time**: O(R * C) where R and C are grid dimensions
  - Each cell is visited once during flood fill
  - Perimeter/corner calculations are O(1) per cell
- **Space**: O(R * C) for visited set and region storage

### Key Insights

1. **Corner counting for sides**: A polygon's sides equal its corners - this transforms a potentially complex boundary-tracing problem into simple neighbor checks
2. **Concave vs convex corners**: Must handle both cases - regions with holes have inner concave corners
3. **Diagonal neighbors matter**: For Part 2, diagonal relationships determine concave corners

## Data Structures Used

- **2D Grid**: Character array for plant types
- **Set**: Track visited cells and cells in current region
- **Queue/Deque**: BFS frontier for flood fill

## Programming Techniques Highlighted

- **Flood Fill / BFS**: Classic connected component algorithm
- **Grid Traversal**: 4-directional neighbor checking
- **Set Operations**: Fast membership testing for region boundaries
- **Corner Detection**: Geometric reasoning about polygon properties

## Language Notes

### Fast Performers
- **Rust, Zig, C**: Low overhead, efficient set implementations (hash sets or bitmaps)
- **Go**: Good performance with native maps

### Middle Tier
- **Python**: Surprisingly competitive with built-in set operations
- **Java, Common Lisp**: JIT compilation helps; Lisp's hash tables are efficient

### Slower Languages
- **Ruby, Perl**: Higher interpreter overhead
- **Clojure**: JVM startup + functional overhead
- **ColdFusion, Bash**: Not designed for algorithmic work; Bash particularly slow due to process spawning for operations

### Implementation Variations
- Some implementations use DFS instead of BFS (equivalent results)
- Compiled languages often use fixed-size arrays instead of sets for visited tracking
- ARM64 assembly uses bit manipulation for efficient corner detection

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Rust        | 16.3         | 3.1         |
| Zig         | 16.2         | 4.5         |
| C           | 17.5         | 1.9         |
| Go          | 19.4         | 9.9         |
| ARM64 asm   | 25.9         | 1.9         |
| C++         | 36.5         | 3.5         |
| Python      | 76.0         | 20.8        |
| Java        | 85.1         | 74.4        |
| Common Lisp | 108.2        | 59.8        |
| Node.js     | 114.0        | 67.2        |
| PHP         | 121.7        | 30.3        |
| Perl        | 150.0        | 10.1        |
| Ruby        | 386.4        | 35.3        |
| Clojure     | 626.1        | 216.5       |
| ColdFusion  | 3,313.3      | 1,089.9     |
| Bash        | 7,221.7      | 6.8         |

## Answers

- Part 1: 1377008
- Part 2: 815788
