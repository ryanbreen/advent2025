# Day 9: Movie Theater

## Problem Summary

The Elves are redecorating a movie theater by switching out tiles. Some tiles are **red**, and they want to find the largest rectangle that uses red tiles for two of its opposite corners.

**Input:** A list of 496 red tile coordinates (x, y) with values up to ~100,000

### Part 1: Largest Rectangle with Red Corners
Find the largest rectangle using any two red tiles as opposite corners. The area includes all tiles within the rectangle (inclusive of corners).

### Part 2: Rectangle Must Be Inside Polygon
The red tiles form a closed polygon in the order they appear in the input. Consecutive red tiles are connected by horizontal or vertical lines of **green** tiles, and the polygon's interior is also green. Find the largest rectangle with red corners where the **entire rectangle** is contained within the red/green region.

## Algorithmic Approach

### Part 1: Brute Force Pairs
With ~500 points, checking all pairs is O(n²) ≈ 123,000 comparisons - very manageable.

**Algorithm:**
1. Parse all red tile coordinates
2. For each pair of points (i, j):
   - Calculate rectangle area = (|x2-x1| + 1) * (|y2-y1| + 1)
   - Track maximum area found
3. Return maximum

**Complexity:** O(n²) time, O(n) space

### Part 2: Rectilinear Polygon Containment
The challenge is efficiently checking if a rectangle is entirely inside the polygon without iterating over every tile (which could be ~10 billion tiles given coordinate ranges up to 100,000).

**Key Insight:** For a rectilinear (axis-aligned) polygon, a rectangle is entirely inside if and only if:
1. No polygon edge crosses through the rectangle's interior
2. The rectangle's center is inside the polygon

**Algorithm:**
1. Build lists of horizontal and vertical edges from consecutive points
2. For each pair of red points:
   - Check if any vertical edge has x-coordinate strictly between rectangle's x bounds AND y-range overlapping
   - Check if any horizontal edge has y-coordinate strictly between rectangle's y bounds AND x-range overlapping
   - If no edges cross through, verify center point is inside using ray casting
3. Return maximum area among valid rectangles

**Ray Casting:** Cast a horizontal ray to the right from the center point. Count crossings with vertical edges. Odd count = inside.

**Complexity:** O(n² * E) where E is number of edges (~500). Total ≈ O(n³) but with small constants.

## Programming Techniques Highlighted

- **Computational Geometry**: Point-in-polygon testing, edge intersection
- **Ray Casting Algorithm**: Classic method for determining if a point is inside a polygon
- **Rectilinear Polygon Properties**: Axis-aligned edges simplify intersection tests
- **Optimization**: Avoiding O(area) checks by using edge-based containment tests

## Data Structures Used

- **Arrays/Lists**: Store points and edges
- **Hash Maps**: Index edges by x-coordinate (vertical) or y-coordinate (horizontal) for efficient lookup
- **Coordinate Compression**: Not needed since we work with edges, not individual tiles

## Computer Science Topics

This problem is an excellent introduction to:
1. **Computational geometry** - working with polygons and containment
2. **Ray casting** - fundamental algorithm for point-in-polygon tests
3. **Rectilinear polygons** - special case with simpler intersection math
4. **Optimization thinking** - avoiding brute force over large coordinate spaces

## Language Notes

### Fast Performers (~14ms)
- **Zig, C, ARM64, Rust**: All within 1ms of each other at ~14ms. The O(n³) algorithm with small constants (n=496) is dominated by simple operations - nested loops with basic comparisons and arithmetic. Hash map lookups for edge checking are fast but the constant overhead is noticeable at this scale.

### Mid-Tier (~40-110ms)
- **C++**: Surprisingly 3x slower than C at 43ms. The STL map/vector abstractions add overhead for this problem's access patterns.
- **Go**: 47ms - efficient but GC and map operations add some overhead.
- **Java**: 110ms - JIT warmup doesn't help since runtime is short; HashMap operations are heavier than native implementations.

### Interpreted Languages (~200-800ms)
- **Node.js, Common Lisp**: Both around 227ms - V8's JIT and SBCL's native compilation put them in similar territory.
- **PHP**: 300ms - reasonable for a scripting language with native hash tables.
- **Python**: 558ms - dictionary operations are well-optimized but still interpreted overhead.
- **Ruby**: 775ms - hash operations and iteration are slower in Ruby's object model.

### Slow Performers (>1s)
- **Clojure**: 1177ms - JVM startup dominates since actual computation is fast.
- **Perl**: 1313ms - nested loops and hash access are particularly slow; perl's strength is regex, not numerics.
- **ColdFusion**: 3943ms - JVM startup plus Lucee engine overhead.
- **Bash/AWK**: 4781ms - AWK handles arrays well but nested loop iteration is costly.

### Key Insight
This problem rewards languages with fast hash map implementations and efficient loop iteration. The O(n³) complexity means even small per-operation overhead compounds quickly.

## Benchmark Results

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 13.91        | 1.92        |
| C           | 13.99        | 1.33        |
| ARM64       | 14.36        | 1.50        |
| Rust        | 14.87        | 1.64        |
| C++         | 43.66        | 1.41        |
| Go          | 47.29        | 4.23        |
| Java        | 110.02       | 72.48       |
| Common Lisp | 227.00       | 53.73       |
| Node.js     | 231.00       | 52.52       |
| PHP         | 300.25       | 24.91       |
| Python      | 557.54       | 15.92       |
| Ruby        | 774.62       | 28.78       |
| Clojure     | 1,176.69     | 1,393.48    |
| Perl        | 1,312.77     | 5.25        |
| ColdFusion  | 3,943.18     | 1,034.75    |
| Bash        | 4,781.25     | 1.98        |

## Answers

- **Part 1**: 4750297200
- **Part 2**: 1578115935
