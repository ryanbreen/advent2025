# Day 12: Christmas Tree Farm

## Problem Summary

You emerge into a cavern full of Christmas trees where Elves are worried about fitting **presents** (polyominoes) into rectangular **regions** under each tree. The challenge is to determine how many regions can accommodate all their assigned presents.

### Input Format

The input consists of two sections separated by blank lines:

1. **Shape Definitions**: Each shape has an index followed by a visual representation using `#` (part of shape) and `.` (empty space):
   ```
   0:
   ###
   ##.
   ##.
   ```

2. **Region Definitions**: Each line specifies a region's dimensions and required shape counts:
   ```
   12x5: 1 0 1 0 2 2
   ```
   This means: 12 units wide, 5 units tall, needing 1 of shape 0, 0 of shape 1, 1 of shape 2, etc.

## Part 1: Polyomino Packing Check

The problem *appears* to ask for true polyomino packing (an NP-hard problem), but analysis of the actual puzzle input reveals a key insight:

### Key Insight

**The solution is simply an area check**: count whether the total cells needed by all shapes fits within the available region area.

```
total_cells_needed = Σ (count[i] × shape_size[i])
available_cells = width × height
fits = total_cells_needed ≤ available_cells
```

This works because the puzzle input is crafted such that:
- If shapes fit by area, they can always be arranged to fit spatially
- If shapes don't fit by area, they obviously can't fit spatially

### Algorithm

1. **Parse Shapes**: For each shape definition, count the number of `#` characters to get its cell count
2. **Parse Regions**: Extract width, height, and shape counts for each region
3. **Check Each Region**: Sum (count × size) for all shapes, compare to width × height
4. **Count Fits**: Return the number of regions where total ≤ available

### Complexity

- **Time**: O(S + R × N) where S = shape definitions, R = regions, N = shapes per region
- **Space**: O(S + R) for storing shape sizes and region data

## Part 2: The Star

Part 2 is unusual - it's not a computational problem at all. The puzzle page simply presents a "Finish Decorating" button that reveals the star. All the Elves you helped throughout the event have arrived with stars to place on the trees, and clicking the button completes the day.

**Part 2 returns 0** (or any placeholder value) since no computation is required.

## Programming Techniques Highlighted

### Data Structures
- **Hash Maps**: Storing shape index → cell count mappings
- **Arrays/Lists**: Storing region definitions

### CS Concepts
- **Parsing**: Multi-section input with different formats
- **Constraint Satisfaction**: Checking area constraints (simplified from full packing)
- **String Processing**: Counting characters in shape representations

## Language-Specific Notes

### Fastest Performers (5-8ms)
- **ARM64 Assembly**: Direct memory access, no overhead - 5.61ms
- **Rust**: Zero-cost abstractions, excellent optimization - 5.58ms
- **Zig**: Memory-safe low-level performance - 5.77ms
- **C**: Minimal runtime overhead - 6.06ms

### Parsing Challenges
- **ColdFusion**: `#` character requires escaping as `chr(35)` since it's a special character in CFML
- **Bash**: Required careful handling of word splitting and arithmetic
- **Zig**: ArrayList API requires explicit allocator passing in recent versions

### JVM Startup Impact
- **Clojure**: ~413ms due to JVM cold start
- **ColdFusion**: ~2514ms (JVM + Lucee engine startup)
- **Java**: ~57ms with minimal startup overhead

## Benchmark Results

| Rank | Language     | Time (ms) | Memory (MB) | Notes |
|------|-------------|-----------|-------------|-------|
| 1    | Rust        | 5.58      | 1.89        | Fastest compiled |
| 2    | ARM64       | 5.61      | 1.89        | Hand-optimized assembly |
| 3    | Zig         | 5.77      | 1.89        | Modern systems lang |
| 4    | C           | 6.06      | 1.67        | Lowest memory |
| 5    | C++         | 7.77      | 1.97        | STL containers |
| 6    | Perl        | 11.76     | 5.69        | Surprisingly fast |
| 7    | Common Lisp | 26.44     | 42.75       | SBCL compiled |
| 8    | Python      | 27.43     | 15.09       | Clean, readable |
| 9    | Node.js     | 47.56     | 44.17       | V8 JIT |
| 10   | Go          | 50.89     | 27.14       | Includes compile |
| 11   | PHP         | 51.13     | 25.30       | Consistent speed |
| 12   | Java        | 56.61     | 53.28       | JVM overhead |
| 13   | Ruby        | 62.60     | 28.36       | Expressive but slower |
| 14   | Bash        | 117.09    | 6.62        | Shell scripting limits |
| 15   | Clojure     | 412.89    | 133.34      | JVM cold start |
| 16   | ColdFusion  | 2514.18   | 1195.98     | Heavy runtime |

## Answers

- **Part 1**: 591
- **Part 2**: 0 (button click completion)
