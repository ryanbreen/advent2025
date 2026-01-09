# Day 18: Boiling Boulders

## Problem Summary

Calculate surface area of a 3D lava droplet approximated by 1x1x1 cubes on a 3D grid.

**Part 1**: Calculate the total surface area by counting all cube faces not touching another cube.

**Part 2**: Calculate the exterior surface area only (excluding trapped air pockets inside the droplet).

## Input Format

Each line contains x,y,z coordinates of a cube:
```
2,2,2
1,2,2
3,2,2
...
```

## Algorithmic Approach

### Part 1: Simple Face Counting

For each cube, check all 6 faces (neighbors in ±x, ±y, ±z directions). If a neighbor position is not occupied by another cube, count that face as exposed.

**Complexity**: O(n) where n is the number of cubes

### Part 2: Exterior Surface Only (BFS Flood Fill)

The key insight is that internal air pockets are enclosed and unreachable from outside the droplet.

**Algorithm**:
1. Find bounding box of all cubes with 1-unit padding
2. BFS flood fill from a corner to discover all "exterior" air cells
3. Count cube faces that touch these exterior cells (ignoring faces touching trapped air pockets)

**Complexity**: O(n + volume of bounding box)

### Data Structures

- **Set/HashSet**: Store cube coordinates for O(1) neighbor lookup
- **Queue**: BFS traversal for flood fill
- **Set for exterior**: Track visited exterior air cells

## Programming Techniques Highlighted

- **3D coordinate handling**: Efficient coordinate storage and neighbor iteration
- **BFS flood fill**: Classic technique for finding connected regions
- **Set operations**: Fast membership testing for collision detection

## Language-Specific Notes

### Performance Characteristics

Day 18 is relatively fast across all languages due to the simple algorithm and moderate input size (~2000 cubes).

**Very fast (< 10ms)**:
- **ARM64**: 4.8ms - hand-tuned assembly, excellent
- **C**: 5.4ms - minimal overhead
- **Rust**: 6.7ms - efficient HashSet
- **Zig**: 7.4ms - good hash map performance
- **Go**: 8.0ms - efficient implementation (much better than Day 17!)
- **C++**: 9.9ms - STL unordered_set

**Fast (30-70ms)**:
- **Common Lisp**: 33.9ms - hash tables work well
- **Perl**: 36.2ms - hashes are efficient
- **Python**: 37.6ms - set operations optimized
- **PHP**: 62.0ms - associative arrays
- **Java**: 63.6ms - HashMap overhead
- **Node.js**: 69.8ms - V8 Map/Set

**Moderate (100-500ms)**:
- **Ruby**: 141.9ms - slower hash operations
- **Clojure**: 465.5ms - JVM startup + immutable data

**Slow**:
- **Bash**: 848ms - associative arrays for 3D grid
- **ColdFusion**: 2536ms - JVM overhead

### Implementation Notes

- Go performs much better here than Day 17 (8ms vs 1.4s) - Day 17's Go implementation had issues
- Bash is surprisingly fast for Day 18 (848ms vs 49s for Day 17) due to simpler data structure requirements
- Coordinate packing (e.g., `x * 10000 + y * 100 + z`) can be faster than string keys in some languages

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 4.8          | 1.5         |
| C           | 5.4          | 2.7         |
| Rust        | 6.7          | 1.9         |
| Zig         | 7.4          | 1.9         |
| Go          | 8.0          | 6.3         |
| C++         | 9.9          | 1.9         |
| Common Lisp | 33.9         | 48.5        |
| Perl        | 36.2         | 6.3         |
| Python      | 37.6         | 16.4        |
| PHP         | 62.0         | 26.7        |
| Java        | 63.6         | 58.1        |
| Node.js     | 69.8         | 49.2        |
| Ruby        | 141.9        | 29.0        |
| Clojure     | 465.5        | 168.8       |
| Bash        | 848.4        | 8.0         |
| ColdFusion  | 2,536.5      | 951.1       |

## Answers

- Part 1: 3662
- Part 2: 2060
