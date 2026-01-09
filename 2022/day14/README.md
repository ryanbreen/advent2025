# Day 14: Regolith Reservoir

## Problem Summary

Sand is falling into a cave from point (500, 0). The cave contains rock structures that block the sand. You need to simulate the falling sand to determine how many units come to rest.

**Part 1**: Count how many units of sand come to rest before sand starts falling into the endless void below the rock structures.

**Part 2**: There's an infinite floor at `max_y + 2`. Count how many units of sand come to rest before the source at (500, 0) becomes blocked.

## Input Format

Lines defining rock paths with coordinates connected by ` -> `:
```
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
```

Each path segment is either horizontal or vertical.

## Algorithmic Approach

### Sand Physics

Each unit of sand follows these rules:
1. Try to move **down** (y + 1)
2. If blocked, try to move **down-left** (x - 1, y + 1)
3. If blocked, try to move **down-right** (x + 1, y + 1)
4. If all three blocked, sand **comes to rest**

### Key Insight

This is a simulation problem where we need to efficiently track blocked positions (rocks and settled sand). A hash set provides O(1) lookups for checking if a position is blocked.

### Part 1 Algorithm

```
blocked = parse_rock_positions()
max_y = max y-coordinate of all rocks

while True:
    pos = simulate_one_sand_unit(blocked, max_y)
    if pos.y > max_y:  # fell into abyss
        break
    blocked.add(pos)
    count++

return count
```

### Part 2 Algorithm

```
blocked = parse_rock_positions()
max_y = max y-coordinate of all rocks
floor_y = max_y + 2

while True:
    pos = simulate_sand_with_floor(blocked, floor_y)
    blocked.add(pos)
    count++
    if pos == (500, 0):  # source blocked
        break

return count
```

### Data Structures

- **Hash Set**: Store blocked positions as (x, y) tuples or encoded keys
- **Position encoding**: Many implementations use string keys like `"x,y"` or bit-packed integers

### Complexity

- **Part 1**: O(s * h) where s = sand count, h = average fall height
- **Part 2**: O(s * h) but s is much larger (fills to pyramid shape)
- **Space**: O(r + s) for rocks and sand positions

## Programming Techniques Highlighted

- **Simulation**: Step-by-step physics simulation
- **Hash set operations**: Fast position lookup and insertion
- **Coordinate parsing**: Extracting (x, y) pairs from path strings
- **Line drawing**: Filling in coordinates between path endpoints

## Language-Specific Notes

### Performance Characteristics

This problem heavily exercises hash set operations, so languages with efficient hash implementations perform well:

- **C**: Fastest - used 2D array grid directly, O(1) array indexing
- **ARM64**: Close second - direct memory access to grid
- **C++, Zig, Rust, Go**: Efficient compiled code with good hash sets
- **Java**: Moderate - JIT compilation helps but startup overhead
- **Common Lisp**: Surprisingly fast hash tables
- **Node.js, Python, PHP, Perl**: Scripted but usable
- **Ruby**: Unexpectedly slow for this simulation-heavy problem
- **Clojure**: Immutable data structure overhead
- **ColdFusion**: Runtime overhead dominates
- **Bash**: Very slow - associative array lookups expensive

### Grid vs Set

Two main approaches:
1. **2D Array Grid**: Faster access but wastes memory if coords are sparse
2. **Hash Set**: More memory-efficient, slightly slower lookups

C and ARM64 used grid approach which proved fastest for this problem's coordinate range.

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 7.8          | 1.9         |
| ARM64 asm   | 11.4         | 1.9         |
| C++         | 20.5         | 2.8         |
| Zig         | 33.5         | 1.9         |
| Rust        | 43.5         | 2.3         |
| Go          | 58.1         | 6.3         |
| Java        | 123.1        | 102.1       |
| Common Lisp | 170.9        | 89.4        |
| Node.js     | 299.8        | 51.9        |
| Python      | 435.9        | 19.8        |
| PHP         | 440.9        | 28.0        |
| Perl        | 712.0        | 8.9         |
| Clojure     | 946.1        | 607.0       |
| Ruby        | 2,849.7      | 30.8        |
| ColdFusion  | 3,726.0      | 1,054.1     |
| Bash        | TBD          | TBD         |

## Answers

- Part 1: 1072
- Part 2: 24659
