# Day 5: If You Give A Seed A Fertilizer

## Problem Summary

The gardener needs help interpreting an almanac that maps seeds through a chain of transformations (seed → soil → fertilizer → water → light → temperature → humidity → location) to determine where to plant them.

**Input Format:**
```
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
...
```

Each map entry contains three numbers: `destination_start source_start length`, defining how to translate a range of source values to destination values.

## Part 1: Individual Seeds

Given a list of seed numbers, transform each through all 7 maps and find the **minimum location number**.

**Answer: 825516882**

## Part 2: Seed Ranges

The seeds line now describes **ranges**: pairs of `start length` values. With billions of seeds, we can't process them individually.

**Answer: 136096660**

## Algorithmic Approach

### Part 1: Direct Mapping

1. Parse seeds as individual numbers
2. For each seed, apply 7 transformations sequentially
3. For each transformation, check if the value falls within any source range
4. If in range: `destination = dest_start + (value - src_start)`
5. If not in range: value passes through unchanged
6. Track minimum final location

**Time Complexity:** O(s × m × r) where s = seeds, m = maps (7), r = ranges per map
**Space Complexity:** O(r) for storing map ranges

### Part 2: Range Splitting

The key insight is to work with **ranges** instead of individual values. When a range passes through a map:

1. Split the input range into pieces that align with map ranges
2. For overlapping portions: apply the offset transformation
3. For non-overlapping portions: pass through unchanged
4. Continue with all resulting ranges to the next map

**Range splitting algorithm:**
```
For each input range [start, end):
  For each map entry (dst, src, len):
    - Before overlap: [start, min(end, src)) → keep unmapped
    - Overlap: [max(start, src), min(end, src+len)) → transform by (dst-src)
    - After overlap: [max(start, src+len), end) → keep unmapped
```

**Time Complexity:** O(m × r²) where r = ranges (can grow during splitting)
**Space Complexity:** O(r) for current ranges

### Key Insight

The problem looks like it requires iterating billions of seeds, but the range-based approach reduces it to operations on range boundaries only. The number of ranges stays manageable because:
- Input has ~10 seed ranges
- Each map has ~20-50 entries
- Splitting at most doubles the range count per map

## Programming Techniques Highlighted

### Data Structures
- **Range representation:** Store as (start, length) or (start, end) pairs
- **Map entries:** Triple of (destination, source, length)
- **Sorted ranges:** Helps with efficient splitting

### Parsing Challenges
- Multi-section input with different formats
- Variable whitespace handling
- Large numbers (billions) require 64-bit integers

### Patterns Used
- **Range arithmetic:** Intersection, splitting, offsetting
- **Pipeline processing:** Chain of transformations
- **Interval scheduling:** Dividing ranges at boundaries

## Language-Specific Notes

### Fast Performers (< 10 ms)
- **C, C++, ARM64:** Direct memory access and simple data structures
- **Rust:** Zero-cost abstractions with safe range handling

### Mid-Tier (15-60 ms)
- **Perl:** Surprisingly fast for this problem
- **Python, Lisp:** Good balance of readability and performance
- **Zig, Go, Java:** Moderate overhead

### Slower Performers (> 400 ms)
- **Clojure:** JVM startup plus functional overhead
- **ColdFusion:** JVM startup dominates

### Bash
After tuning to use associative arrays instead of eval-based dynamic variables, the Bash implementation now runs at a reasonable speed (~350ms), though still slower than compiled languages.

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 5.5          | 1.9         |
| Rust        | 6.5          | 1.9         |
| C++         | 6.6          | 1.9         |
| C           | 6.8          | 1.9         |
| Zig         | 7.0          | 1.9         |
| Perl        | 20.5         | 6.3         |
| ColdFusion  | 26.8         | 4.9         |
| Lisp        | 30.5         | 42.2        |
| Python      | 38.9         | 18.0        |
| Node.js     | 57.8         | 46.3        |
| PHP         | 65.4         | 26.1        |
| Go          | 70.4         | 27.3        |
| Ruby        | 77.0         | 28.2        |
| Java        | 79.1         | 55.9        |
| Bash        | 347.8        | 7.2         |
| Clojure     | 453.3        | 136.4       |

*Note: ColdFusion benchmark measured via HTTP request to running server; actual startup time is higher.*

## Answers

- **Part 1:** 825516882
- **Part 2:** 136096660
