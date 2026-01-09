# Day 4: Camp Cleanup

## Problem Summary

Elves are assigned section ranges for cleanup duty. Each line contains two ranges (one per Elf in a pair) in the format `a-b,c-d`.

**Part 1**: Count pairs where one range fully contains the other.

**Part 2**: Count pairs where ranges overlap at all.

## Input Format

Each line contains two comma-separated ranges:
```
2-4,6-8
2-3,4-5
5-7,7-9
```

## Algorithmic Approach

### Part 1: Full Containment

Range `[a1, b1]` fully contains `[a2, b2]` if:
- `a1 <= a2` AND `b1 >= b2` (first contains second), OR
- `a2 <= a1` AND `b2 >= b1` (second contains first)

### Part 2: Any Overlap

Two ranges `[a1, b1]` and `[a2, b2]` overlap if:
- `a1 <= b2` AND `a2 <= b1`

This is equivalent to: NOT (b1 < a2 OR b2 < a1) - they overlap unless one ends before the other starts.

### Time Complexity

- O(n) where n = number of pairs
- Each pair evaluated in O(1) with simple comparisons

### Space Complexity

- O(1) - only need to track counts

## Key Insight

Both problems reduce to simple integer comparisons. No need to enumerate actual section IDs or build interval data structures - just compare endpoints.

## Programming Techniques Highlighted

- **Interval arithmetic**: Comparing range endpoints
- **Parsing**: Extracting numbers from formatted strings
- **Predicate functions**: Boolean conditions for counting

## Language-Specific Notes

### Systems Languages (< 7ms)
- **C, Zig, Rust, ARM64, C++**: All ~6ms
- Parsing with `fscanf`/`sscanf` or string splitting
- Simple integer comparisons dominate

### Shell/Scripting (17-27ms)
- **Bash**: AWK with `-F'[-,]'` splits on both delimiters at once
- **Perl**: Regex capture groups for parsing
- **Common Lisp**: Position-based string parsing
- **Python**: `split()` chains for clean parsing

### Interpreted/VM (47-76ms)
- **Node.js, Java, Ruby, PHP, Go**: Standard parsing patterns
- Go includes compilation overhead with `go run`

### Heavy Runtimes (400-2700ms)
- **Clojure**: JVM + Clojure runtime startup
- **ColdFusion**: Enterprise runtime overhead

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.8          | 1.9         |
| Zig         | 6.3          | 1.9         |
| Rust        | 6.6          | 1.9         |
| ARM64 asm   | 6.7          | 1.9         |
| C++         | 6.8          | 1.9         |
| Bash        | 17.4         | 6.7         |
| Perl        | 17.5         | 5.8         |
| Common Lisp | 22.4         | 38.9        |
| Python      | 27.4         | 14.9        |
| Node.js     | 47.3         | 40.5        |
| Java        | 56.5         | 48.3        |
| Ruby        | 57.9         | 27.9        |
| PHP         | 60.7         | 25.7        |
| Go          | 75.2         | 27.1        |
| Clojure     | 425.6        | 129.2       |
| ColdFusion  | 2,704.9      | 1,144.1     |

## Answers

- Part 1: 483
- Part 2: 874
