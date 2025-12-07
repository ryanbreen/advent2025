# Day 4: Ceres Search

## Problem Summary

You're helping an Elf solve a word search puzzle at the Ceres monitoring station. The puzzle input is a 2D grid of letters.

**Input Format:** A rectangular grid of uppercase letters (typically 140x140 characters).

**Part 1:** Count all occurrences of the word "XMAS" in the grid. Words can appear horizontally, vertically, diagonally, forwards, or backwards - in all 8 directions.

**Part 2:** Count all "X-MAS" patterns - where two "MAS" strings form an X shape, crossing at the letter 'A'. Each "MAS" can be written forwards or backwards along its diagonal.

## Part 1 Analysis

### What We're Looking For
Find every instance of "XMAS" in the grid, where the word can be oriented in any of 8 directions.

### Algorithm Overview
1. Iterate through every cell in the grid
2. For each cell, check all 8 directions (right, left, down, up, and 4 diagonals)
3. For each direction, verify if the 4 characters spell "XMAS"
4. Count all valid matches

### Key Data Structures
- **2D Character Array/Grid:** Store the puzzle input for O(1) character access
- **Direction Vectors:** Array of 8 (row_delta, col_delta) pairs representing all directions

### Implementation Approach
```
directions = [(0,1), (0,-1), (1,0), (-1,0), (1,1), (1,-1), (-1,1), (-1,-1)]
for each cell (r, c):
    for each direction (dr, dc):
        if "XMAS" found starting at (r,c) in direction (dr,dc):
            count++
```

## Part 2 Analysis

### How Part 2 Changes the Problem
Instead of finding linear "XMAS" strings, we're now looking for X-shaped patterns where:
- The center character is 'A'
- Both diagonals spell "MAS" (either forwards or backwards)

### Key Insight
The 'A' is always at the center of the X. This means:
1. We only need to check interior cells (not edge cells, since the X needs neighbors on all diagonal corners)
2. For each 'A' found, check the four corner characters
3. Both diagonals must have M at one end and S at the other

### Algorithm Modifications
1. Skip edge rows and columns (the center 'A' needs room for corners)
2. Only consider cells containing 'A'
3. Check both diagonals: top-left/bottom-right AND top-right/bottom-left
4. Each diagonal is valid if it contains {M, S} in any order

### Valid X-MAS Configurations
There are exactly 4 valid configurations:
```
M.S    M.M    S.M    S.S
.A.    .A.    .A.    .A.
M.S    S.S    S.M    M.M
```

## Algorithmic Approach

### Key Insight
The crucial realization is that word search problems are fundamentally about systematic enumeration with bounds checking. For Part 1, treating each cell as a potential starting point and checking all 8 directions is both intuitive and efficient. For Part 2, recognizing that 'A' must be the center reduces the search space significantly.

### Data Structures
- **2D Array:** The grid itself stored as an array of strings or 2D character array
- **Direction Tuples:** Constant array defining the 8 movement directions

### Time Complexity
- **Part 1:** O(R * C * 8 * 4) = O(R * C) where R = rows, C = columns
  - For each of R*C cells, check 8 directions, each requiring 4 character comparisons
- **Part 2:** O(R * C)
  - For each interior cell, perform constant-time checks on 4 corners

### Space Complexity
- **O(R * C)** for storing the grid
- **O(1)** auxiliary space for counters and direction vectors

## Programming Techniques Highlighted

### Computer Science Concepts
1. **Grid Traversal:** Systematic iteration over 2D data structures
2. **Direction Vectors:** Encoding movement as coordinate deltas
3. **Bounds Checking:** Ensuring array accesses stay within valid ranges
4. **Pattern Matching:** Character-by-character string verification

### Mathematical Properties
- **Symmetry:** The X-MAS pattern has 4 rotational variants, all equivalent
- **Enumeration:** The brute-force approach is optimal here since every cell must potentially be examined

## Language-Specific Implementation Notes

### Naturally Suited Languages

**Systems Languages (C, C++, Rust, Zig):** Excel at this problem due to:
- Direct 2D array indexing with minimal overhead
- Character comparisons compile to simple byte operations
- Cache-friendly memory access patterns when iterating row-by-row

**Python:** Natural list indexing and readable direction iteration make the code clean and maintainable, though slower than compiled languages.

### Notable Implementation Details

**C/C++:** Uses fixed-size arrays (`char grid[200][200]`) avoiding dynamic allocation. Bounds checking must be explicit.

**Rust:** Requires explicit type casting between `usize` (for indexing) and `i32` (for signed arithmetic with negative direction deltas).

**Zig:** Achieved the fastest runtime, benefiting from compile-time optimization and zero-overhead abstractions.

**AWK:** Pattern matching capabilities are less useful here; the problem requires direct indexing which AWK handles less elegantly.

**Bash:** Performance suffers dramatically (23+ seconds) due to shell overhead for character-by-character operations and subprocess spawning.

### Performance Characteristics by Language Family

| Family | Languages | Characteristics |
|--------|-----------|-----------------|
| Systems | C, C++, Rust, Zig | 6-7ms, minimal memory |
| JVM | Java, Kotlin, Scala | 68-1766ms, higher memory due to JVM startup |
| Scripting | Python, Ruby, PHP, Perl | 60-158ms, moderate memory |
| Functional | Clojure, Common Lisp | 99-495ms |
| Shell | Bash, AWK | 228ms-23s, pattern matching doesn't help here |

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 6.1          | 1.9         |
| C++         | 6.5          | 1.9         |
| ARM64 asm   | 6.8          | 1.9         |
| C           | 6.9          | 1.9         |
| Rust        | 7.4          | 1.9         |
| Node.js     | 57.7         | 48.5        |
| Python      | 60.7         | 15.6        |
| Java        | 68.8         | 47.4        |
| Go          | 71.9         | 26.7        |
| PHP         | 73.5         | 24.5        |
| Perl        | 75.1         | 5.7         |
| Kotlin      | 84.3         | 55.8        |
| Lisp        | 99.6         | 40.1        |
| Ruby        | 157.9        | 28.3        |
| AWK         | 227.6        | 3.7         |
| Clojure     | 494.7        | 213.9       |
| Scala       | 1,765.9      | 266.1       |
| ColdFusion  | 2,772.2      | 1,119.8     |
| Bash        | 23,480.7     | 2.1         |

### Performance Analysis

The systems languages (Zig, C++, ARM64 assembly, C, Rust) form a tight cluster at 6-7ms, demonstrating that this problem is fundamentally about efficient memory access patterns. The 2D grid traversal benefits from:
- Sequential memory access (good cache utilization)
- Simple character comparisons (single CPU instructions)
- No complex data structure overhead

Notably:
- **Zig** edges out the competition despite being a newer language
- **Go** is slower than expected (71.9ms) compared to its usual performance, possibly due to bounds checking overhead
- **Bash** is 3,800x slower than Zig, making this an extreme example of algorithm-vs-language tradeoffs
- **Scala's** slow performance (1,766ms) is likely due to JVM cold-start overhead combined with functional programming abstractions

## Answers

| Part | Answer |
|------|--------|
| Part 1 | 2532 |
| Part 2 | 1941 |
