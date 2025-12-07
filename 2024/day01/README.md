# Day 1: Historian Hysteria

## Problem Summary

The Chief Historian has gone missing, and the Elvish Senior Historians need help reconciling two lists of location IDs they've compiled while searching his office. The puzzle input consists of two columns of numbers representing location IDs from two independently compiled lists.

### Input Format
- Two columns of integers separated by whitespace
- Each line contains one number from each list
- Approximately 1000 lines in the full input

Example:
```
3   4
4   3
2   5
1   3
3   9
3   3
```

### What We're Computing
- **Part 1**: Total distance between the two lists when paired by sorted position
- **Part 2**: Similarity score based on frequency matching between lists

---

## Part 1 Analysis

### Problem Statement
Pair up the smallest number in the left list with the smallest in the right list, the second-smallest with the second-smallest, and so on. Calculate the sum of absolute differences between all pairs.

### Algorithm Overview
1. Parse input into two separate lists
2. Sort both lists independently in ascending order
3. Iterate through both sorted lists simultaneously
4. For each index, compute the absolute difference between the left and right values
5. Sum all differences to get the total distance

### Key Data Structures
- **Two arrays/lists**: One for each column of input
- No additional data structures needed for Part 1

### Example Walkthrough
Given the example input, after sorting:
- Left: `[1, 2, 3, 3, 3, 4]`
- Right: `[3, 3, 3, 4, 5, 9]`

Pairs and distances: `|1-3| + |2-3| + |3-3| + |3-4| + |3-5| + |4-9| = 2 + 1 + 0 + 1 + 2 + 5 = 11`

---

## Part 2 Analysis

### How Part 2 Changes the Problem
Instead of measuring distance, Part 2 introduces a "similarity score" concept. For each number in the left list, multiply it by the count of how many times that number appears in the right list, then sum all these products.

### Additional Complexity
- Requires frequency counting rather than simple comparison
- Each left list value contributes `value * count_in_right_list` to the score
- Numbers not appearing in the right list contribute 0

### Algorithm Modifications
1. Build a frequency map (hash table) of all values in the right list
2. For each value in the left list:
   - Look up its frequency in the right list (0 if not present)
   - Multiply the value by its frequency
3. Sum all products

### Example Walkthrough
Using the example:
- Left list: `[3, 4, 2, 1, 3, 3]`
- Right list frequency: `{3: 3, 4: 1, 5: 1, 9: 1}`
- Score: `3*3 + 4*1 + 2*0 + 1*0 + 3*3 + 3*3 = 9 + 4 + 0 + 0 + 9 + 9 = 31`

---

## Algorithmic Approach

### Key Insight
- **Part 1**: The optimal pairing for minimum total distance is achieved by sorting both lists and pairing elements by rank. This is a classic result from combinatorics.
- **Part 2**: Frequency counting with a hash map enables O(1) lookup for each left list element.

### Data Structures
| Part | Structure | Purpose |
|------|-----------|---------|
| Part 1 | Sorted arrays | Enable positional pairing |
| Part 2 | Hash map / dictionary | O(1) frequency lookup |

### Time Complexity
- **Part 1**: O(n log n) - dominated by sorting
- **Part 2**: O(n) - linear scan to build frequency map, linear scan to compute score

### Space Complexity
- **Part 1**: O(n) - storing two lists (or O(1) extra if sorting in place)
- **Part 2**: O(k) where k is the number of unique values in the right list

---

## Programming Techniques Highlighted

### Computer Science Concepts
1. **Sorting algorithms** - Fundamental operation for Part 1
2. **Hash tables / frequency maps** - Key data structure for Part 2
3. **Array pairing** - Positional correspondence after sorting

### Mathematical Properties
- **Optimal assignment**: Sorting both lists and pairing by position minimizes total distance (can be proven via exchange argument)
- **Multiset intersection**: Part 2 essentially computes a weighted intersection count

### Common Patterns
- Input parsing with whitespace splitting
- Functional programming style (map, reduce, zip operations)
- Two-pass algorithms (build structure, then query)

---

## Language-Specific Implementation Notes

### Systems Languages (C, C++, Rust, Zig, ARM64)
- **C**: Implements a custom hash table with linear probing for Part 2. Uses `qsort` for sorting. Manual memory management required.
- **C++**: Can leverage `std::sort` and `std::unordered_map` for cleaner code.
- **ARM64 Assembly**: Requires careful register management and manual implementation of sorting/hashing algorithms. Most complex implementation.
- **Rust/Zig**: Benefit from built-in sorting and hash map types with safety guarantees.

### Garbage-Collected Languages (Go, Java, Node.js)
- **Go**: Uses `sort.Ints()` and built-in `map[int]int` for frequency counting. Clean and idiomatic.
- **Java**: Collections framework provides `Arrays.sort()` and `HashMap`.
- **Node.js**: JavaScript's `Map` object and array spread operator (`[...arr].sort()`) make implementation concise.

### Scripting Languages (Python, Ruby, PHP, Perl, Bash)
- **Python**: Most concise implementation using list comprehensions, `zip()`, and `dict.get()` with defaults.
- **Ruby**: Similar to Python with blocks and hash defaults.
- **PHP**: Uses `sort()` (modifies in place) and associative arrays.
- **Perl**: Hash-based frequency counting is idiomatic. Uses `sort` function.
- **Bash**: Slowest by far due to lack of native data structures. Requires external tools or arrays with linear search.

### Functional Languages (Clojure, Common Lisp)
- **Clojure**: Uses `frequencies` function for Part 2, `sort` for Part 1. Immutable by default.
- **Common Lisp**: Requires manual frequency counting with hash tables or alists.

### ColdFusion
- Requires Java interop or CFML structures. Highest memory overhead due to JVM + CFML runtime.

### Languages Naturally Suited to This Problem
- **Python**: Built-in sorting, dictionary operations, and `zip()` make this trivial
- **Go**: Clean map syntax and efficient sorting
- **Perl**: Excellent hash support and text processing

### Languages Requiring Workarounds
- **Bash**: No native hash maps (prior to Bash 4), requires creative solutions
- **ARM64 Assembly**: Everything must be built from scratch
- **C**: Manual hash table implementation needed

---

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 6.6          | 1.9         |
| C           | 6.7          | 1.9         |
| Rust        | 7.2          | 2.1         |
| C++         | 7.3          | 1.9         |
| Go          | 7.7          | 4.2         |
| Perl        | 14.5         | 5.1         |
| Zig         | 16.6         | 1.9         |
| Python      | 28.9         | 15.9        |
| Lisp        | 40.0         | 38.3        |
| Node.js     | 45.5         | 42.2        |
| Java        | 59.1         | 51.5        |
| Ruby        | 97.5         | 28.3        |
| PHP         | 110.5        | 24.8        |
| Clojure     | 405.4        | 129.2       |
| Bash        | 3,381.8      | 2.1         |
| ColdFusion  | 4,654.0      | 1,012.9     |

### Performance Analysis

**Fastest tier (< 10ms)**: Systems languages (C, ARM64, Rust, C++, Go) dominate due to compiled native code and minimal runtime overhead.

**Mid tier (10-100ms)**: Perl performs surprisingly well for a scripting language. Python benefits from optimized C implementations of sorting. Zig's performance is unexpectedly slower, possibly due to implementation choices.

**Slower tier (100-500ms)**: Ruby, PHP, and Clojure have higher interpreter/runtime overhead.

**Slowest tier (> 1000ms)**: Bash suffers from spawning subprocesses and lack of efficient data structures. ColdFusion's massive memory footprint (~1GB) reflects the JVM + CFML runtime overhead.

---

## Answers

| Part | Answer |
|------|--------|
| Part 1 | 2000468 |
| Part 2 | 18567089 |

---

## Implementation Checklist

- [x] ARM64 Assembly
- [x] C
- [x] C++
- [x] Rust
- [x] Zig
- [x] Go
- [x] Java
- [x] Node.js
- [x] Python
- [x] Ruby
- [x] PHP
- [x] Perl
- [x] Bash
- [x] Clojure
- [x] Common Lisp
- [x] ColdFusion
