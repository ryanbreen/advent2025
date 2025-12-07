# Day 5: Cafeteria

## Problem Summary

The Elves in the kitchen have a complicated inventory management system for tracking which ingredients are fresh versus spoiled. The puzzle input is a database consisting of two sections separated by a blank line:

1. **Fresh ingredient ID ranges**: Lines in the format `start-end` (e.g., `3-5`, `10-14`) where the ranges are inclusive
2. **Available ingredient IDs**: Individual numbers, one per line

An ingredient is considered **fresh** if its ID falls within any of the defined ranges. Ranges can overlap, but an ingredient only needs to be in one range to be fresh.

### Input Format
```
3-5
10-14
16-20
12-18

1
5
8
11
17
32
```

## Part 1 Analysis

### What It Asks
Count how many of the available ingredient IDs (from section 2) are fresh, meaning they fall within at least one of the defined ranges.

### Algorithm Overview
For each ingredient ID in the list, check if it falls within any range. This is a straightforward membership test.

### Key Data Structures
- **List of tuples/pairs**: Store each range as `(start, end)`
- **List of integers**: Store the ingredient IDs to check

### Approach
```python
for ingredient_id in ingredient_ids:
    for start, end in ranges:
        if start <= ingredient_id <= end:
            fresh_count += 1
            break  # No need to check remaining ranges
```

The `break` optimization is important: once we find one matching range, we can stop checking. An ingredient being in multiple ranges does not count it multiple times.

## Part 2 Analysis

### How It Changes the Problem
Part 2 ignores the ingredient ID list entirely. Instead, we must count the **total number of unique IDs** that are considered fresh across all ranges.

### Additional Complexity
The challenge is that ranges can **overlap**. Simply summing `(end - start + 1)` for each range would double-count overlapping regions. For example:
- Range `10-14` covers 5 IDs
- Range `12-18` covers 7 IDs
- But together they only cover 9 unique IDs (10, 11, 12, 13, 14, 15, 16, 17, 18)

### Algorithm Modifications
We must **merge overlapping ranges** before counting:
1. Sort ranges by start position
2. Iterate through, merging any overlapping or adjacent ranges
3. Sum the sizes of the merged ranges

## Algorithmic Approach

### Key Insight
The crucial realization for Part 2 is that this is an **interval merging** problem. By sorting ranges by their start position, we can merge overlapping intervals in a single linear pass.

Two ranges can be merged if the second range's start is less than or equal to the first range's end plus one (handling adjacent ranges like `3-5` and `6-8`).

### Data Structures
- **List of ranges (tuples)**: For both parts
- **Sorted list**: Required for efficient merging in Part 2

### Time Complexity
- **Part 1**: O(n * m) where n = number of ingredient IDs, m = number of ranges
- **Part 2**: O(m log m) for sorting + O(m) for merging = O(m log m)

### Space Complexity
- **Part 1**: O(m) for storing ranges + O(n) for ingredient IDs
- **Part 2**: O(m) for the merged ranges list

### The Merge Algorithm
```python
# Sort by start position
ranges.sort(key=lambda x: x[0])

merged = []
for start, end in ranges:
    if merged and start <= merged[-1][1] + 1:
        # Overlapping or adjacent - extend the last range
        merged[-1] = (merged[-1][0], max(merged[-1][1], end))
    else:
        # No overlap - add as new range
        merged.append((start, end))

# Count unique IDs
total = sum(end - start + 1 for start, end in merged)
```

## Programming Techniques Highlighted

### CS Concepts Tested
1. **Interval/Range Processing**: A classic problem type in competitive programming
2. **Sorting as a Preprocessing Step**: Enables efficient single-pass algorithms
3. **Greedy Algorithms**: The merge step greedily combines adjacent ranges
4. **Two-Section Input Parsing**: Handling multiple data formats in one input file

### Mathematical Properties Exploited
- **Transitivity of Overlap**: If range A overlaps B and B overlaps C, sorting ensures we can merge them in sequence
- **Inclusive Range Counting**: For range `[a, b]`, the count is `b - a + 1`

## Language-Specific Implementation Notes

### Languages Naturally Suited

**Python** excels here due to:
- Native tuple support for ranges
- Built-in `sort()` with key functions
- Clean list comprehensions for parsing
- The `break` statement for early exit in Part 1

**Go** provides clean, readable code with:
- Struct-based range representation
- `sort.Slice()` for custom sorting
- Explicit but straightforward logic

### Languages Requiring Workarounds

**C** needs manual memory management:
- Static arrays with `MAX_RANGES` and `MAX_INGREDIENTS` constants
- Custom `qsort()` comparison function
- Uses `long long` for Part 2's large numbers (339+ trillion)

```c
typedef struct {
    long long start;
    long long end;
} Range;
```

**Clojure** uses a functional approach with `reduce`:
```clojure
(reduce
  (fn [acc [start end]]
    (if (and (not-empty acc)
             (<= start (inc (second (last acc)))))
      ;; Merge with last range
      (let [last-range (last acc)
            new-range [(first last-range) (max (second last-range) end)]]
        (conj (vec (butlast acc)) new-range))
      ;; Add as new range
      (conj acc [start end])))
  []
  sorted-ranges)
```
The immutable data structures mean we rebuild collections rather than mutating in place.

### Performance Characteristics

| Language Family | Characteristics |
|-----------------|-----------------|
| **Systems (C, C++, Zig)** | Fastest execution, manual memory management, need to handle integer overflow |
| **Compiled GC (Go, Java)** | Fast with automatic memory, clean struct/class syntax |
| **Scripting (Python, Ruby, Perl)** | Slower but most concise, dynamic typing simplifies parsing |
| **Functional (Clojure, Lisp)** | Elegant recursion/reduce patterns, immutable by default |

### Notable Differences

1. **Integer types**: C/C++ need `long long` for Part 2; dynamic languages handle arbitrary precision automatically
2. **Parsing**: Scripting languages have one-liner parsing; C requires `sscanf()` or manual parsing
3. **Sorting**: Most languages have built-in sort; Assembly (ARM64) requires a custom implementation
4. **Early exit**: `break` works in most languages, but Clojure uses `some` for short-circuit evaluation

## Answers

- **Part 1**: 513
- **Part 2**: 339668510830757
