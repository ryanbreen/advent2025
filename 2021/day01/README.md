# Day 1: Sonar Sweep

## Problem Summary

You're in a submarine searching for sleigh keys that fell into the ocean. The submarine performs a sonar sweep, giving you depth measurements as you descend.

**Part 1**: Count how many times the depth measurement increases from one reading to the next.

**Part 2**: To reduce noise, count how many times the sum of a 3-measurement sliding window increases from the previous window sum.

## Algorithmic Approach

### Part 1 Algorithm

Simple linear scan comparing adjacent elements:
- Iterate through measurements from index 1 to n-1
- Count when `depths[i] > depths[i-1]`

**Time Complexity**: O(n)
**Space Complexity**: O(1)

### Part 2 Algorithm

Naive approach: compute each window sum and compare consecutively.

**Optimized approach**: When comparing adjacent 3-element windows:
- Window A: `depths[i] + depths[i+1] + depths[i+2]`
- Window B: `depths[i+1] + depths[i+2] + depths[i+3]`

The middle two terms (`depths[i+1] + depths[i+2]`) cancel out, so:
- Window B > Window A is equivalent to `depths[i+3] > depths[i]`

This optimization eliminates the need to compute explicit sums.

**Time Complexity**: O(n)
**Space Complexity**: O(1) with optimization, O(n) without

### Key Insight

The sliding window comparison can be reduced to a single element comparison because consecutive windows share two elements. This is a common optimization pattern in sliding window problems.

## Programming Techniques Highlighted

- **Sliding Window**: Classic technique for processing contiguous subarrays
- **Mathematical Simplification**: Recognizing that shared terms cancel
- **Linear Scan**: Simple iteration with comparison

## Data Structures Used

- Array/Vector of integers for depth measurements
- No additional data structures needed

## Language-Specific Notes

- **Compiled languages (C, C++, Rust, Zig, ARM64)**: All complete in ~5-7ms, dominated by process startup time
- **Rust**: Elegant use of `windows()` iterator for idiomatic sliding window
- **Perl**: Surprisingly fast at 9ms, efficient for simple numeric processing
- **Common Lisp**: Good performance at 19ms using vectors instead of lists
- **Bash**: Reasonable 25ms using `mapfile` for array loading
- **Clojure**: Slower startup (~390ms) but clean functional solution with `partition`
- **ColdFusion**: JVM startup dominates at ~2.5s

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C++         | 5.3          | 1.9         |
| Rust        | 5.5          | 1.9         |
| ARM64 asm   | 5.7          | 1.9         |
| C           | 5.9          | 1.9         |
| Zig         | 6.5          | 1.9         |
| Go          | 7.1          | 4.1         |
| Perl        | 9.2          | 4.8         |
| Common Lisp | 19.3         | 38.2        |
| Bash        | 24.7         | 6.8         |
| Python      | 30.2         | 16.0        |
| Node.js     | 42.7         | 38.1        |
| PHP         | 50.2         | 25.5        |
| Ruby        | 56.9         | 28.1        |
| Java        | 59.3         | 47.7        |
| Clojure     | 389.8        | 134.3       |
| ColdFusion  | 2,458.8      | 1,079.4     |

## Answers

- **Part 1**: 1759
- **Part 2**: 1805
