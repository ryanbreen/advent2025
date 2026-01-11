# Day 3: Binary Diagnostic

## Problem Summary

The submarine's diagnostic report contains binary numbers that need to be analyzed for power consumption and life support ratings.

**Part 1**: Calculate power consumption by finding:
- **Gamma rate**: For each bit position, use the most common bit across all numbers
- **Epsilon rate**: For each bit position, use the least common bit (bitwise complement of gamma)
- Answer: gamma × epsilon

**Part 2**: Calculate life support rating using iterative filtering:
- **Oxygen generator rating**: Filter numbers by most common bit at each position (ties favor '1')
- **CO2 scrubber rating**: Filter numbers by least common bit at each position (ties favor '0')
- Answer: oxygen × CO2

## Algorithmic Approach

### Part 1 Algorithm

For each bit position (0 to width-1):
1. Count how many numbers have '1' at that position
2. If ones >= zeros, set that bit in gamma
3. Epsilon = gamma XOR mask (all 1s within bit width)

**Time Complexity**: O(n × w) where n = number count, w = bit width
**Space Complexity**: O(1)

### Part 2 Algorithm

For each rating (oxygen and CO2):
1. Start with all numbers as candidates
2. For each bit position (left to right):
   - Count ones vs zeros among remaining candidates
   - Determine target bit based on criteria (most/least common, with tie-breaker)
   - Filter candidates to only those matching target bit
   - Stop if one candidate remains
3. Convert remaining binary string to decimal

**Time Complexity**: O(n × w) for each rating
**Space Complexity**: O(n) for candidate lists

### Key Insight

Part 2's iterative filtering is a binary search tree traversal where each bit position splits the search space. The tie-breaking rules ('1' for oxygen, '0' for CO2) ensure deterministic results when the data is evenly split.

## Programming Techniques Highlighted

- **Bit Manipulation**: Building integers bit by bit, XOR for complement
- **Binary String Parsing**: Converting between string and integer representations
- **Filtering/Reduction**: Iteratively narrowing candidate sets
- **Counting Patterns**: Aggregating bit frequencies across positions

## Data Structures Used

- Array of binary strings (input)
- Candidate list for filtering (Part 2)
- Integer accumulators for gamma/epsilon

## Language-Specific Notes

- **Compiled languages (Zig, ARM64, C++, C, Rust)**: All ~5-7ms, bit manipulation is native
- **Go**: 7.4ms, slightly slower due to string handling overhead
- **Perl**: 11.6ms - efficient text processing
- **Common Lisp**: 23.4ms, good performance with vectors
- **Python**: 27.9ms, list comprehensions make filtering clean
- **Node.js**: 46.8ms, array filter operations work well
- **Bash**: 102.4ms - string manipulation is slower, but still reasonable
- **Clojure**: 432ms with JVM startup, uses functional filtering idioms
- **ColdFusion**: 2.5s with CommandBox overhead

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 5.5          | 1.9         |
| ARM64 asm   | 5.5          | 1.9         |
| C++         | 6.1          | 1.9         |
| C           | 6.6          | 1.9         |
| Rust        | 6.7          | 1.9         |
| Go          | 7.4          | 4.1         |
| Perl        | 11.6         | 8.6         |
| Common Lisp | 23.4         | 38.9        |
| Python      | 27.9         | 15.1        |
| Node.js     | 46.8         | 42.3        |
| Java        | 58.3         | 46.1        |
| Ruby        | 60.6         | 28.0        |
| PHP         | 66.0         | 25.7        |
| Bash        | 102.4        | 2.2         |
| Clojure     | 431.7        | 128.4       |
| ColdFusion  | 2,462.7      | 1,125.8     |

## Answers

- **Part 1**: 2743844
- **Part 2**: 6677951
