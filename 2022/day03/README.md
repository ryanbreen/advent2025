# Day 3: Rucksack Reorganization

## Problem Summary

Each Elf has a rucksack with two compartments. Items are identified by single letters (a-z, A-Z). One item type per rucksack appears in both compartments incorrectly.

**Part 1**: Find the item type that appears in both compartments of each rucksack and sum their priorities.

**Part 2**: Find the badge item (common to all three Elves in each group of three) and sum their priorities.

## Input Format

Each line represents a rucksack's contents as a string of letters:
```
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
```

For Part 1, each line splits in half (first compartment / second compartment).
For Part 2, every three consecutive lines form a group.

## Priority System

- Lowercase `a-z`: priorities 1-26
- Uppercase `A-Z`: priorities 27-52

## Algorithmic Approach

### Part 1: Set Intersection (Two Sets)

1. Split each rucksack string at the midpoint
2. Convert each half to a set of characters
3. Find the intersection (common character)
4. Sum priorities

### Part 2: Set Intersection (Three Sets)

1. Group rucksacks in sets of 3
2. Convert each rucksack to a set of characters
3. Find the intersection of all three sets
4. Sum priorities

### Implementation Techniques

**Hash Set Approach**: O(n) time, O(k) space where k is alphabet size
- Build sets using hash maps/sets
- Intersection via membership testing

**Bitmask Approach**: O(n) time, O(1) space
- Use 52-bit integer as a set (one bit per letter)
- Intersection via bitwise AND
- Very efficient on modern CPUs

### Time Complexity

- O(n * m) where n = number of rucksacks, m = average rucksack length
- Each character processed once per set operation

### Space Complexity

- Hash set: O(52) = O(1) per rucksack (bounded alphabet)
- Bitmask: O(1) - single 64-bit integer per set

## Key Insight

The alphabet is small (52 characters), making bitmask operations extremely efficient. A single 64-bit integer can represent any subset of the alphabet, and intersection is just a bitwise AND.

## Programming Techniques Highlighted

- **Set operations**: Union, intersection using language-native sets
- **Bitmasks**: Compact set representation for small alphabets
- **String slicing**: Splitting strings at midpoints
- **Character arithmetic**: Converting chars to indices
- **Grouping**: Processing items in fixed-size chunks

## Language-Specific Notes

### Bitmask Implementations (< 7ms)
- **C**: Uses `uint64_t` bitmask, extremely efficient
- **ARM64**: Native 64-bit registers, bitwise operations
- **Zig**: Direct bit manipulation with compile-time safety
- **C++/Rust**: Can use either sets or bitmasks

### Shell/Scripting (18-25ms)
- **Bash**: AWK with associative arrays for sets
- **Perl**: Hash-based sets, efficient string handling
- **Common Lisp**: List-based sets with `intersection`

### Interpreted Languages (40-70ms)
- **Python**: Native `set` type with `&` operator
- **Node.js**: `Set` class with manual intersection
- **Ruby**: Array/Set with `&` operator
- **PHP**: `array_intersect()` function
- **Java**: `Set.retainAll()` for intersection

### Heavy Runtimes (400-2600ms)
- **Go**: Includes compilation time with `go run`
- **Clojure**: `clojure.set/intersection`, JVM startup
- **ColdFusion**: Required Java LinkedHashMap for case-sensitivity

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.4          | 1.9         |
| Zig         | 6.1          | 1.9         |
| ARM64 asm   | 6.3          | 1.9         |
| C++         | 6.4          | 1.9         |
| Rust        | 6.9          | 1.9         |
| Bash        | 18.5         | 6.7         |
| Perl        | 20.8         | 6.0         |
| Common Lisp | 24.0         | 39.5        |
| Python      | 41.2         | 14.9        |
| Node.js     | 46.3         | 40.0        |
| Java        | 52.4         | 48.1        |
| Ruby        | 63.0         | 28.2        |
| PHP         | 63.5         | 25.9        |
| Go          | 69.6         | 27.1        |
| Clojure     | 485.7        | 134.9       |
| ColdFusion  | 2,575.1      | 1,119.6     |

## Answers

- Part 1: 8233
- Part 2: 2821
