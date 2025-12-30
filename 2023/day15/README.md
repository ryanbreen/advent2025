# Day 15: Lens Library

## Problem Summary

You need to help initialize the Lava Production Facility using the Holiday ASCII String Helper (HASH) algorithm and HASHMAP procedure.

**Part 1**: Calculate the sum of HASH values for all comma-separated initialization steps.

**Part 2**: Execute the HASHMAP procedure on 256 boxes containing lenses, then calculate the total focusing power.

## Algorithmic Approach

### HASH Algorithm

For each character in a string:
```
current = ((current + ASCII_value) * 17) % 256
```

This produces a value from 0-255 that determines which box a lens belongs to.

### Part 1: Simple Hashing

Split the input by commas, hash each step, and sum the results.

### Part 2: HASHMAP Procedure

Maintain 256 boxes, each containing an ordered list of (label, focal_length) pairs:

- **`label-`**: Remove the lens with that label from box `hash(label)`
- **`label=N`**: Add or replace a lens with focal length N in box `hash(label)`

**Focusing Power** = Sum of `(box_num + 1) × slot × focal_length` for all lenses

### Complexity

- **Part 1**: O(n) where n is total characters in input
- **Part 2**: O(s × b) where s is number of steps and b is average box size
- **Space**: O(256 × avg_lenses_per_box) ≈ O(number_of_unique_labels)

## Programming Techniques Highlighted

- **Hashing**: Simple custom hash function
- **Ordered maps**: Maintaining insertion order while allowing updates
- **String parsing**: Splitting and pattern matching

## Language-Specific Notes

- **Fast performers**: All compiled languages perform similarly (~6ms) due to simple string operations
- **Perl**: Surprisingly fast (17ms) for this string-heavy problem
- **Python**: Benefits from efficient string handling (32ms)
- **JVM languages**: Startup overhead dominates for this quick problem

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C++         | 5.8          | 1.9         |
| ARM64       | 6.2          | 1.9         |
| Zig         | 6.5          | 1.9         |
| C           | 6.6          | 1.9         |
| Rust        | 7.0          | 1.9         |
| Go          | 7.3          | 4.3         |
| Perl        | 16.8         | 4.9         |
| Common Lisp | 30.4         | 40.7        |
| Python      | 31.8         | 16.2        |
| Java        | 52.6         | 50.6        |
| Node.js     | 52.1         | 44.4        |
| PHP         | 56.0         | 26.4        |
| Ruby        | 64.0         | 28.9        |
| Clojure     | 446.1        | 139.3       |
| Bash        | ~2000        | ~10         |
| ColdFusion  | 2695.3       | 1168.3      |

## Answers

- Part 1: **517965**
- Part 2: **267372**
