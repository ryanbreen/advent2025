# Day 2: Gift Shop

## Problem Summary

You're helping a gift shop clerk identify invalid product IDs in their database. A mischievous young Elf has added bogus entries by creating "silly pattern" IDs, and your task is to find and sum all these invalid IDs across a set of numeric ranges.

### Input Format

The input is a single line containing comma-separated ranges. Each range specifies a start and end ID separated by a dash:

```
92916254-92945956,5454498003-5454580069,28-45,4615-7998,...
```

### What We're Computing

- **Part 1**: Sum all IDs where the number is a sequence of digits repeated **exactly twice** (e.g., `1212`, `5555`, `123123`)
- **Part 2**: Sum all IDs where the number is a sequence of digits repeated **at least twice** (e.g., `111`, `1212`, `121212`)

## Part 1 Analysis

### Problem Statement

An ID is "invalid" if its string representation consists of some pattern repeated exactly twice. For example:
- `55` = `5` repeated twice
- `6464` = `64` repeated twice
- `123123` = `123` repeated twice

Numbers with leading zeros are not valid IDs (e.g., `0101` is not considered).

### Algorithm Overview

1. Parse each comma-separated range into start/end pairs
2. For each number in each range:
   - Convert to string
   - Check if length is even (required for exactly two repetitions)
   - Split string in half
   - Compare the two halves for equality
3. Sum all matching "invalid" IDs

### Key Data Structures

- **Ranges**: Array/list of (start, end) tuples
- **String buffer**: For numeric-to-string conversion and substring comparison

## Part 2 Analysis

### How Part 2 Changes the Problem

Part 2 generalizes the pattern matching: instead of requiring exactly two repetitions, we now accept **any** repetition count >= 2. This means:
- `111` (1 repeated 3 times) is now invalid
- `1212121212` (12 repeated 5 times) is now invalid
- `999999` (9 repeated 6 times, or 99 repeated 3 times, or 999 repeated 2 times) is now invalid

### Additional Complexity

The challenge shifts from a simple half-split comparison to trying multiple potential pattern lengths. We must:
1. Try all divisors of the string length as potential pattern sizes
2. For each valid pattern size, check if the entire string is that pattern repeated
3. Accept the number as invalid if ANY pattern length works

### Algorithm Modifications

```python
# Part 1: Only check length/2
if len(s) % 2 == 0 and s[:len(s)//2] == s[len(s)//2:]:
    return True

# Part 2: Try all pattern lengths from 1 to len/2
for pattern_len in range(1, len(s) // 2 + 1):
    if len(s) % pattern_len == 0:
        pattern = s[:pattern_len]
        if pattern * (len(s) // pattern_len) == s:
            return True
```

## Algorithmic Approach

### Key Insight

The crucial realization is that pattern matching can be reduced to **string multiplication**. Instead of iterating through the string comparing character by character, we can:
1. Extract a candidate pattern (the first N characters)
2. Repeat it the expected number of times
3. Compare the result to the original string

This is elegant because it leverages built-in string operations that are highly optimized in most languages.

### Data Structures

- **Primitive types only**: This problem requires no complex data structures
- **String/character arrays**: For pattern extraction and comparison
- **Integer accumulator**: For summing invalid IDs

### Time Complexity

Let `R` = number of ranges, `N` = average range size, `D` = max digits in a number

- **Part 1**: O(R * N * D) - linear scan with constant-time half comparison
- **Part 2**: O(R * N * D^2) - for each number, we try O(D) pattern lengths, each requiring O(D) comparison

In practice, ranges in the puzzle input vary from small (e.g., `28-45`) to large (e.g., `92916254-92945956`), but the number of digits remains manageable (up to 10-11 digits).

### Space Complexity

O(D) - only need storage for the string representation of the current number being checked.

## Programming Techniques Highlighted

### String Manipulation

This problem is fundamentally about **string pattern matching**. Key techniques:
- Substring extraction (`s[:n]`, `s[n:]`)
- String repetition/multiplication (`pattern * count`)
- Divisibility checking for valid pattern lengths

### Modular Arithmetic

Finding valid pattern lengths requires checking divisibility:
```c
if (len % pattern_len == 0) {
    // pattern_len is a valid candidate
}
```

### Early Termination

Efficient implementations can short-circuit:
- Part 1: Immediately return false for odd-length strings
- Part 2: Return true on first matching pattern (no need to try all)

### Mathematical Property Exploited

A repeating pattern of length `k` in a string of length `n` requires `n % k == 0`. This allows us to skip many invalid pattern lengths without checking them.

## Language-Specific Implementation Notes

### Languages Naturally Suited

**Python** excels here due to:
- Native string slicing (`s[:mid]`, `s[mid:]`)
- String multiplication (`pattern * n`)
- Clean, readable syntax for the core algorithm

**Go** provides a good balance with `strings.Repeat()` for pattern multiplication and efficient string comparison.

**Clojure** offers elegant functional expressions:
```clojure
(= s (apply str (repeat repetitions pattern)))
```

### Languages Requiring Workarounds

**C** requires manual character-by-character comparison since there's no native string multiplication:
```c
for (int i = pattern_len; i < len; i++) {
    if (str[i] != str[i % pattern_len]) {
        is_repeated = false;
        break;
    }
}
```

**Assembly (ARM64)** requires significant low-level work for:
- Number-to-string conversion
- String comparison loops
- Range parsing from input

### Performance Characteristics

| Language Family | Relative Performance | Notes |
|-----------------|---------------------|-------|
| C/C++/Zig | Fastest | Direct memory access, no GC |
| Go | Fast | Compiled, efficient runtime |
| Java/Clojure | Medium | JVM warmup, then competitive |
| Python/Ruby/PHP | Slower | Interpreter overhead |
| Bash/Perl | Slowest | Shell overhead, external commands |

### Notable Differences

1. **String immutability**: Languages like Python, Go, and Java create new strings for each pattern test, while C operates on the same buffer

2. **Integer size**: Large IDs (e.g., `8892865662`) require 64-bit integers; Python handles this automatically, while C/C++ need explicit `long long`

3. **Input parsing**: The single-line comma-separated format is trivial in high-level languages but requires careful tokenization in C

## Benchmark Results

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 37           | 1.9         |
| ARM64 asm   | 65           | 1.9         |
| Rust        | 136          | 1.9         |
| C++         | 198          | 1.9         |
| C           | 211          | 1.9         |
| Go          | 217          | 10.0        |
| Java        | 289          | 596.0       |
| Node.js     | 315          | 87.6        |
| Common Lisp | 587          | 89.4        |
| PHP         | 610          | 24.4        |
| Python      | 1,192        | 15.6        |
| Clojure     | 1,210        | 1,298       |
| Ruby        | 2,092        | 28.2        |
| Perl        | 2,461        | 4.3         |
| ColdFusion  | 6,909        | 1,141.8     |
| Bash        | 90,930       | 1.5         |

## Answers

- **Part 1**: 23039913998
- **Part 2**: 35950619148
