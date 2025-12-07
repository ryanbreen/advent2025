# Day 1: Trebuchet?!

## Problem Summary

The Elves are launching you via trebuchet to investigate snow production issues, but their calibration document has been vandalized by an artistic young Elf. Each line of the document contains a calibration value that must be recovered.

### Input Format
- A text file with multiple lines
- Each line contains a mix of letters and digits
- Lines may contain spelled-out digit words (one, two, three, etc.)

### What We're Computing
- **Part 1**: For each line, find the first and last **numeric digit**, combine them into a two-digit number, and sum all calibration values
- **Part 2**: Same as Part 1, but spelled-out digits (one, two, three, four, five, six, seven, eight, nine) also count as valid digits

## Part 1 Analysis

### What Does Part 1 Ask For?
Extract the first and last numeric digit from each line, form a two-digit number, and compute the sum across all lines.

### Algorithm Overview
1. For each line, scan left-to-right to find the first digit
2. Scan right-to-left (or track during forward scan) to find the last digit
3. Combine as: `first * 10 + last`
4. Sum all calibration values

### Key Data Structures
- Simple character iteration - no complex structures needed
- A running sum accumulator

### Example
```
1abc2      -> first=1, last=2 -> 12
pqr3stu8vwx -> first=3, last=8 -> 38
a1b2c3d4e5f -> first=1, last=5 -> 15
treb7uchet -> first=7, last=7 -> 77
                          Total: 142
```

## Part 2 Analysis

### How Does Part 2 Change the Problem?
Spelled-out digit words now count as valid digits. The strings "one", "two", "three", etc. should be recognized as 1, 2, 3, etc.

### Additional Complexity
The key challenge is **overlapping matches**. Consider:
- `eightwothree` - starts with "eight" (8), ends with "three" (3) -> 83
- `twone` - "two" (2) and "one" (1) overlap at the 'o'

This means you cannot simply replace words with digits in the string (as "twone" would become ambiguous). Instead, you must check at each position whether a digit word starts there.

### Algorithm Modifications
1. At each position in the line:
   - Check if the character is a numeric digit
   - If not, check if any digit word starts at this position
2. Track first and last digit found (whether numeric or word-based)
3. Continue the same summation logic

### Example
```
two1nine      -> first=2, last=9 -> 29
eightwothree  -> first=8, last=3 -> 83
xtwone3four   -> first=2, last=4 -> 24
zoneight234   -> first=1, last=4 -> 14
```

## Algorithmic Approach

### Key Insight
The crucial realization for Part 2 is that digit words can overlap (e.g., "oneight", "twone", "eighthree"). A simple find-and-replace approach fails because it destroys potential matches. The correct approach is to scan character-by-character and check if a digit word **starts** at each position, without consuming characters.

### Data Structures
- **Word-to-digit mapping**: Array or dictionary of 9 entries (one->1, two->2, ..., nine->9)
- **Position tracking**: Simple integer indices for current position, first digit, last digit

### Time Complexity
- **Part 1**: O(n) where n is total characters across all lines - single pass through input
- **Part 2**: O(n * k) where k is the number of digit words (9) and average word length (constant ~4)
- Effectively O(n) since k is constant

### Space Complexity
- O(1) auxiliary space beyond the input storage
- The word mapping is constant size (9 entries)

## Programming Techniques Highlighted

### CS Concepts Tested
1. **String scanning and pattern matching** - Finding substrings at specific positions
2. **Handling overlapping patterns** - Understanding why greedy replacement fails
3. **Edge cases** - Lines with single digit, lines where first and last digit are the same position

### Mathematical Properties
- None significant - this is primarily a string processing problem

### Common Pitfalls
1. Using regex replacement for digit words (fails on overlaps)
2. Not handling lines with only one digit (first == last)
3. Off-by-one errors in substring extraction

## Language-Specific Implementation Notes

### Naturally Suited Languages

**Python** excels here due to:
- Built-in `str.startswith()` method for clean word matching
- List comprehensions for concise digit extraction
- `enumerate()` for position-aware iteration

**Rust** benefits from:
- Iterator chains with `filter_map` for elegant functional style
- Strong string slicing with `starts_with()`
- Pattern matching in functional pipelines

### Languages Requiring Workarounds

**Bash** faces significant challenges:
- No native string slicing syntax - must use `${var:pos:len}`
- Loop iteration over characters is verbose
- No built-in associative array iteration order guarantee
- Results in dramatically slower execution (5+ seconds vs milliseconds)

**C** requires manual memory management:
- Must cache `strlen()` to avoid O(n) calls in loops
- Uses `memcmp` instead of `strncmp` for performance
- Explicit struct for word-to-digit mapping

### Performance Characteristics

**Compiled Systems Languages** (Zig, C++, Rust, C):
- Fastest execution at 6-7ms
- Minimal memory footprint (~2MB)
- Direct memory access and optimized string operations

**Garbage-Collected Languages** (Go, Java):
- Moderate overhead from GC
- Go performs well (9ms) due to efficient runtime
- Java's JVM startup adds latency (70ms)

**Scripting Languages** (Python, Ruby, PHP, Perl, Node.js):
- Interpreter overhead adds 40-90ms
- Higher memory usage from dynamic typing

**JVM Functional Languages** (Clojure):
- Significant startup overhead (~500ms)
- Very high memory for immutable data structures

**Shell Scripting** (Bash):
- Extremely slow (~5 seconds) due to subprocess creation and string manipulation limitations

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 6.5          | 1.9         |
| ARM64 asm   | 6.5          | 1.9         |
| C++         | 6.6          | 1.4         |
| Rust        | 7.0          | 1.9         |
| C           | 7.4          | 1.9         |
| Go          | 9.1          | 4.2         |
| Lisp        | 27.1         | 39.2        |
| Perl        | 40.3         | 6.0         |
| Python      | 41.1         | 15.6        |
| Node.js     | 57.8         | 45.7        |
| Java        | 69.8         | 55.8        |
| PHP         | 69.0         | 24.6        |
| Ruby        | 92.3         | 28.2        |
| Clojure     | 555.6        | 486.0       |
| ColdFusion  | 2,910.6      | 1,163.7     |
| Bash        | 5,112.4      | 2.1         |

### Performance Tiers

1. **Sub-10ms** (Zig, ARM64, C++, Rust, C, Go): Native compilation with minimal overhead
2. **30-100ms** (Lisp, Perl, Python, Node.js, Java, PHP, Ruby): Interpreted or JIT-compiled
3. **500ms+** (Clojure, ColdFusion, Bash): Significant startup or execution overhead

## Answers

- **Part 1**: 56042
- **Part 2**: 55358
