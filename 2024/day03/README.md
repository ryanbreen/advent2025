# Day 3: Mull It Over

## Problem Summary

The North Pole Toboggan Rental Shop's computer has corrupted memory, and we need to help recover the output of a simple multiplication program. The input is a single string of "corrupted" memory containing both valid and invalid instruction sequences jumbled together with garbage characters.

**Input Format:** A single continuous string containing corrupted computer memory with embedded instructions.

**Part 1:** Find all valid `mul(X,Y)` instructions (where X and Y are 1-3 digit numbers) and sum their products.

**Part 2:** The same as Part 1, but now `do()` and `don't()` instructions toggle whether subsequent `mul` instructions are enabled or disabled.

## Part 1 Analysis

### What does Part 1 ask for?

Extract all valid multiplication instructions from the corrupted memory and compute the sum of their products. A valid instruction must be exactly in the form `mul(X,Y)` where:
- `mul(` is the literal prefix
- `X` is a 1-3 digit number
- `,` is a literal comma (no spaces)
- `Y` is a 1-3 digit number
- `)` is the literal closing parenthesis

Invalid patterns like `mul(4*`, `mul(6,9!`, `mul ( 2 , 4 )`, or `mul[3,7]` must be ignored.

### Algorithm Overview

1. Scan through the input string character by character (or use regex)
2. When a potential `mul(` is found, attempt to parse the full instruction
3. Validate the format strictly: no spaces, exactly 1-3 digits for each operand
4. If valid, multiply the two operands and add to running total
5. Continue scanning from the next character position

### Key Data Structures

- **String/byte array**: The input data to scan
- **Integer accumulator**: Running sum of valid multiplication products
- No complex data structures needed for Part 1

## Part 2 Analysis

### How does Part 2 change the problem?

Part 2 introduces conditional execution through two new control instructions:
- `do()` - Enables subsequent `mul` instructions
- `don't()` - Disables subsequent `mul` instructions

The program starts in the enabled state. When disabled, `mul` instructions are still parsed but their products are not added to the sum.

### Additional Complexity

The key insight is that instructions must be processed in the order they appear in the input. This requires:
1. Finding all three instruction types with their positions
2. Sorting or processing them in positional order
3. Maintaining an "enabled" state that toggles based on `do()`/`don't()` instructions

### Algorithm Modifications

Two main approaches exist:

**Approach 1: Event Collection and Sorting**
1. Find all `mul(X,Y)` instructions and record their positions
2. Find all `do()` instructions and record their positions
3. Find all `don't()` instructions and record their positions
4. Sort all events by position
5. Process events in order, maintaining enabled/disabled state

**Approach 2: Single-Pass State Machine**
1. Scan through input sequentially
2. At each position, check for `do()`, `don't()`, or `mul(X,Y)`
3. Update enabled state or add to sum as appropriate
4. Continue to next character

The Rust implementation uses Approach 2, while Python and C use Approach 1.

## Algorithmic Approach

### Key Insight

This is fundamentally a **pattern matching and state machine** problem. The critical realization is that:
1. Pattern matching must be exact (no fuzzy matching)
2. For Part 2, the order of instructions in the input determines program behavior
3. A simple boolean state is sufficient to track enable/disable status

### Data Structures

| Structure | Purpose |
|-----------|---------|
| String/byte buffer | Hold the input data |
| Event list (Part 2) | Store (position, type, operands) tuples for sorting |
| Boolean flag | Track enabled/disabled state in Part 2 |
| Integer accumulator | Running sum of products |

### Time Complexity

- **Part 1:** O(n) where n is input length - single pass through input
- **Part 2:**
  - Event collection approach: O(n) to find events + O(k log k) to sort where k is number of events
  - Single-pass approach: O(n)

### Space Complexity

- **Part 1:** O(1) - only need the accumulator
- **Part 2:**
  - Event collection: O(k) where k is the number of instructions found
  - Single-pass: O(1)

## Programming Techniques Highlighted

### Computer Science Concepts

1. **Regular Expressions**: The problem is a natural fit for regex pattern matching
2. **Lexical Analysis**: Parsing a simple instruction set from a character stream
3. **State Machines**: Part 2 requires maintaining and transitioning between enabled/disabled states
4. **Event-Driven Processing**: Collecting and sorting events by position

### Mathematical Properties

- Simple integer multiplication and summation
- No overflow concerns with 1-3 digit operands (max product: 999 * 999 = 998,001)

## Language-Specific Implementation Notes

### Naturally Suited Languages

**Perl** stands out as exceptionally well-suited for this problem. Its regex-first design makes the pattern matching trivial:
```perl
while ($data =~ /mul\((\d{1,3}),(\d{1,3})\)/g) {
    $total += $1 * $2;
}
```
The built-in `$-[0]` variable automatically captures match positions, making Part 2 equally elegant.

**Python** with its `re` module provides similarly clean regex handling:
```python
pattern = r'mul\((\d{1,3}),(\d{1,3})\)'
matches = re.findall(pattern, data)
return sum(int(x) * int(y) for x, y in matches)
```

### Manual Parsing Approaches

**C, Rust, and Zig** demonstrate manual byte-by-byte parsing. While more verbose, this approach:
- Avoids regex library overhead
- Gives precise control over parsing behavior
- Often results in faster execution

The Rust implementation shows a clean single-pass state machine:
```rust
while i < bytes.len() {
    // Check for "do()", "don't()", or "mul(" at current position
    // Update state or accumulate sum accordingly
}
```

### Performance Characteristics

| Language Family | Approach | Trade-offs |
|-----------------|----------|------------|
| Systems (C, Rust, Zig) | Manual parsing | Fastest, most verbose |
| Scripting (Python, Perl, Ruby) | Regex | Concise, slightly slower |
| JVM (Java, Clojure) | Regex with objects | Higher startup overhead |
| Shell (Bash) | grep/sed pipelines | Surprisingly fast for this problem |

**Notable Observations:**
- **Bash** (78.6ms) performs exceptionally well on this problem compared to other days, as it can leverage Unix text tools like `grep` effectively
- **Java** (335.6ms) is unusually slow, likely due to regex compilation and object allocation overhead
- **ARM64 assembly** and **Zig** tie at 6.5ms for fastest execution

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 6.5          | 1.3         |
| Zig         | 6.5          | 1.9         |
| C           | 7.4          | 1.9         |
| Rust        | 7.8          | 1.9         |
| Go          | 8.2          | 4.7         |
| C++         | 10.8         | 1.9         |
| Perl        | 17.6         | 6.4         |
| Lisp        | 27.9         | 39.9        |
| Python      | 29.4         | 15.9        |
| Node.js     | 46.2         | 39.6        |
| PHP         | 60.8         | 25.5        |
| Ruby        | 61.8         | 28.3        |
| Bash        | 78.6         | 1.9         |
| Java        | 335.6        | 127.0       |
| Clojure     | 424.7        | 127.7       |
| ColdFusion  | 2,726.8      | 1,135.2     |

## Answers

- **Part 1:** 175700056
- **Part 2:** 71668682
