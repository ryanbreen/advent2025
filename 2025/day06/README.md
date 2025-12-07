# Day 6: Trash Compactor

## Problem Summary

You've fallen into a garbage compactor and are trapped while waiting for friendly cephalopods to magnetically unseal the door. To pass the time, you help the youngest cephalopod with her math homework.

### Input Format

The input is a horizontally-arranged worksheet of math problems. Each problem consists of:
- Numbers stacked vertically in columns
- An operator (`+` or `*`) at the bottom indicating the operation
- Problems are separated by columns containing only spaces

Example:
```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
```

### What We Compute

- **Part 1**: Read each problem as numbers appearing horizontally (row-by-row), apply the operator, and sum all results.
- **Part 2**: Re-interpret the input where each column within a problem represents a single multi-digit number read top-to-bottom, and numbers are read right-to-left.

---

## Part 1 Analysis

### What Does Part 1 Ask For?

Parse the worksheet traditionally: each row within a problem block contains one number. Apply the indicated operator (`+` for sum, `*` for product) to all numbers in the problem, then sum all problem results.

From the example:
- Problem 1: `123 * 45 * 6 = 33210`
- Problem 2: `328 + 64 + 98 = 490`
- Problem 3: `51 * 387 * 215 = 4243455`
- Problem 4: `64 + 23 + 314 = 401`
- **Grand Total**: `4277556`

### Algorithm Overview

1. Find the operator row (last non-empty row containing only `+`, `*`, and spaces)
2. Identify problem boundaries by finding separator columns (all spaces from top to operator row)
3. For each problem block:
   - Extract numbers from each row (trim whitespace, parse as integer)
   - Extract the operator
   - Compute the result
4. Sum all problem results

### Key Data Structures

- **Padded line array**: All rows padded to the same width for consistent column indexing
- **Problem tuples**: List of `(numbers[], operator)` pairs

---

## Part 2 Analysis

### How Does Part 2 Change the Problem?

The twist reveals that "cephalopod math" is written differently:
- Numbers are read **column-by-column** (not row-by-row)
- Columns are processed **right-to-left** within each problem
- Each column's digits read **top-to-bottom** form a single number (most significant digit at top)

### What Additional Complexity Is Required?

This requires a fundamental change in parsing logic:
- Instead of extracting horizontal number strings from rows, we extract vertical digit sequences from columns
- Column iteration must be reversed (right-to-left)
- Sparse columns (with spaces between digits) must be handled correctly

From the example, reading right-to-left:
- Rightmost problem: column digits form `4`, `431`, `623` -> sum = `1058`
- Second from right: `175 * 581 * 32 = 3253600`
- Third from right: `8 + 248 + 369 = 625`
- Leftmost: `356 * 24 * 1 = 8544`
- **New Grand Total**: `3263827`

### Algorithm Modifications

The core change is in number extraction:
```python
# Part 1: Extract horizontal number from each row
for row in padded_number_rows:
    num_str = row[start_col:end_col].strip()
    if num_str:
        numbers.append(int(num_str))

# Part 2: Extract vertical number from each column (right-to-left)
for c in range(end_col - 1, start_col - 1, -1):  # Right to left
    digits = []
    for row in padded_number_rows:
        if row[c].isdigit():
            digits.append(row[c])
    if digits:
        numbers.append(int(''.join(digits)))
```

---

## Algorithmic Approach

### Key Insight

The critical realization is that problem boundaries are defined by **separator columns** - columns where every cell (including the operator row) is a space. This is the unifying concept for both parts; only the number extraction within problem bounds differs.

### Data Structures

| Structure | Purpose |
|-----------|---------|
| Padded string array | Uniform width for consistent column access |
| Character matrix | O(1) random access to any cell (row, column) |
| Problem list | Accumulated `(numbers, operator)` pairs |

### Time Complexity

- **O(R x W)** where R = number of rows, W = worksheet width
- Each cell is visited a constant number of times during:
  - Separator detection
  - Number extraction
  - Digit concatenation

### Space Complexity

- **O(R x W)** for the padded worksheet representation
- **O(P)** additional space for P problems (each problem stores its numbers)

---

## Programming Techniques Highlighted

### Concepts Tested

1. **2D Grid Parsing**: Working with positional data in a character matrix
2. **Column-Major vs Row-Major Access**: Part 2 requires transposing the mental model
3. **Boundary Detection**: Identifying problem separators via column analysis
4. **String-to-Integer Conversion**: Building numbers from digit characters

### Mathematical Properties

- **Associativity of operations**: The order of operations within a problem doesn't matter for `+` or `*`
- **No operator precedence**: Each problem uses a single operator type

### Parsing Patterns

The problem demonstrates a common AoC pattern: **positional parsing** where meaning is derived from spatial arrangement rather than explicit delimiters.

---

## Language-Specific Implementation Notes

### Naturally Suited Languages

**Python** excels here due to:
- Flexible string slicing: `row[start_col:end_col]`
- Built-in `strip()` and `isdigit()` methods
- `math.prod()` for multiplication reduction
- Dynamic typing allows easy list building

**Ruby** offers similar expressiveness with its string manipulation and functional iteration.

### Languages Requiring Extra Care

**C** requires:
- Manual memory management for padded strings and number arrays
- Explicit string padding with loops
- Careful bounds checking for column access
- Character-by-character digit extraction

**ARM64 Assembly** is the most challenging:
- No native string operations
- Manual loop construction for all traversals
- Register pressure during nested column/row iteration
- Requires careful tracking of column indices

**Java** uses `BigInteger` to avoid overflow concerns with large products, adding verbosity but ensuring correctness.

### Performance Characteristics

| Language Family | Relative Performance | Notes |
|----------------|---------------------|-------|
| Systems (C, Rust, Zig) | Fastest | Direct memory access, minimal overhead |
| JVM (Java, Clojure) | Fast | JIT compilation helps, startup cost amortized |
| Scripting (Python, Ruby, Perl) | Moderate | Interpreter overhead, but simple operations |
| Shell (Bash) | Slowest | Fork/exec overhead, no native integer arrays |

**Rust** provides a good balance with:
- Zero-cost abstractions for iteration
- `chars().collect()` for efficient character access
- Pattern matching for operator handling
- Memory safety without GC pause

### Notable Differences

- **Clojure/Lisp**: Functional approach with `reduce` for summing/multiplying; immutable data structures
- **PHP**: Associative arrays and `array_map` enable concise transformations
- **Perl**: Regex-friendly but this problem benefits more from positional indexing
- **ColdFusion (CFML)**: Array functions like `ArraySum()` provide built-in reductions

---

## Answers

| Part | Answer |
|------|--------|
| Part 1 | **4951502530386** |
| Part 2 | **8486156119946** |

---

## Benchmark Results

| Language | Runtime (ms) | Memory (MB) |
|----------|-------------|-------------|
| ARM64 Assembly | - | - |
| C | - | - |
| C++ | - | - |
| Rust | - | - |
| Zig | - | - |
| Go | - | - |
| Java | - | - |
| Node.js | - | - |
| Python | - | - |
| Ruby | - | - |
| PHP | - | - |
| Perl | - | - |
| Bash | - | - |
| Clojure | - | - |
| Common Lisp | - | - |
| ColdFusion | - | - |

*Note: Benchmarks to be filled in after running `python3 runner/benchmark.py` on each implementation.*
