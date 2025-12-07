# Day 3: Lobby

## Problem Summary

You're in the vast lobby of the underground complex, but the elevators are offline due to an electrical surge. To proceed, you need to power up the escalator using emergency batteries.

**Input Format:** Each line represents a "bank" of batteries, where each character is a digit from 1-9 representing that battery's joltage rating. The input consists of multiple lines, each containing a string of digits (typically 100 characters per line).

**What We're Computing:**
- **Part 1:** Select exactly 2 batteries from each bank to form a 2-digit number. Find the maximum possible 2-digit number for each bank and sum them all.
- **Part 2:** Select exactly 12 batteries from each bank to form a 12-digit number. Find the maximum possible 12-digit number for each bank and sum them all.

The key constraint is that batteries must maintain their relative positions - you cannot rearrange them. If you select batteries at positions 3 and 7, the resulting number uses the digit at position 3 as the tens digit and position 7 as the units digit.

---

## Part 1 Analysis

**What does Part 1 ask for?**

Given a string of digits, select exactly two positions (i, j) where i < j, and form the 2-digit number by concatenating digit[i] and digit[j]. Maximize this value for each bank, then sum across all banks.

**Algorithm Overview:**

For a naive approach, you could try all pairs (i, j) where i < j, which is O(n^2) per line. However, a more efficient approach uses the observation that:

1. For any fixed first position i, the optimal second position is the one with the maximum digit value from i+1 to end
2. We can precompute "max suffix" values to look this up in O(1)

**Key Data Structures:**
- `max_suffix[i]` = maximum digit value from position i to the end of the string

**Algorithm:**
```
1. Build max_suffix array from right to left
2. For each position i from 0 to n-2:
   - first_digit = line[i]
   - second_digit = max_suffix[i+1]
   - joltage = first_digit * 10 + second_digit
   - Track maximum joltage
3. Sum maximum joltages across all banks
```

---

## Part 2 Analysis

**How does Part 2 change the problem?**

Instead of selecting 2 batteries, we must select exactly 12. This fundamentally changes the problem from a simple optimization to a classic greedy selection problem.

**What additional complexity or insight is required?**

The key insight is that this becomes the "maximum subsequence of length k" problem. You're selecting k=12 digits while maintaining their relative order to form the largest possible number.

**Algorithm Modifications:**

The greedy approach works because:
- The first digit has the most significant place value
- We should always choose the largest possible digit for the leftmost unfilled position
- But we must ensure enough digits remain to fill the remaining positions

**Greedy Algorithm:**
```
For each of the 12 positions we need to fill:
    - remaining_needed = 12 - positions_filled - 1
    - search_end = n - remaining_needed  (we must leave room for remaining digits)
    - Find the maximum digit in range [current_pos, search_end)
    - Add that digit to our result
    - Move current_pos past the selected position
```

---

## Algorithmic Approach

### Key Insight

The crucial realization for Part 2 is that this is a **greedy subsequence selection** problem. When building a maximum number digit-by-digit from left to right:

1. The leftmost position has the highest place value (10^11 for a 12-digit number)
2. We should greedily pick the largest available digit for each position
3. The constraint is that we must leave enough digits remaining to complete the selection

This greedy approach is provably optimal because any suboptimal choice for a more significant digit position cannot be compensated by better choices at less significant positions.

### Data Structures

**Part 1:**
- **Max Suffix Array:** An array where `max_suffix[i]` stores the maximum digit value from index i to the end. Built in O(n) with a single right-to-left pass.

**Part 2:**
- **Result Array/String:** Accumulates the selected digits
- **Position Tracker:** Keeps track of current search starting position

### Time Complexity

**Part 1:**
- Building max suffix: O(n) per line
- Scanning all positions: O(n) per line
- Total: **O(n * L)** where L is number of lines

**Part 2:**
- For each line: k iterations, each searching up to n positions
- Worst case: O(k * n) per line
- Total: **O(k * n * L)** where k=12

With optimization (using data structures like segment trees or sparse tables), Part 2 could be reduced to O(k * log(n) * L), but for k=12 and n~100, the simple O(k*n) approach is efficient enough.

### Space Complexity

- **Part 1:** O(n) for the max suffix array
- **Part 2:** O(k) = O(12) = O(1) for the result buffer
- Overall: **O(n)** dominated by the suffix array

---

## Programming Techniques Highlighted

### CS Concepts Tested

1. **Greedy Algorithms:** Part 2 is a textbook example of when greedy selection produces an optimal result. The greedy choice property holds because place value significance decreases left-to-right.

2. **Suffix Maximum/Preprocessing:** Part 1 demonstrates the power of precomputation - by building a suffix maximum array, we transform an O(n^2) problem into O(n).

3. **Subsequence Selection:** Selecting elements while maintaining relative order is a fundamental pattern that appears in many algorithm problems.

4. **String as Numeric Data:** Treating strings as sequences of numeric values and understanding positional notation.

### Mathematical Properties Exploited

- **Place Value Dominance:** A larger digit in a more significant position always yields a larger number, regardless of less significant digits. For example, 91 > 89 even though 8+9 > 9+1.

- **Greedy Choice Property:** For maximum subsequence selection, the locally optimal choice (largest available digit that leaves room for remaining selections) leads to the globally optimal solution.

---

## Language-Specific Implementation Notes

### Languages Naturally Suited

**Python** is exceptionally well-suited due to:
- Native arbitrary-precision integers handle the 12-digit numbers effortlessly
- Clean list comprehensions for suffix array construction
- Simple string indexing and conversion

```python
max_suffix[-1] = int(line[-1])
for i in range(n - 2, -1, -1):
    max_suffix[i] = max(int(line[i]), max_suffix[i + 1])
```

**Ruby** shares Python's strengths with expressive syntax and automatic big integer handling.

**Clojure** elegantly expresses the suffix maximum using `reductions`:
```clojure
(vec (reverse (reductions max (reverse (map #(Character/digit % 10) line)))))
```

### Languages Requiring Care

**C** requires explicit handling of:
- 64-bit integers (`uint64_t`) for Part 2's large sums
- Manual memory allocation for suffix arrays
- Character-to-digit conversion with `- '0'`

**Go** similarly needs `uint64` for Part 2 and explicit byte-to-int conversion.

**Bash** faces significant challenges:
- No native data structures; arrays are clunky
- Arithmetic on large numbers requires careful handling
- String indexing via `${line:i:1}` is less intuitive
- Significantly slower due to subprocess overhead for each operation

### Performance Characteristics

| Language Family | Typical Performance | Notes |
|----------------|---------------------|-------|
| **Compiled (C, C++, Go, Zig)** | Fastest | Direct memory access, no GC pauses |
| **JVM (Java, Clojure)** | Fast after warmup | JIT compilation helps, startup cost |
| **Interpreted (Python, Ruby, PHP, Perl)** | Moderate | Convenience over speed |
| **Shell (Bash)** | Slowest | Each operation has subprocess overhead |
| **Assembly (ARM64)** | Fastest potential | Manual optimization required |

### Notable Differences

**Integer Types:**
- C/C++/Go need explicit 64-bit types for Part 2 sums
- Python/Ruby handle arbitrary precision automatically
- Java's `long` suffices for these values

**String Handling:**
- C treats strings as char arrays; `line[i] - '0'` for digit value
- Python/Ruby have cleaner `int(line[i])` or `line[i].to_i`
- Clojure uses `(Character/digit (nth line i) 10)`

---

## Answers

- **Part 1:** 17554
- **Part 2:** 175053592950232
