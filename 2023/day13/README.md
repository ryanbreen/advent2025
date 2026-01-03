# Day 13: Point of Incidence

## Problem Summary

You find yourself in a valley full of mirrors on Lava Island. The terrain is covered with patterns of ash (`.`) and rocks (`#`), and you need to find lines of reflection in each pattern to navigate safely.

**Input Format:** Multiple patterns separated by blank lines. Each pattern is a grid of `.` (ash) and `#` (rocks).

## Part 1: Find Reflection Lines

For each pattern, find a perfect reflection across either:
- A **vertical line** between two columns, OR
- A **horizontal line** between two rows

The reflection must be perfect for all rows/columns that have a mirrored counterpart (edge rows/columns without a pair are ignored).

**Scoring:**
- Vertical reflection: count of columns to the left of the line
- Horizontal reflection: 100 x count of rows above the line

Sum all pattern scores for the answer.

## Part 2: Fix the Smudge

Each mirror has exactly one smudge - a single `.` that should be `#` or vice versa. After fixing the smudge, a **different** line of reflection becomes valid.

Find the new reflection line (ignoring the original) and calculate the score the same way.

## Algorithmic Approach

### Core Algorithm: Difference Counting

The key insight is that instead of searching for a smudge location, we can count character differences across potential reflection lines.

**Part 1:** Find a reflection line with **exactly 0 differences** between mirrored halves.

**Part 2:** Find a reflection line with **exactly 1 difference** between mirrored halves (the smudge).

### Reflection Detection

For each potential reflection point:

1. **Vertical reflection at column `c`:**
   - For each row, reverse the left side `row[:c]` and compare with the right side `row[c:]`
   - Compare only the overlapping portion (min of left and right lengths)
   - Count total character differences across all rows

2. **Horizontal reflection at row `r`:**
   - Reverse the top portion `pattern[:r]` and compare with bottom `pattern[r:]`
   - Compare only the overlapping rows
   - For each row pair, count character differences

### Key Insight

The "smudge" approach elegantly unifies both parts:
- Part 1: Look for reflections with 0 total differences
- Part 2: Look for reflections with exactly 1 total difference

This avoids the need to enumerate all possible smudge positions and recheck.

### Complexity

- **Time:** O(P x W x H) where P = number of patterns, W = width, H = height
  - For each pattern, we check O(W) vertical lines and O(H) horizontal lines
  - Each check involves comparing O(W x H) characters
- **Space:** O(1) extra space beyond the input

This is highly efficient since there's no DP, recursion, or memoization needed.

## Programming Techniques Highlighted

1. **String Reversal and Comparison**: Core operation for mirror detection
2. **Two-pointer / Sliding Window**: Comparing mirrored regions
3. **Early Termination**: Stop checking once differences exceed threshold (1 for Part 2)
4. **Pattern Separation**: Parsing multiple blocks from input

## Data Structures Used

- **2D Grid / List of Strings**: The pattern representation
- **Integer counters**: For difference counting
- No complex data structures needed - this is a straightforward string comparison problem

## Language-Specific Notes

### Fast Performers
- **C (5.3ms)**: Direct character array manipulation, minimal overhead
- **C++ (5.8ms)**: Efficient string handling with standard library
- **Zig (5.9ms)**: Low-level control with safety checks
- **ARM64 (6.0ms)**: Hand-optimized assembly for string operations
- **Rust (6.2ms)**: Zero-cost abstractions for string slicing

### Observations
- **Go (9.1ms)**: Slightly slower due to garbage collection overhead
- **Perl (21.9ms)**: Strong string handling makes this problem relatively fast
- **Python (30.2ms)**: List slicing and string comparison are well-optimized
- **Node.js (47.7ms)**: V8's string operations are reasonably fast

### This Problem Favors
- Languages with efficient string slicing and comparison
- No need for complex data structures (hash maps, trees)
- Simple iteration patterns mean less advantage for compiled languages

### Implementation Patterns

| Approach | Languages |
|----------|-----------|
| Array of strings with slicing | Python, Ruby, Node.js, PHP |
| Character array with pointer arithmetic | C, C++, Rust, Zig |
| List of strings with reversals | Common Lisp, Clojure |
| String operations with regex support | Perl, Bash |

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.3          | 1.9         |
| C++         | 5.8          | 1.9         |
| Zig         | 5.9          | 1.9         |
| ARM64 asm   | 6.0          | 1.9         |
| Rust        | 6.2          | 1.9         |
| Go          | 9.1          | 4.8         |
| Perl        | 21.9         | 6.2         |
| Lisp        | 30.1         | 40.1        |
| Python      | 30.2         | 16.0        |
| Node.js     | 47.7         | 44.1        |
| PHP         | 63.7         | 26.0        |
| Java        | 69.3         | 53.0        |
| Ruby        | 76.3         | 28.7        |
| Clojure     | 447.9        | 144.9       |
| Bash        | 1,824.7      | 7.0         |
| ColdFusion  | 2,680.5      | 1,156.9     |

## Answers

- **Part 1:** 33520
- **Part 2:** 34824
