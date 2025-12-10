# Day 10: Factory

## Problem Summary

The Elves need help initializing factory machines. Each machine has indicator lights and buttons that interact with them. The problem presents two distinct challenges that require different mathematical approaches.

### Input Format
Each line represents a machine with:
- `[.##.]` - Indicator light pattern (`.` = off, `#` = on)
- `(0,1,2)` - Button schematics listing which lights/counters each button affects
- `{3,5,4,7}` - Joltage requirements (Part 2 only)

## Part 1: Toggle Lights (XOR Problem)

**Goal**: Find the minimum button presses to configure indicator lights to match the target pattern.

### Algorithm: Brute Force over GF(2)

This is a problem over the Galois Field GF(2) (binary field) where:
- Each button press XORs (toggles) a set of lights
- Pressing a button twice is equivalent to not pressing it
- Therefore, each button is pressed either 0 or 1 times

**Approach**: Since buttons are either pressed or not, we try all 2^n combinations where n is the number of buttons. For each combination, simulate the XOR operations and check if we reach the target state.

```
For each subset of buttons (bitmask from 0 to 2^n - 1):
    state = 0
    For each button in subset:
        state ^= button_mask
    If state == target:
        presses = popcount(bitmask)
        min_presses = min(min_presses, presses)
```

### Complexity
- Time: O(2^n * n) where n = number of buttons (typically ≤ 15)
- Space: O(n)

## Part 2: Joltage Counters (Integer Linear Programming)

**Goal**: Find the minimum total button presses to reach exact joltage values, where each button press increments (not toggles) affected counters.

### Algorithm: Gaussian Elimination + Null Space Search

This is an Integer Linear Programming (ILP) problem:
- Minimize: sum(x_i) for all button presses x_i
- Subject to: Ax = b (where A is the button-to-counter incidence matrix, b is target joltage)
- Constraint: x_i ≥ 0 and x_i ∈ Z (non-negative integers)

**Approach**:

1. **Build the system**: Create matrix A where A[i][j] = 1 if button j affects counter i
2. **Gaussian elimination with rationals**: Reduce to Row Echelon Form to find:
   - Particular solution (one valid solution)
   - Null space basis (free variable directions)
3. **Search over free variables**: The general solution is:
   ```
   x = x_particular + t₁·v₁ + t₂·v₂ + ... + t_k·v_k
   ```
   where t_i are integer parameters and v_i are null space vectors.
4. **Find optimal**: Search integer values of t_i that keep all x_j ≥ 0 and minimize sum(x).

### Key Insight: Rational Arithmetic Required

The null space vectors often have fractional components (e.g., 1/2, 3/2). Using floating-point arithmetic introduces precision errors that cause incorrect results. The solution must use exact rational arithmetic (fractions with arbitrary-precision numerator/denominator).

### Complexity
- Gaussian elimination: O(min(m,n) * m * n) where m = counters, n = buttons
- Null space search: O(bound^k) where k = number of free variables, bound = max joltage
- In practice: k ≤ 3 for most inputs, making search tractable

## Programming Techniques Highlighted

### Data Structures
- **Bitmasks**: Efficient representation of button toggle patterns (Part 1)
- **Rational numbers**: Exact arithmetic for Gaussian elimination (Part 2)
- **2D arrays/matrices**: Augmented matrix for linear algebra

### Algorithms
- **Bit manipulation**: XOR for toggle simulation, popcount for counting presses
- **Gaussian elimination**: Transform to Reduced Row Echelon Form (RREF)
- **Null space computation**: Extract free variables and their corresponding vectors
- **Constraint propagation**: Compute valid bounds for free variables during search

### Mathematical Concepts
- **Galois Field GF(2)**: Binary field where 1+1=0 (XOR arithmetic)
- **Linear algebra over rationals**: Systems of linear equations with exact solutions
- **Integer Linear Programming**: Optimization with integer constraints
- **Null space / kernel**: The set of all solutions to Ax = 0

## Language-Specific Notes

### Fast Performers
- **C/C++/Rust**: Low overhead, efficient bit operations
- **Go/Java**: Good performance with built-in arbitrary precision (big.Rat, BigInteger)

### Rational Arithmetic
Different languages handle exact fractions differently:

| Language | Rational Type |
|----------|--------------|
| Python | `fractions.Fraction` |
| Ruby | `Rational` (built-in) |
| Clojure | Ratio (built-in) |
| Common Lisp | Ratio (built-in) |
| Java | Custom class with `BigInteger` |
| Go | `math/big.Rat` |
| PHP | GMP functions |
| Perl | `Math::BigRat` |
| JavaScript | Custom `Fraction` class with `BigInt` |

### Challenging Languages
- **Bash**: Requires `bc` for arithmetic; very slow for Part 2's search
- **ARM64 Assembly**: Part 2's rational arithmetic is extremely complex to implement

## Answers

- **Part 1**: 558
- **Part 2**: 20317

## Benchmarks

All benchmarks run on Apple Silicon (ARM64), measuring both parts combined.

| Language | Runtime (ms) | Memory (MB) | Notes |
|----------|-------------|-------------|-------|
| C | 38.19 | 1.89 | Fastest overall |
| C++ | 44.77 | 2.33 | |
| Zig | 51.50 | 5.19 | |
| Java | 69.15 | 47.86 | JVM startup overhead |
| Rust | 71.43 | 2.12 | |
| Common Lisp | 267.90 | 88.98 | Built-in rationals |
| Node.js | 593.11 | 118.50 | Custom BigInt fractions |
| Clojure | 1,690.80 | 1,366.59 | JVM + built-in ratios |
| Ruby | 1,698.00 | 28.55 | Built-in Rational |
| Go | 2,276.81 | 10.83 | math/big.Rat |
| Python | 4,808.24 | 17.67 | fractions.Fraction |
| PHP | 6,493.87 | 25.17 | GMP functions |
| Perl | 9,525.33 | 6.97 | Math::BigRat |
| CFML | 10,485.87 | 1,034.81 | JVM-based |
| ARM64 | 122.33 | 1.89 | FP bounds + rational validation |
| Bash | 15,418.12 | 16.05 | bc for arithmetic |

### Performance Analysis

**Fast tier (< 100ms)**: C, C++, Zig, Java, Rust
- Systems languages with efficient rational implementations
- Java benefits from JIT compilation despite JVM startup

**Medium tier (100ms - 2s)**: ARM64, Common Lisp, Node.js, Clojure, Ruby
- ARM64 uses floating-point for bound computation, rationals only for validation (209x speedup from naive brute-force)
- Languages with built-in or efficient rational number support
- Clojure's high memory due to JVM and immutable data structures

**Slow tier (> 2s)**: Go, Python, PHP, Perl, CFML, Bash
- Go's big.Rat has allocation overhead
- Scripting languages pay interpretation costs
- Bash uses external `bc` process for each arithmetic operation
