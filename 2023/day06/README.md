# Day 6: Wait For It

## Problem Summary

You're competing in a toy boat race where holding a button charges the boat. The longer you hold, the faster the boat goes when released, but holding time counts against total race time.

**Part 1**: Given multiple races with time limits and record distances, find how many ways to win each race and multiply them together.

**Part 2**: Treat all the time/distance numbers as a single concatenated number (ignore spaces between digits) and find how many ways to win this single long race.

### Input Format
```
Time:      7  15   30
Distance:  9  40  200
```

## Algorithmic Approach

### Key Insight

If you hold the button for `t` milliseconds in a race of `T` milliseconds total, you travel:
- Distance = `t * (T - t)` millimeters

To beat record `D`, we need: `t * (T - t) > D`

This is a quadratic inequality: `-t² + Tt - D > 0`

Using the quadratic formula, the roots are:
```
t = (T ± sqrt(T² - 4D)) / 2
```

The parabola opens downward, so values between the roots beat the record. We need to count integers **strictly between** the roots.

### Part 1 Algorithm
1. Parse times and distances as separate races
2. For each race, compute discriminant `T² - 4D`
3. Find roots using quadratic formula
4. Count integers strictly between roots: `floor(t_low) + 1` to `ceil(t_high) - 1`
5. Multiply all counts together

### Part 2 Algorithm
1. Concatenate all times into one number (e.g., 7, 15, 30 → 71530)
2. Concatenate all distances similarly
3. Apply same quadratic formula approach to this single race

### Complexity
- **Time**: O(n) where n is number of races (Part 1), O(1) for Part 2
- **Space**: O(1) - only need to store parsed numbers

The quadratic formula gives us the answer in constant time per race - no need to simulate each possible hold time.

## Programming Techniques Highlighted

- **Mathematical optimization**: Transforming a brute-force simulation (O(T) per race) into O(1) using algebra
- **Floating-point precision**: Using floor/ceil carefully to handle boundary conditions
- **String concatenation for parsing**: Part 2 requires treating numbers as digit strings

## Language-Specific Notes

### Fast Performers
- **Zig, C++, ARM64, C, Rust** (~5-6ms): Minimal overhead, direct floating-point operations
- Native math operations (sqrt, floor, ceil) are highly optimized on modern CPUs

### Scripting Languages
- **Perl** (13.8ms): Surprisingly fast due to efficient regex parsing and math
- **Python** (24.7ms): math.sqrt and floor/ceil are C-backed, good performance
- **Bash** (126.3ms): Requires `bc` for floating-point math, adding overhead

### JVM Languages
- **Clojure** (395.4ms): JVM startup dominates for this simple problem
- **Java** (56.8ms): Also affected by JVM warmup

### Notable Implementation Details
- **ARM64 Assembly**: The trickiest part was parsing - the `skip_ws` function was incorrectly skipping newlines, causing the distance parsing to fail. Fixed by creating a `skip_spaces` function that only skips space characters.
- **ColdFusion**: Requires `writeOutput` instead of `print.line` when running via CommandBox

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 5.1          | 1.9         |
| C++         | 5.6          | 1.9         |
| ARM64 asm   | 5.9          | 1.9         |
| C           | 6.2          | 1.8         |
| Rust        | 6.2          | 1.9         |
| Go          | 11.2         | 4.0         |
| Perl        | 13.8         | 6.2         |
| Lisp        | 22.9         | 39.4        |
| Python      | 24.7         | 15.6        |
| Node.js     | 42.1         | 37.5        |
| Java        | 56.8         | 45.8        |
| Ruby        | 58.6         | 27.7        |
| PHP         | 63.2         | 25.6        |
| Bash        | 126.3        | 6.7         |
| Clojure     | 395.4        | 129.1       |
| ColdFusion  | 2,440.5      | 1,174.8     |

## Answers

- Part 1: **303600**
- Part 2: **23654842**
