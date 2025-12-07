# Day 2: Red-Nosed Reports

## Problem Summary

The engineers at the Red-Nosed Reindeer nuclear fusion/fission plant need help analyzing safety reports from their reactor. Each report is a line containing a sequence of numbers called "levels" separated by spaces.

### Input Format
- Multiple lines, one report per line
- Each report contains space-separated integers (typically 5-8 levels per report)
- Example: `7 6 4 2 1` represents a single report with 5 levels

### What We're Computing
- **Part 1**: Count how many reports are "safe" according to strict safety rules
- **Part 2**: Count how many reports are safe when the "Problem Dampener" allows tolerating one bad level

## Part 1 Analysis

### What Does Part 1 Ask?
A report is considered **safe** if and only if:
1. The levels are either **all increasing** or **all decreasing**
2. Any two adjacent levels differ by **at least 1** and **at most 3**

Both conditions must be met. A report like `8 6 4 4 1` fails because `4 4` has a difference of 0 (not between 1-3), and `1 3 2 4 5` fails because it changes direction (increases then decreases).

### Algorithm Overview
1. Parse each line into a list of integers
2. Compute differences between adjacent pairs
3. Check if all differences are in range [1,3] (increasing) OR all in range [-3,-1] (decreasing)
4. Count reports passing the check

### Key Data Structures
- **Array/List of integers**: Store levels for each report
- **Differences array**: Compute once and check properties

## Part 2 Analysis

### How Does Part 2 Change the Problem?
Part 2 introduces the **Problem Dampener**, which can tolerate a single bad level. A report is now considered safe if:
- It's already safe by Part 1 rules, OR
- Removing exactly one level would make it safe

### Additional Complexity
This adds a layer of brute-force exploration. For each report that fails the initial safety check, we must try removing each level one at a time and re-test.

### Algorithm Modifications
1. First check if report is safe using Part 1 logic
2. If not safe, iterate through each position:
   - Create a new list with that element removed
   - Check if the modified list is safe
   - If any removal makes it safe, count the report
3. Early exit once any valid removal is found

## Algorithmic Approach

### Key Insight
The safety check can be elegantly expressed as checking if all differences fall within a consistent range. Rather than tracking direction separately, check if ALL differences are in [1,3] (increasing) OR ALL are in [-3,-1] (decreasing). This combines the monotonicity and magnitude requirements into a single pass.

### Data Structures
- **Array/Vector**: Simple sequential storage for levels
- **Temporary array**: For Part 2, to store the modified sequence with one element removed

### Time Complexity
- **Part 1**: O(n * m) where n = number of reports, m = average levels per report
- **Part 2**: O(n * m^2) - for each unsafe report, we try removing each of m elements and re-check

### Space Complexity
- **Part 1**: O(m) for storing differences (or O(1) if checking in-place)
- **Part 2**: O(m) for the temporary modified array

## Programming Techniques Highlighted

### Computer Science Concepts
- **Monotonicity checking**: Verifying a sequence is strictly increasing or decreasing
- **Constraint satisfaction**: All differences must satisfy multiple conditions simultaneously
- **Brute-force with pruning**: Part 2 tries all single-element removals but exits early on success

### Mathematical Properties
- **Bounded differences**: The [1,3] constraint ensures gradual changes
- **Sign consistency**: All differences must have the same sign (all positive or all negative)
- **Short-circuit evaluation**: Once any removal works, no need to try others

## Language-Specific Implementation Notes

### Naturally Suited Languages

**Python** excels here with list comprehensions and slicing:
```python
diffs = [levels[i+1] - levels[i] for i in range(len(levels) - 1)]
all_increasing = all(1 <= d <= 3 for d in diffs)
modified = levels[:i] + levels[i+1:]  # Easy element removal
```

**Clojure** shines with its functional approach:
```clojure
(every? (fn [[a b]] (and (< a b) (<= (- b a) 3)))
        (partition 2 1 levels))
```
The `partition 2 1` creates sliding pairs elegantly.

**Go** provides clean, readable code with explicit loops and good performance from static typing.

### Languages Requiring Workarounds

**C** requires manual memory management for the temporary arrays in Part 2 and careful string parsing with `strtok`.

**ARM64 Assembly** requires implementing all list operations manually, but the straightforward nature of the algorithm maps well to register-based computation.

**Bash** faces challenges with array manipulation and arithmetic, requiring creative use of string operations.

### Notable Differences

- **Memory**: C and ARM64 use stack-allocated fixed-size arrays; dynamic languages allocate on the heap
- **Parsing**: Scripting languages have built-in `split()` functions; C uses `strtok`; Clojure uses regex splitting
- **Iteration**: Functional languages use `map`/`filter`; imperative languages use explicit loops

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 5.7          | 1.9         |
| C           | 5.8          | 1.9         |
| Go          | 8.1          | 4.7         |
| C++         | 8.6          | 1.9         |
| Rust        | 9.5          | 1.9         |
| Lisp        | 23.6         | 39.2        |
| Perl        | 29.7         | 6.6         |
| Python      | 35.1         | 15.6        |
| Zig         | 35.6         | 2.0         |
| Node.js     | 50.1         | 44.3        |
| Java        | 59.0         | 55.2        |
| PHP         | 69.0         | 24.6        |
| Ruby        | 71.7         | 28.4        |
| Clojure     | 432.0        | 150.0       |
| Bash        | 932.0        | 4.8         |
| ColdFusion  | 2,705.6      | 1,149.0     |

### Performance Observations

- **Fastest**: ARM64 assembly and C achieve ~6ms with minimal memory overhead
- **Systems languages**: C, C++, Go, Rust all complete in under 10ms
- **Scripting languages**: Python, Perl, Ruby range from 30-70ms
- **JVM startup overhead**: Java and Clojure show higher memory usage due to JVM initialization
- **Shell scripting**: Bash at ~1 second is ~160x slower than C, primarily due to process spawning for arithmetic

## Answers

- **Part 1**: 246
- **Part 2**: 318
