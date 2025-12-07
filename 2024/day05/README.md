# Day 5: Print Queue

## Problem Summary

The North Pole printing department needs help with their sleigh launch safety manual updates. The printer requires pages to be printed in a specific order based on ordering rules.

### Input Format

The input consists of two sections separated by a blank line:

1. **Page Ordering Rules**: Lines in the format `X|Y`, meaning page X must be printed before page Y (if both are present in an update)
2. **Updates**: Comma-separated lists of page numbers representing print jobs

Example:
```
47|53
97|13
97|61
...

75,47,61,53,29
97,61,53,29,13
...
```

### What We're Computing

- **Part 1**: Sum of middle page numbers from updates that are already in correct order
- **Part 2**: Sum of middle page numbers from incorrectly-ordered updates after fixing their order

## Part 1 Analysis

### What Does Part 1 Ask?

Identify which updates already satisfy all applicable ordering rules, then sum their middle page numbers.

### Algorithm Overview

1. Parse the ordering rules into a data structure for efficient lookup
2. For each update, verify that no rule is violated
3. A rule `X|Y` is violated if both X and Y appear in the update AND Y appears before X
4. Sum the middle elements of all valid updates

### Key Data Structures

- **Rules Map**: `Map<int, Set<int>>` where `rules[X]` contains all pages that must come after X
- **Position Map**: For each update, create a map from page number to its index position for O(1) position lookups

## Part 2 Analysis

### How Does Part 2 Change the Problem?

Instead of identifying valid updates, we must:
1. Find the incorrectly-ordered updates
2. Reorder them to satisfy all rules
3. Sum the middle page numbers of the corrected orderings

### Additional Complexity

The key insight is that the ordering rules define a partial ordering over page numbers. To fix an invalid update, we need to sort the pages according to this partial ordering.

### Algorithm Modifications

Use a custom comparator for sorting:
- If rule `X|Y` exists, then X < Y in the sort order
- If rule `Y|X` exists, then Y < X in the sort order
- Otherwise, maintain relative order (stable sort behavior)

## Algorithmic Approach

### Key Insight

The ordering rules define a **partial order** (a transitive, antisymmetric, reflexive relation). The problem guarantees that for any subset of pages in an update, there exists a unique valid ordering. This means the rules form a **total order** over each update's pages - there are no cycles or ambiguities.

This critical property allows us to use standard comparison-based sorting with a custom comparator derived from the rules.

### Data Structures

| Structure | Purpose |
|-----------|---------|
| `Map<int, Set<int>>` | Stores rules: `rules[X]` = pages that must come after X |
| `Map<int, int>` | Position lookup: `pos[page]` = index in current update |
| Array/List | Store updates and maintain page order |

### Time Complexity

- **Parsing**: O(R + U*P) where R = number of rules, U = number of updates, P = average pages per update
- **Part 1 Validation**: O(U * P * A) where A = average number of "after" rules per page
- **Part 2 Sorting**: O(U * P * log(P) * lookup_time)
  - With hash-based rule lookup: O(U * P * log(P))
  - With linear rule search (C implementation): O(U * P * log(P) * R)

### Space Complexity

- O(R) for storing rules
- O(P) per update for position mapping
- Total: O(R + U*P) for all data

## Programming Techniques Highlighted

### Computer Science Concepts

1. **Partial Orders and Topological Sorting**: The rules define a directed acyclic graph (DAG) where each rule `X|Y` is an edge from X to Y. Fixing order is essentially finding a topological sort.

2. **Custom Comparators**: Both validation and reordering rely on defining a comparison function based on the rule set.

3. **Hash-Based Lookups**: Efficient implementations use hash maps/sets for O(1) rule and position lookups.

4. **Two-Pass Parsing**: Standard technique for inputs with multiple sections.

### Mathematical Properties

- **Transitivity**: If X must come before Y, and Y must come before Z, then X must come before Z. The problem's rules are designed to be transitively consistent.

- **Antisymmetry**: No cycles exist in the rules (you can't have both `X|Y` and `Y|X`).

## Language-Specific Implementation Notes

### Naturally Suited Languages

**Go** (4.9ms - fastest): Go's combination of efficient map implementations, slice handling, and the `sort.Slice` function with custom comparators makes it ideal. The language's simplicity results in clean, performant code.

**Zig** (7.2ms): Excellent systems-level performance with straightforward data structure implementations.

**C++** (7.9ms): The STL provides efficient `unordered_map`, `unordered_set`, and `sort` with custom comparators.

**Rust** (8.5ms): HashMap and HashSet from std::collections, with excellent sort performance.

### Languages Requiring Workarounds

**C** (16.0ms): No built-in hash maps requires either:
- Linear search through rules (slower but simpler - used here)
- Custom hash table implementation
- Using qsort_r for reentrant sorting with context

The C implementation uses bubble sort because standard `qsort` doesn't support passing context to the comparator.

**ARM64 Assembly** (22.6ms): Requires manual implementation of all data structures. The overhead shows compared to higher-level languages, though it's still reasonable.

**Bash** (78,992.4ms - slowest): Shell scripting is fundamentally unsuited for this problem:
- No native hash maps (associative arrays are slow)
- No built-in sorting with custom comparators
- Heavy process spawning overhead for operations

### Performance Characteristics by Language Family

| Family | Languages | Typical Performance |
|--------|-----------|---------------------|
| Systems | C, C++, Rust, Zig, Go | 5-20ms |
| Assembly | ARM64 | ~23ms |
| Scripting | Python, Perl, Ruby, PHP | 17-70ms |
| JVM | Java, Clojure | 65-422ms |
| Niche | Common Lisp | ~29ms |
| Legacy | ColdFusion | ~2600ms |
| Shell | Bash | ~79000ms |

### Notable Observations

1. **Go outperforms C**: This is unusual but explained by Go's superior hash map implementation vs. C's linear rule search.

2. **Perl performs well** (17.5ms): Perl's hash-based data structures and text processing heritage make it efficient for this problem.

3. **Common Lisp surprise** (28.5ms): Competitive with Python, showing that Lisp implementations have excellent hash table performance.

4. **Bash extreme slowness**: Nearly 80 seconds highlights why shell scripting should be avoided for algorithmic problems.

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Go          | 4.9          | 1.9         |
| Zig         | 7.2          | 1.9         |
| C++         | 7.9          | 1.9         |
| Rust        | 8.5          | 1.9         |
| C           | 16.0         | 1.9         |
| Perl        | 17.5         | 5.0         |
| ARM64 asm   | 22.6         | 1.9         |
| Lisp        | 28.5         | 42.6        |
| Python      | 32.1         | 15.8        |
| Node.js     | 49.8         | 44.7        |
| PHP         | 63.2         | 24.8        |
| Java        | 65.1         | 51.5        |
| Ruby        | 68.4         | 29.0        |
| Clojure     | 421.8        | 145.8       |
| ColdFusion  | 2,602.9      | 1,122.0     |
| Bash        | 78,992.4     | 3.8         |

### Performance Analysis

- **Fastest**: Go at 4.9ms demonstrates that garbage-collected languages can compete with systems languages when hash operations dominate
- **Most Memory Efficient**: Systems languages (C, C++, Rust, Zig, Go, ARM64) all use ~1.9MB
- **Largest Memory Footprint**: ColdFusion at 1,122MB - JVM overhead plus framework bloat
- **Best Scripting Language**: Perl at 17.5ms, benefiting from its hash-optimized design

## Answers

| Part | Answer |
|------|--------|
| Part 1 | 6034 |
| Part 2 | 6305 |
