# Day 8: Playground

## Problem Summary

The Elves are setting up a Christmas decoration project with suspended junction boxes in 3D space. They need to connect junction boxes with strings of lights so electricity can reach every box.

**Input:** 1000 junction boxes with 3D coordinates (X, Y, Z)

### Part 1: Connect the 1000 Closest Pairs
Connect the 1000 pairs of junction boxes that are closest together (by Euclidean distance). After connecting, find the sizes of the three largest circuits (connected components) and multiply them together.

### Part 2: Connect Until One Circuit
Continue connecting the closest unconnected pairs until all junction boxes are in one single circuit. Return the product of the X coordinates of the last two junction boxes connected.

## Algorithmic Approach

### Key Insight
This is a classic **Union-Find (Disjoint Set Union)** problem combined with sorting all pairs by distance. The problem is essentially building a minimum spanning tree using Kruskal's algorithm, but stopping at specific points.

### Data Structures Used
- **Union-Find with Path Compression and Union by Rank**: Provides near-constant time O(α(n)) operations for finding and merging components
- **Array of Pairs with Distances**: All n*(n-1)/2 pairs (~500,000 for 1000 points) sorted by Euclidean distance

### Algorithm
1. Parse all 3D points from input
2. Generate all possible pairs (n choose 2 = ~500,000 pairs)
3. Calculate squared Euclidean distance for each pair (avoiding sqrt for efficiency)
4. Sort pairs by distance
5. **Part 1**: Connect the first 1000 pairs using Union-Find, return product of 3 largest component sizes
6. **Part 2**: Continue connecting until only 1 component remains, return product of X coordinates of the final connection

### Complexity
- **Time**: O(n² log n) - dominated by sorting ~500K pairs
- **Space**: O(n²) - storing all pair distances

## Programming Techniques Highlighted

- **Union-Find Data Structure**: Classic DSU with path compression and union by rank
- **Minimum Spanning Tree**: Similar to Kruskal's algorithm
- **3D Euclidean Distance**: Using squared distance to avoid expensive sqrt operations
- **Large Number Handling**: Part 2 result exceeds 32-bit integers

## Computer Science Topics

This problem is an excellent introduction to:
1. **Graph connectivity** - tracking connected components
2. **Greedy algorithms** - always connecting the closest available pair
3. **Disjoint Set Union (DSU)** - efficient data structure for dynamic connectivity
4. **Kruskal's MST algorithm** - the foundation of this approach
5. **Amortized analysis** - understanding why path compression gives O(α(n)) per operation

## Language Notes

### Fast Performers (Compiled, Systems Languages)
- **Rust** (40ms): Fastest overall - excellent iterator performance, safe memory management
- **C++** (57ms): STL sort is highly optimized, good cache locality
- **C** (86ms): Low memory footprint (9MB), straightforward implementation
- **ARM64/C** (89ms): Compiled C with ARM64 optimizations
- **Zig** (96ms): Modern systems language with explicit memory management

### Moderate Performance (VM/JIT Languages)
- **Go** (227ms): Includes compilation time, clean idiomatic code
- **Java** (231ms): JVM startup overhead, excellent OOP structure
- **Node.js** (541ms): V8 JIT optimization helps with repeated operations

### Scripting Languages
- **Clojure** (1103ms): Immutable data structures add overhead; tuple-return pattern for state threading
- **Python** (1173ms): Reference implementation, clean and readable
- **Bash** (1527ms): **Optimized from 46s to 1.5s (30x speedup)** using AWK for computation
- **Common Lisp** (1556ms): SBCL provides decent performance, good use of `loop` macro
- **PHP** (2118ms): Requires 512MB memory limit, modern OOP style
- **Ruby** (2577ms): Exemplary idiomatic code, dynamic typing overhead
- **Perl** (2833ms): Excellent Perl idioms (`//`, `x` operator), hash overhead

### Enterprise Runtime
- **ColdFusion** (9772ms): High resource usage (1.8GB memory), 1-based arrays handled correctly

## Idiomaticity Scores

All implementations passed algorithmic purity review (no hardcoded answers, correct Union-Find).

**All 16 languages now score 10/10 for idiomaticity!**

| Language | Idiomaticity | Notes |
|----------|-------------|-------|
| Node.js | 10/10 | Exemplary modern ES6+ code |
| Go | 10/10 | Textbook idiomatic Go |
| Zig | 10/10 | Proper allocator patterns |
| Ruby | 10/10 | Excellent Ruby idioms |
| Perl | 10/10 | Deep Perl knowledge shown |
| Python | 10/10 | Clean, no unused imports |
| Rust | 10/10 | Efficient root check in get_component_sizes |
| Java | 10/10 | Modern `var` keyword usage |
| C | 10/10 | Good memory management |
| Common Lisp | 10/10 | Idiomatic use of loop macro and structs |
| PHP | 10/10 | Uses $_SERVER['argv'] superglobal |
| C++ | 10/10 | Single Pair struct at file scope |
| ColdFusion | 10/10 | Proper CLI argument handling |
| Clojure | 10/10 | Idiomatic atoms for mutable Union-Find state |
| Bash | 10/10 | AWK for computation (Unix philosophy) |
| ARM64/C | 10/10 | Optimized compiled C |

## Bash Optimization Deep Dive

The original Bash solution took ~46 seconds. Key optimizations achieved 30x speedup:

1. **Eliminated recursive subshells** - Replaced Bash function calls with AWK loops
2. **Moved computation to AWK** - AWK is orders of magnitude faster for numerical work
3. **Single-pass processing** - Process sorted pairs once for both parts
4. **Iterative path compression** - Proper Union-Find in AWK
5. **Eliminated bc dependency** - AWK handles large integers natively

## Benchmark Results

| Language | Runtime (ms) | Memory (MB) | Notes |
|----------|-------------|-------------|-------|
| Rust | 39.55 | 33.42 | Fastest overall |
| C++ | 57.27 | 21.09 | Efficient STL |
| C | 86.48 | 9.08 | Low memory footprint |
| ARM64/C | 88.67 | 9.03 | Compiled C |
| Zig | 96.11 | 12.92 | Good balance |
| Go | 226.75 | 61.34 | Includes compile time |
| Java | 230.99 | 100.56 | JVM overhead |
| Node.js | 541.04 | 173.38 | V8 optimized |
| Clojure | 1103.47 | 364.78 | JVM + functional |
| Python | 1172.98 | 98.31 | Reference impl |
| Bash | 1526.76 | 78.47 | AWK-optimized |
| Common Lisp | 1556.30 | 99.09 | SBCL |
| PHP | 2118.14 | 169.45 | Needs memory boost |
| Ruby | 2577.36 | 87.59 | Dynamic typing |
| Perl | 2833.25 | 117.23 | Hash overhead |
| ColdFusion | 9772.06 | 1872.66 | High resource usage |

## Answers

- **Part 1**: 244188
- **Part 2**: 8361881885
