# Day 11: Reactor

## Problem Summary

You're in a factory's reactor room where a server rack is having communication issues with the reactor. The system consists of devices connected by cables, forming a directed acyclic graph (DAG). Data flows only forward through outputs from device to device.

**Input format:** Each line describes a device and its connections:
```
device_name: neighbor1 neighbor2 neighbor3 ...
```

## Part 1: Count All Paths

Find the total number of distinct paths from the device labeled `you` to the output labeled `out`.

### Algorithm

This is a classic DAG path counting problem solved with **memoization**:

```
count_paths(node):
    if node == "out": return 1
    if node not in graph: return 0
    return sum(count_paths(neighbor) for neighbor in graph[node])
```

The key insight is that each node's path count can be computed once and cached. Without memoization, this would be exponential; with it, we visit each node exactly once.

**Time Complexity:** O(V + E) where V = nodes, E = edges
**Space Complexity:** O(V) for the memoization cache

## Part 2: Constrained Path Counting

Count paths from `svr` (server rack) to `out` that visit **both** `dac` (digital-to-analog converter) **and** `fft` (fast Fourier transform device), in any order.

### Algorithm

The insight is that in a DAG, a path visiting both nodes must visit them in exactly one order. We decompose the problem into two mutually exclusive cases:

1. **dac before fft:** `svr → dac → fft → out`
2. **fft before dac:** `svr → fft → dac → out`

For each case, we multiply the path counts of each segment:

```
total = paths(svr→dac) × paths(dac→fft) × paths(fft→out)
      + paths(svr→fft) × paths(fft→dac) × paths(dac→out)
```

This works because:
- Every combination of sub-paths yields a unique full path
- The segments are independent (DAG structure)
- The two cases are mutually exclusive

**Time Complexity:** O(V + E) - we compute 3 different path-counting functions
**Space Complexity:** O(V) for each memoization cache

## Key Insights

1. **DAG Structure:** The problem explicitly states data flows only forward, guaranteeing no cycles. This makes memoization safe and efficient.

2. **Multiplicative Decomposition:** When counting paths through mandatory waypoints, we can decompose into segments and multiply. This avoids expensive enumeration.

3. **Large Numbers:** Part 2 produces ~319 trillion paths, requiring 64-bit or arbitrary precision integers.

## Programming Techniques Highlighted

- **Memoization / Dynamic Programming:** Core technique for avoiding exponential blowup
- **Graph Representation:** Adjacency list using hash maps
- **Recursion with Caching:** Natural fit for path counting
- **Big Integer Arithmetic:** Part 2 exceeds 32-bit limits

## Language Notes

### Fast Performers
- **C, C++, Rust, Zig:** Low overhead, efficient hash maps
- **Go, Java:** Good JIT optimization

### Big Integer Handling
The Part 2 answer (319,473,830,844,560) fits in 64-bit integers, but multiplication of intermediate values may overflow 64-bit if not careful.

- **Python, Ruby, Clojure, Common Lisp:** Native arbitrary precision
- **JavaScript:** BigInt for safety
- **PHP:** bcmul() for precision
- **Perl:** Math::BigInt
- **Bash:** bc for arbitrary precision
- **Java:** long suffices (or BigInteger for safety)

### Special Considerations
- **ARM64 Assembly:** Requires implementing hash table from scratch; string handling is tedious
- **Bash:** Associative arrays work, but bc needed for multiplication

## Benchmark Results

| Language | Runtime (ms) | Memory (MB) |
|----------|-------------|-------------|
| Zig      | 6.36        | 1.78        |
| C        | 7.31        | 1.58        |
| Go       | 7.71        | 4.42        |
| C++      | 8.42        | 1.72        |
| Rust     | 13.32       | 2.08        |
| ARM64    | 27.49       | 15.19       |
| Python   | 27.33       | 15.16       |
| Common Lisp | 30.30    | 42.30       |
| Perl     | 51.32       | 13.44       |
| Java     | 54.12       | 51.27       |
| Node.js  | 57.23       | 46.91       |
| PHP      | 64.16       | 24.75       |
| Bash     | 115.79      | 7.25        |
| Ruby     | 124.39      | 28.19       |
| Clojure  | 413.22      | 131.09      |
| ColdFusion | 2700.00   | 1140.00     |

## Answers

- **Part 1:** 590
- **Part 2:** 319473830844560
