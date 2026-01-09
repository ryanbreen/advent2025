# Day 16: Proboscidea Volcanium

## Problem Summary

You're trapped in a volcano with elephants, and need to release maximum pressure from valves before eruption. Starting at valve AA, you can move through tunnels (1 minute each) and open valves (1 minute each). Open valves release pressure equal to their flow rate multiplied by remaining time.

**Part 1**: Find the maximum pressure release in 30 minutes with just yourself.

**Part 2**: With an elephant helper trained in 4 minutes (leaving 26 minutes), find maximum combined pressure where you and the elephant open disjoint sets of valves.

## Input Format

Lines describing valve flow rates and tunnel connections:
```
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
```

## Algorithmic Approach

### Graph Preprocessing

The raw graph has ~60 valves but only ~15 have positive flow rate. We precompute shortest distances between all relevant valves (those with flow > 0, plus AA) using BFS from each. This reduces the problem from ~60 nodes to ~16 nodes.

### Part 1: DFS with Memoization

With the compressed graph, use depth-first search with memoization:
- State: (current position, time remaining, set of opened valves)
- At each step, try moving to each unopened valuable valve and opening it
- Use bitmask to represent the set of opened valves (efficient for 15 valves)
- Memoize on state to avoid recomputation

### Part 2: Subset Enumeration

The key insight is that you and the elephant will open disjoint sets of valves. Instead of simulating both simultaneously:

1. Precompute `maxPressure[mask]` for every subset of valves (2^n subsets)
2. Each `maxPressure[mask]` = maximum pressure achievable by opening only valves in that subset in 26 minutes
3. Find the best partition: `max(maxPressure[mask] + maxPressure[complement])` over all masks

This is O(2^n × DFS) for precomputation + O(2^n) for partition search, where n ≈ 15.

### Key Insight

The exponential blowup in Part 2 comes from the subset enumeration (32,768 subsets). Each subset requires a full DFS traversal, making this one of the most computationally intensive AoC problems.

### Complexity

- **Part 1**: O(n! × memoization) where n = valuable valves (~15), heavily pruned
- **Part 2**: O(2^n × n! × memoization) - around 32K DFS calls
- **Space**: O(states) for memoization cache, can be large

## Programming Techniques Highlighted

- **Graph compression**: Reducing 60-node graph to 16 nodes via BFS preprocessing
- **Bitmask for set representation**: Efficient state encoding with 15-bit integers
- **DFS with memoization**: State-space search with caching
- **Subset enumeration**: Iterating over all 2^n subsets
- **Partition optimization**: Finding optimal disjoint subset pairs

## Language-Specific Notes

### Performance Characteristics

Day 16 is extremely computationally intensive due to the exponential nature of Part 2. This is one of the slowest problems in AoC 2022.

**Fast implementations (1.5-4s)**:
- **C**: Fastest at 1.57s - efficient hash maps and low overhead
- **ARM64**: 1.74s - tight assembly but similar algorithm
- **Zig/Rust**: 3.9-4.1s - systems languages with good hash map performance

**Moderate (8-25s)**:
- **Java**: 8.3s despite JVM overhead - benefits from HotSpot optimization
- **Node.js**: 12.2s - V8's JIT helps significantly
- **Go**: 19.2s - map operations are slower than expected
- **C++**: 23.8s - surprisingly slow, likely STL overhead

**Slow (25-60s)**:
- **PHP**: 25.5s
- **Common Lisp**: 37.3s
- **Python**: 47.7s - @lru_cache memoization helps but still slow
- **Clojure**: 60.5s - functional overhead

**Very slow (1.5+ minutes)**:
- **Bash**: 89s - associative arrays are slow
- **Perl**: 95s - hash operations at scale are expensive
- **Ruby**: 142s (2.4 minutes) - memoization overhead

### Memory Usage Notes

- Perl uses 2.8GB due to hash table overhead at scale
- Java/Clojure use 1.3GB (JVM heap)
- Most compiled languages stay under 100MB

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 1,570.9      | 63.5        |
| ARM64 asm   | 1,742.6      | 1.9         |
| Zig         | 3,990.3      | 3.2         |
| Rust        | 4,089.7      | 13.1        |
| Java        | 8,292.7      | 1,286.2     |
| Node.js     | 12,218.2     | 95.4        |
| Go          | 19,171.8     | 17.6        |
| C++         | 23,761.0     | 9.3         |
| PHP         | 25,505.1     | 35.8        |
| Common Lisp | 37,321.1     | 117.6       |
| Python      | 47,733.5     | 91.2        |
| Clojure     | 60,453.3     | 1,352.3     |
| Bash        | 89,063.4     | 7.7         |
| Perl        | 94,767.1     | 2,849.7     |
| Ruby        | 141,826.6    | 47.6        |
| ColdFusion  | TBD          | TBD         |

Note: This is one of the most computationally expensive days, with even fast languages taking over 1 second and interpreted languages taking 1-2+ minutes.

## Answers

- Part 1: 1580
- Part 2: 2213
