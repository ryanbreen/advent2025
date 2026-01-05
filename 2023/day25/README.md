# Day 25: Snowverload

## Problem Summary

This is the classic **minimum cut** problem in graph theory. Given a connected graph of components and their connections (wires), find exactly 3 edges (wires) to cut that will divide the graph into two separate components. Return the product of the sizes of the two resulting components.

**Part 2** is the traditional Advent of Code finale - once you have 49 stars, you simply push the big red button to complete the year.

**Input format**: Lines of `node: neighbor1 neighbor2 ...`

## Part 1: Minimum 3-Cut

Find three edges whose removal splits the graph into exactly two connected components.

### Algorithm: Edge Betweenness Centrality

The key insight is that the 3 cut edges must lie on **many shortest paths** between the two eventual components. Edges with high "betweenness centrality" (many shortest paths passing through them) are prime candidates.

1. **Sample shortest paths**: For each node (or a random sample), run BFS to all other nodes
2. **Count edge usage**: Track how many shortest paths use each edge
3. **Identify cut candidates**: Edges with highest betweenness are likely cut edges
4. **Verify**: Test combinations of high-betweenness edges until finding 3 that split the graph

### Alternative: Karger's Algorithm

A randomized approach that repeatedly contracts random edges until only 2 nodes remain:
- Each contraction merges two nodes, preserving multi-edges
- When only 2 "super-nodes" remain, the multi-edges between them are the cut
- Repeat many times, tracking the minimum cut found

### Complexity
- **Edge Betweenness**: O(V × E) for BFS from each vertex, plus O(E² × V) for verification
- **Karger's**: O(V² × E) per iteration, O(V² log V) iterations needed for high probability

## Part 2: Push the Button

No computation required - this is the traditional AoC finale that unlocks when you have all 49 other stars.

## Programming Techniques Highlighted

1. **Graph Algorithms**: BFS, connected components, edge contraction
2. **Betweenness Centrality**: Finding critical edges in a network
3. **Randomized Algorithms**: Karger's min-cut approach
4. **Network Flow**: Equivalent to max-flow min-cut theorem

## Language-Specific Notes

- **ARM64**: Exceptionally fast implementation using optimized graph traversal (8ms)
- **Java**: Benefits from JIT optimization on repeated graph operations (59ms)
- **Zig/Rust**: Fast compiled performance (~140ms)
- **Node.js**: Efficient JavaScript V8 engine for graph operations (177ms)
- **Python**: Uses edge betweenness heuristic for reasonable performance (271ms)
- **ColdFusion/Bash**: Slow due to language overhead for graph algorithms

## Benchmarks

| Language | Runtime (ms) | Memory (MB) |
|----------|--------------|-------------|
| ARM64 | 8.2 | 1.9 |
| Java | 58.7 | 47.9 |
| Zig | 140.3 | 3.2 |
| Rust | 144.7 | 4.9 |
| Node.js | 177.2 | 70.7 |
| Go | 181.5 | 27.8 |
| PHP | 259.3 | 27.6 |
| Python | 270.6 | 17.5 |
| C | 299.0 | 29.2 |
| Common Lisp | 305.8 | 90.7 |
| Perl | 424.7 | 8.6 |
| C++ | 489.3 | 3.6 |
| Ruby | 600.7 | 31.0 |
| Clojure | 895.7 | 801.8 |
| ColdFusion | 4003.1 | 1063.7 |
| Bash | >300000 | ~10 |

## Answers

- Part 1: **550080**
- Part 2: **Push the big red button!** (no computation - just have 49 stars)
