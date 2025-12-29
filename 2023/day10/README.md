# Day 10: Pipe Maze

## Problem Summary

You discover a field densely packed with pipes arranged in a 2D grid. An animal has scurried into one of the pipes, and you need to find it by understanding the pipe network.

**Input format:** A 2D grid of characters where:
- `|` connects north and south
- `-` connects east and west
- `L` connects north and east (90-degree bend)
- `J` connects north and west (90-degree bend)
- `7` connects south and west (90-degree bend)
- `F` connects south and east (90-degree bend)
- `.` is ground (no pipe)
- `S` is the starting position (pipe shape unknown)

The pipes form one large continuous loop, with `S` being part of that loop.

## Part 1: Farthest Point in Loop

**Goal:** Find the tile in the main loop that is farthest from the starting position, measured by steps along the loop.

### Algorithm
1. Find the starting position `S` in the grid
2. Determine which adjacent pipes connect back to `S` (these are the loop neighbors)
3. Use **Breadth-First Search (BFS)** starting from `S` to traverse the entire loop
4. Track distances from `S` to each tile in the loop
5. Return the maximum distance found

### Key Insight
BFS naturally finds the shortest path to every reachable node. Since the loop is continuous, BFS will spread in both directions around the loop, and the farthest point will be exactly at the midpoint of the loop (where both paths meet).

## Part 2: Enclosed Tiles

**Goal:** Count how many tiles are enclosed by the main loop (not part of the loop, but completely surrounded by it).

### Algorithm: Ray Casting (Point-in-Polygon)

This is a classic computational geometry problem. For each tile not on the loop:

1. Cast a ray from the tile to the edge of the grid (we scan left-to-right)
2. Count how many times the ray crosses the loop boundary
3. If the count is odd, the tile is inside; if even, it's outside

### Key Insight: The "North Connection" Rule

When counting crossings, we need to be careful about how we count pipe segments. Simply counting every loop tile would be incorrect because horizontal pipes (`-`) don't change inside/outside status.

**The rule:** Only count pipes that have a **north connection**: `|`, `L`, and `J`.

Why this works:
- `|` is a full vertical crossing
- `L` and `J` start going north (then turn), representing a crossing
- `-`, `7`, and `F` don't have north connections and are handled by their partners

This is equivalent to counting crossings at any consistent horizontal level (like the "top edge" of each cell).

### Important Step: Replace S

Before ray casting, we must determine what pipe shape `S` actually represents based on its connections in the loop. Otherwise, we might miscount crossings if `S` has a north connection.

## Data Structures Used

- **Hash Map/Set**: Store distances and loop membership for O(1) lookup
- **Queue (Deque)**: BFS traversal
- **2D Grid**: Parse input as array of strings or characters

## Complexity

- **Time:** O(n*m) where n and m are grid dimensions
  - BFS visits each loop tile once
  - Ray casting scans every cell once
- **Space:** O(n*m) for storing the loop positions and distances

## Programming Techniques Highlighted

1. **BFS (Breadth-First Search)**: Finding shortest paths in unweighted graphs
2. **Ray Casting**: Classic point-in-polygon algorithm from computational geometry
3. **State Machine**: The inside/outside toggle acts as a simple state machine
4. **Direction Vectors**: Using (dr, dc) pairs to represent movement directions

## Language Notes

### Performance Observations

- **ARM64 Assembly & C** lead the pack (~7ms) - minimal abstraction overhead, direct memory access
- **Rust** (~11ms) - close to C performance with safety guarantees
- **Go** (~12ms) - efficient compiled language with simple hash maps
- **C++** (~18ms) - slightly slower due to STL container overhead
- **Zig** (~21ms) - respectable performance, slightly slower than expected
- **Python** (~47ms) - surprisingly fast for an interpreted language; efficient built-in sets
- **Common Lisp** (~53ms) - SBCL's native compilation shows here
- **Bash** (~36 seconds!) - shell loops are extremely slow; string manipulation overhead is massive

### Implementation Challenges

- **String Key Generation**: Several languages need to convert coordinate pairs to hashable keys (`"${r},${c}"` pattern)
- **Set Operations**: Languages without native set types (C, Assembly) must implement their own or use arrays with O(n) lookup
- **BFS Queue**: Some languages (Bash, Assembly) don't have built-in queue data structures

### Language-Specific Approaches

- **Clojure/Lisp**: Functional approach with immutable data structures; slightly higher memory usage
- **ColdFusion**: JVM-based; startup time dominates small workloads
- **Bash**: Used associative arrays for sets; external tools like `bc` could help but loop overhead dominates

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 6.8          | 1.9         |
| C           | 6.9          | 1.9         |
| Rust        | 11.3         | 3.5         |
| Go          | 12.2         | 8.6         |
| C++         | 18.1         | 2.8         |
| Zig         | 20.6         | 3.1         |
| Python      | 47.1         | 19.7        |
| Lisp        | 53.3         | 51.0        |
| Perl        | 66.6         | 9.9         |
| Node.js     | 67.7         | 54.5        |
| PHP         | 79.0         | 30.6        |
| Java        | 86.6         | 77.1        |
| Ruby        | 149.4        | 32.4        |
| Clojure     | 540.4        | 199.1       |
| ColdFusion  | 2,837.2      | 1,130.0     |
| Bash        | 36,354.9     | 6.9         |

## Answers

- **Part 1:** 6956
- **Part 2:** 455
