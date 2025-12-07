# Advent of Code 2025 - Day 4: Printing Department

## Problem Summary

In this puzzle, you're in the North Pole's printing department helping optimize forklift operations so they can break through a wall to the cafeteria. The department has large rolls of paper (`@`) arranged on a 2D grid.

### Input Format
- A 2D grid of characters
- `@` represents a roll of paper
- `.` represents an empty space

### What We're Computing
- **Part 1**: Count how many rolls of paper are initially accessible by forklifts
- **Part 2**: Count the total number of rolls that can be removed through iterative removal

The key constraint: A forklift can only access a roll of paper if it has **fewer than 4** neighboring rolls in the 8 adjacent positions (including diagonals).

## Part 1 Analysis

### Problem Statement
Find all rolls of paper that can be accessed by a forklift. A roll is accessible if it has fewer than 4 adjacent rolls in its 8 neighboring cells.

### Algorithm Overview
1. Parse the input into a 2D grid
2. For each cell containing a roll (`@`):
   - Count the number of adjacent rolls in all 8 directions
   - If count < 4, the roll is accessible
3. Return the total count of accessible rolls

### Key Data Structures
- **2D character array/grid**: Store the puzzle input
- **Direction offsets**: Array of 8 (dr, dc) pairs for neighbor traversal:
  ```
  (-1,-1) (-1,0) (-1,1)
  ( 0,-1)   X    ( 0,1)
  ( 1,-1) ( 1,0) ( 1,1)
  ```

## Part 2 Analysis

### How Part 2 Changes the Problem
Part 2 introduces a **cascading removal** mechanic. Once an accessible roll is removed, it may cause previously inaccessible rolls to become accessible (since they now have fewer neighbors). This process repeats until no more rolls can be removed.

### Additional Complexity
The naive approach of repeatedly scanning the entire grid until no changes occur would be O(n^2 * m) where n is the number of rolls and m is the number of removal iterations. This can be optimized significantly with proper data structures.

### Algorithm Modifications
Instead of repeated full scans, we use a **queue-based propagation** approach:
1. Precompute neighbor counts for all rolls
2. Initialize a queue with all initially accessible rolls
3. Process the queue:
   - Remove a roll from the queue
   - Mark it as removed and increment the counter
   - Decrement neighbor counts for all adjacent rolls
   - If any adjacent roll's count drops below 4, add it to the queue
4. Continue until the queue is empty

## Algorithmic Approach

### Key Insight
The crucial realization is that this is a **propagation problem** similar to flood fill or BFS. When a roll is removed, we only need to check its immediate neighbors for newly accessible candidates - not the entire grid. This transforms the problem from repeated O(grid_size) scans into a single pass with O(neighbors_updated) work per removal.

The condition `neighbors < 4` acts as a threshold that creates a "wave" of removals starting from the edges and sparse regions, propagating inward until blocked by dense clusters.

### Data Structures

| Structure | Purpose |
|-----------|---------|
| 2D Grid | Store current state of rolls |
| Neighbor Count Matrix | Track how many adjacent rolls each cell has |
| Queue | BFS-style processing of accessible rolls |
| Direction Array | 8-directional neighbor offsets |

### Time Complexity
- **Part 1**: O(R * C) where R = rows and C = columns - single pass through grid
- **Part 2**: O(R * C + N * 8) where N = number of removable rolls - each roll is processed at most once, with constant work per neighbor update

### Space Complexity
- **Part 1**: O(R * C) for storing the grid
- **Part 2**: O(R * C) for the neighbor count matrix + O(N) for the queue

## Programming Techniques Highlighted

### CS Concepts Tested
1. **Grid/Matrix Traversal**: Working with 2D arrays and 8-directional neighbors
2. **BFS-style Propagation**: Using a queue to process elements that become "ready"
3. **Incremental Updates**: Maintaining counts efficiently rather than recomputing
4. **Simulation with State Changes**: Tracking grid mutations over time

### Mathematical Properties Exploited
- **Locality**: A roll's accessibility depends only on its immediate neighbors
- **Monotonicity**: Once a roll is removed, it stays removed; neighbor counts only decrease
- **Threshold Behavior**: The `< 4` condition creates binary accessibility states

### Related Problems
This problem is structurally similar to:
- Conway's Game of Life (neighbor-based rules)
- Topological sort (removing elements when dependencies are satisfied)
- Flood fill algorithms (propagating from accessible regions)

## Language-Specific Implementation Notes

### Languages Naturally Suited

**Python** excels at this problem due to:
- Clean 2D list comprehensions for grid parsing
- `collections.deque` for efficient queue operations
- Dictionary-based position tracking with tuple keys
- Concise neighbor iteration with list unpacking

**Rust** benefits from:
- Strong typing prevents index errors
- `Vec<Vec<char>>` provides clean 2D grid semantics
- Pattern matching on direction tuples
- Zero-cost abstractions for the queue processing loop

**Go** works well because:
- Slices provide natural 2D array handling
- Simple, explicit loops match the algorithm structure
- No overhead from complex abstractions

### Languages Requiring Workarounds

**C** requires:
- Manual memory management considerations
- Fixed-size arrays (`MAX_SIZE`) instead of dynamic allocation
- Character-by-character string handling
- Manual queue implementation using arrays with start/end indices

**Java** has some verbosity:
- No tuple types, requiring `int[]` pairs for coordinates
- ArrayList-based queue (less efficient than ArrayDeque for this use case)
- Explicit null checks and bounds validation

**ARM64 Assembly** is the most challenging:
- Manual stack management for the queue
- Character-by-character grid parsing
- Register allocation for nested loops
- No built-in data structure support

### Performance Characteristics

| Language Family | Typical Runtime | Notes |
|----------------|-----------------|-------|
| Compiled (C, C++, Rust, Zig) | < 10ms | Direct memory access, minimal overhead |
| JVM (Java, Clojure) | 50-200ms | JIT warmup, but competitive once hot |
| Scripting (Python, Ruby, Perl) | 100-500ms | Interpreter overhead, but clean code |
| Go | 10-30ms | Fast compilation, efficient runtime |
| Shell (Bash) | 1-5s | Line-by-line processing, subprocess overhead |

### Notable Implementation Differences

1. **Queue Implementation**:
   - Python uses `deque` with `popleft()`
   - C uses array indices with manual head/tail tracking
   - Java uses `ArrayList` with index tracking (avoiding actual removal for efficiency)

2. **Grid Mutation**:
   - Some implementations modify the original grid in-place
   - Others work with a copy to preserve Part 1's grid
   - Functional languages may use immutable transformations

3. **Neighbor Counting**:
   - Most use explicit loops over 8 directions
   - Some precompute the full neighbor list during parsing
   - The C implementation checks `== 3` when decrementing (optimization: only add to queue at the exact threshold crossing)

## Answers

From successful implementations:

- **Part 1**: 1438
- **Part 2**: 11194
