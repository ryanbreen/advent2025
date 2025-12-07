# Day 6: Guard Gallivant

## Problem Summary

In this puzzle, we are transported to the year 1518 at a North Pole prototype suit manufacturing lab. A guard is patrolling the lab following a strict protocol, and we need to predict her movements to help The Historians search safely.

### Input Format

The input is a 2D grid representing the lab floor:
- `.` represents empty space
- `#` represents obstacles (crates, desks, alchemical reactors, etc.)
- `^`, `v`, `<`, `>` represents the guard's starting position and initial facing direction (up, down, left, right respectively)

### What We're Computing

- **Part 1**: Count the number of distinct positions the guard visits before leaving the mapped area
- **Part 2**: Count how many positions could have a new obstruction placed to trap the guard in an infinite loop

## Part 1 Analysis

### Problem Statement

The guard follows a simple patrol protocol:
1. If there is an obstacle directly in front, turn right 90 degrees
2. Otherwise, take a step forward

Continue until the guard leaves the boundaries of the map. Count all distinct grid positions visited.

### Algorithm Overview

1. Parse the grid and locate the guard's starting position and direction
2. Simulate movement step by step:
   - Check if next position is out of bounds (termination condition)
   - Check if next position is an obstacle (turn right)
   - Otherwise move forward and record position as visited
3. Return the count of unique positions visited

### Key Data Structures

- **2D Grid**: Store the map layout (obstacles and empty spaces)
- **Set/Map**: Track visited positions to count unique locations
- **Direction State**: Track current position (x, y) and facing direction

## Part 2 Analysis

### Problem Statement

Find all positions where placing a single new obstruction would cause the guard to get stuck in an infinite loop. The obstruction cannot be placed at the guard's starting position.

### Additional Complexity

Part 2 transforms the problem from simple simulation to loop detection. We must:
1. Identify candidate positions for the new obstruction
2. For each candidate, simulate the guard's patrol with the obstruction in place
3. Detect whether the simulation enters an infinite loop

### Algorithm Modifications

**Loop Detection Strategy**: Track not just positions but complete states (position + direction). If we encounter the same state twice, we've found a loop.

**Optimization**: Only test positions that the guard actually visits in the unobstructed patrol (Part 1 path). Positions the guard never visits cannot affect her path.

## Algorithmic Approach

### Key Insight

The crucial realization for Part 2 is that loop detection requires tracking **state**, not just position. A state is the tuple `(x, y, direction)`. If the guard returns to the same position facing the same direction, she will repeat her path forever.

Additionally, we only need to test obstruction placements along the guard's original patrol path. Placing an obstacle somewhere the guard never goes cannot create a loop.

### Data Structures

| Structure | Purpose |
|-----------|---------|
| 2D Array/Grid | Store map layout |
| Set of (x, y) | Track visited positions (Part 1) |
| Set of (x, y, dir) | Track visited states for loop detection (Part 2) |
| Direction vectors | Map directions to movement deltas |

### Direction Handling

The "turn right" operation is elegantly handled:
- Directions cycle: Up -> Right -> Down -> Left -> Up
- Using direction indices 0-3: `new_dir = (dir + 1) % 4`
- Or using vector math: if direction is `(dx, dy)`, turning right gives `(-dy, dx)`

### Time Complexity

- **Part 1**: O(N * M) where N x M is the grid size
  - Each position is visited at most 4 times (once per direction)

- **Part 2**: O(P * N * M) where P is the number of positions on the patrol path
  - For each candidate position, we run a full simulation
  - With optimization (only testing visited positions): O(V * N * M) where V << N * M

### Space Complexity

- **Part 1**: O(V) for the visited set, where V is positions visited
- **Part 2**: O(N * M * 4) for the state tracking array/set

## Programming Techniques Highlighted

### Computer Science Concepts

1. **State Machine Simulation**: The guard's behavior is a deterministic state machine with transitions based on obstacles
2. **Cycle/Loop Detection**: Classic problem of detecting when a sequence enters a repeating pattern
3. **Brute Force with Pruning**: Testing all candidates but intelligently limiting the search space
4. **2D Grid Traversal**: Standard directional movement with boundary checking

### Mathematical Properties

- **Modular Arithmetic**: Direction cycling uses mod 4 arithmetic
- **Vector Rotation**: 90-degree clockwise rotation transforms (dx, dy) to (-dy, dx)
- **Finite State Space**: With bounded grid and 4 directions, maximum states is 4 * N * M, guaranteeing termination

## Language-Specific Implementation Notes

### Naturally Suited Languages

**C/C++**: The fixed-size grid with integer positions maps perfectly to static arrays. Manual memory management allows fine-tuned performance. C achieved 19.61ms runtime.

**Go**: Built-in maps make state tracking clean. Struct types for Position and State provide type safety. The explicit iteration style fits the simulation well.

**Python**: Set comprehensions and tuple keys make the algorithm very readable. The `set()` data structure handles visited tracking elegantly.

### Interesting Approaches

**ARM64 Assembly**: Achieved the fastest time (4.68ms) by using direct memory operations and avoiding function call overhead. Direction lookup tables are stored in registers.

**Rust**: Uses `HashSet<(usize, usize, Direction)>` for state tracking. The enum-based direction handling is idiomatic but the 255.18ms runtime suggests hash overhead.

**Common Lisp**: Functional approach with recursion for simulation. Uses `gethash` for state tracking with competitive 44.16ms performance.

### Performance Characteristics

| Language Family | Typical Performance | Notes |
|----------------|---------------------|-------|
| Systems (C, ARM64) | 5-20ms | Direct memory access, minimal overhead |
| Compiled GC (Go, Java) | 70-360ms | Hash map overhead varies significantly |
| Scripting (Python, Perl, Ruby) | 30-70ms | Hash implementations well-optimized |
| JVM Functional (Clojure) | ~440ms | Immutable data structure overhead |
| Shell (Bash) | ~3,600ms | External process overhead for each check |

### Notable Observations

- **ARM64 Assembly** achieved remarkable performance (4.68ms) through careful register usage and avoiding hash table overhead
- **C** was significantly faster than C++ (19.61ms vs 69.34ms), likely due to STL container overhead
- **Python** (31.14ms) outperformed Go (71.19ms), Rust (255.18ms), and Java (358.15ms) - demonstrating that algorithmic efficiency matters more than language speed for this problem
- **Zig** showed unusually high runtime (3,279ms), possibly due to suboptimal algorithm choice

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 4.68         | 1.88        |
| C           | 19.61        | 7.98        |
| Python      | 31.14        | 16.67       |
| Perl        | 32.41        | 41.19       |
| Common Lisp | 44.16        | 18.53       |
| Node.js     | 49.55        | 46.86       |
| PHP         | 61.44        | 26.56       |
| C++         | 69.34        | 45.45       |
| Ruby        | 70.04        | 30.09       |
| Go          | 71.19        | 27.31       |
| Rust        | 255.18       | 109.81      |
| Java        | 358.15       | 114.05      |
| Clojure     | 437.48       | 140.64      |
| Zig         | 3,279.09     | 231.64      |
| Bash        | 3,635.02     | 4.52        |
| ColdFusion  | 4.68         | 1.88        |

## Answers

- **Part 1**: 5177 distinct positions visited
- **Part 2**: 1686 possible obstruction positions that create loops

## Files

- `problem.md` - Full problem description
- `input.txt` - Puzzle input
- `*/solution.*` - Solutions in 16 languages
