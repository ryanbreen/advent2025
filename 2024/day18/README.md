# Day 18: RAM Run

## Problem Summary

You're inside a computer's memory space represented as a 71x71 grid (coordinates 0-70). Bytes are falling and corrupting memory locations one at a time. You need to navigate from the top-left corner (0,0) to the bottom-right corner (70,70), but corrupted cells are impassable.

### Part 1: Shortest Path After 1024 Bytes
After the first 1024 bytes have fallen and corrupted their locations, find the minimum number of steps needed to reach the exit using only up/down/left/right movement.

### Part 2: Find the Blocking Byte
Determine the coordinates of the first byte that, when it falls, completely blocks all paths from start to exit.

### Input Format
- List of X,Y coordinate pairs (one per line)
- Each pair represents where a byte will fall in sequence
- Example: `26,43` means a byte falls at position x=26, y=43

## Algorithmic Approach

### Part 1: BFS Shortest Path

**Algorithm**: Standard Breadth-First Search (BFS)

1. Parse all byte positions from input
2. Mark the first 1024 positions as corrupted on the grid
3. Run BFS from (0,0) to (70,70):
   - Use a queue initialized with the start position
   - Track visited cells to avoid revisiting
   - Explore all 4 cardinal directions
   - Return the step count when reaching the goal

**Complexity**:
- Time: O(V + E) = O(n²) where n = grid size (71)
- Space: O(n²) for the visited set and queue

### Part 2: Binary Search + BFS

**Key Insight**: If n bytes block the path, then n+1 bytes also block it. This monotonicity allows binary search.

**Algorithm**:
1. Binary search over the number of fallen bytes (0 to total_bytes)
2. At each midpoint:
   - Create corrupted set from bytes 0 to mid
   - Run BFS to check if a path exists
   - If blocked: search lower half
   - If path exists: search upper half
3. The final index is the first blocking byte

**Complexity**:
- Time: O(log(m) × n²) where m = number of bytes, n = grid size
- Space: O(n²) for each BFS call

**Why Binary Search?**
- A naive approach would check each byte sequentially: O(m × n²)
- Binary search reduces this to O(log(m) × n²)
- For ~3500 bytes, this is ~12 BFS calls vs 3500

## Programming Techniques Highlighted

### Data Structures
- **Hash Set**: O(1) lookup for corrupted cells and visited tracking
- **Queue**: FIFO structure for BFS traversal (deque in Python, LinkedList in Java)
- **2D Array**: Alternative to hash set for fixed-size grids (faster in compiled languages)

### Key Concepts
- **BFS**: Guaranteed shortest path in unweighted graphs
- **Binary Search**: Logarithmic search in monotonic sequences
- **Coordinate Hashing**: Converting (x,y) pairs to unique keys for set membership

## Language-Specific Notes

### Fast Performers (C, ARM64, Zig, Rust)
- Use fixed-size 2D arrays instead of hash sets
- Stack-allocated data structures avoid heap overhead
- Array-based queue implementation (no dynamic allocation)
- ARM64 achieves 8ms through direct memory manipulation

### Moderate Performers (Python, Node.js, Go)
- Python's `set` and `deque` provide clean, efficient BFS
- Go's maps work well but compiled binary adds some overhead
- Node.js Set is efficient but JS runtime adds latency

### Interpreted Languages (Ruby, PHP, Perl)
- Hash-based sets work but with higher constant factors
- Ruby's `Set` class needs explicit require
- PHP uses associative arrays with string keys

### JVM Languages (Java, Clojure)
- Java startup time dominates (JIT compilation)
- Clojure's persistent data structures add overhead but provide safety
- Both benefit from warm-up runs in production

### Shell/Scripting (Bash)
- Associative arrays (`declare -A`) for O(1) lookups
- Array-based queue simulation with index pointers
- Slowest due to subprocess overhead and lack of native data structures

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 8.2          | 1.9         |
| Zig         | 8.4          | 1.9         |
| C           | 9.5          | 1.9         |
| Rust        | 10.3         | 1.9         |
| C++         | 11.1         | 1.9         |
| Go          | 13.1         | 8.6         |
| Common Lisp | 35.2         | 46.1        |
| Python      | 38.8         | 15.8        |
| Perl        | 55.6         | 6.2         |
| Node.js     | 67.6         | 56.4        |
| PHP         | 73.6         | 25.8        |
| Java        | 121.5        | 61.9        |
| Ruby        | 129.7        | 29.6        |
| Clojure     | 559.1        | 164.6       |
| Bash        | 688.7        | 7.1         |
| ColdFusion  | 2,961.5      | 1,167.6     |

## Answers

- **Part 1**: 330
- **Part 2**: 10,38
