# Day 21: Step Counter

## Problem Summary

The Elf needs to track his pedometer steps by determining how many garden plots he can reach in exactly a given number of steps. Starting from position `S`, he can move north, south, east, or west onto garden plots (`.`) but not rocks (`#`).

**Part 1**: Count garden plots reachable in exactly 64 steps on a bounded grid.

**Part 2**: The grid tiles infinitely in all directions. Count garden plots reachable in exactly 26,501,365 steps.

## Algorithmic Approach

### Part 1: BFS with Parity

The key insight is that reaching a cell in exactly N steps doesn't require tracking all paths—only the **minimum distance** to each cell matters:

- A cell reachable in `d` steps can also be reached in `d+2`, `d+4`, etc. steps (by backtracking)
- Therefore, a cell is reachable in exactly `N` steps if `d ≤ N` and `d % 2 == N % 2`

**Algorithm:**
1. BFS from start to compute minimum distance to all reachable cells
2. Count cells where `distance ≤ 64` and `distance % 2 == 64 % 2`

### Part 2: Quadratic Extrapolation

The massive step count (26,501,365) makes direct BFS impossible. However, the problem has special structure:

**Key observations:**
- Grid is 131×131 with `S` at center (65, 65)
- `26501365 = 65 + 202300 × 131` — steps form a perfect pattern
- Clear paths exist along edges and through center (no rocks blocking expansion)
- The reachable area forms a diamond shape that grows quadratically

**The quadratic pattern:**
For steps of the form `65 + n × 131`, the count follows `f(n) = an² + bn + c`

**Algorithm:**
1. Compute f(0) = count at 65 steps (half grid)
2. Compute f(1) = count at 65 + 131 = 196 steps
3. Compute f(2) = count at 65 + 262 = 327 steps
4. Use finite differences to solve for a, b, c:
   - `a = (f(2) - 2×f(1) + f(0)) / 2`
   - `b = f(1) - f(0) - a`
   - `c = f(0)`
5. Evaluate `f(202300)` using the quadratic formula

### Complexity

**Part 1:**
- Time: O(rows × cols) for BFS
- Space: O(rows × cols) for visited map

**Part 2:**
- Time: O(3 × (327)²) ≈ O(320,000) for three BFS runs on expanded grid
- Space: O((327)²) for the largest BFS

## Programming Techniques Highlighted

- **BFS (Breadth-First Search)**: Finding shortest paths on a grid
- **Parity trick**: Exploiting the alternating nature of reachability
- **Infinite grid handling**: Modulo arithmetic for coordinate wrapping
- **Polynomial interpolation**: Using Lagrange/finite differences to extrapolate patterns
- **Hash tables**: Efficiently tracking visited cells with coordinates as keys

## Language-Specific Notes

### Fast Performers
- **ARM64 Assembly** (25ms): Hand-optimized BFS with FNV-1a hashing. Required careful handling of negative coordinates in the hash table—the empty marker had to be a value that couldn't represent any valid position.
- **C++** (27ms): Efficient `unordered_map` with custom pair hash.
- **Zig** (34ms): Excellent hash map performance with minimal overhead.

### Special Considerations

- **Big Integer Handling**: Part 2's answer (594 trillion) exceeds 32-bit integers
  - Most languages handle this naturally (Python, Ruby, JS BigInt, Clojure)
  - C/C++/Rust/Zig/Go: 64-bit `long long` or `i64` suffices
  - ARM64: 64-bit registers handle the calculation

- **Perl** (37s): Hash table operations are very slow for large BFS traversals.

- **Bash** (1868ms): Relies on associative arrays which have significant overhead.

### Infinite Grid Coordinate Mapping
All implementations use modulo for mapping infinite coordinates to grid indices:
```
grid_row = ((row % rows) + rows) % rows
grid_col = ((col % cols) + cols) % cols
```
The double modulo handles negative coordinates correctly.

## Benchmarks

| Language | Runtime (ms) | Memory (MB) |
|----------|--------------|-------------|
| C | 13.42 | 13.05 |
| ARM64 | 24.71 | 21.48 |
| C++ | 27.08 | 11.06 |
| Zig | 33.59 | 8.11 |
| Rust | 40.99 | 17.59 |
| Go | 43.86 | 17.73 |
| Java | 157.97 | 103.78 |
| Node.js | 204.85 | 125.11 |
| PHP | 208.33 | 39.17 |
| Python | 212.29 | 53.84 |
| Common Lisp | 337.22 | 93.16 |
| Clojure | 849.27 | 712.95 |
| Ruby | 926.18 | 53.25 |
| Bash | 1867.94 | 36.61 |
| ColdFusion | 4029.66 | 1076.64 |
| Perl | 37189.22 | 117.78 |

## Answers

- **Part 1**: 3578
- **Part 2**: 594115391548176
