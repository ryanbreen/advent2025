# Day 3: Gear Ratios

## Problem Summary

You arrive at a gondola lift station where the lift is broken. An engineer needs help identifying missing engine parts from a schematic diagram. The puzzle input is a 2D grid containing numbers, symbols (like `*`, `#`, `$`, `+`), and periods (`.`).

**Input Format:** A rectangular grid of characters where:
- Numbers span multiple consecutive columns (e.g., `467` occupies 3 cells)
- Symbols are single characters that are not digits or periods
- Periods (`.`) represent empty space

**Part 1:** Sum all numbers that are adjacent (including diagonally) to at least one symbol.

**Part 2:** Find all "gears" (asterisk `*` symbols adjacent to exactly two numbers) and sum their gear ratios (the product of those two adjacent numbers).

## Part 1 Analysis

### What Part 1 Asks For
Find every number in the grid that has at least one adjacent symbol (horizontally, vertically, or diagonally), then sum those "part numbers."

### Algorithm Overview
1. **Parse the grid** into a 2D array of characters
2. **Scan for numbers** by iterating through each row and identifying consecutive digit sequences
3. **For each number found**, check all cells in the bounding box around it (including diagonals)
4. **If any adjacent cell contains a symbol**, add the number to the running sum

### Key Data Structures
- **2D character array/grid:** Store the schematic
- **Number tracking:** Store each number's value along with its row and column span (start_col, end_col)

## Part 2 Analysis

### How Part 2 Changes the Problem
Instead of finding numbers adjacent to any symbol, we now focus specifically on `*` symbols that are adjacent to **exactly two** numbers. These are "gears," and we need to calculate the product of each gear's two adjacent numbers (the gear ratio).

### Additional Complexity
- Must track the relationship between `*` symbols and their adjacent numbers
- A number can be adjacent to multiple `*` symbols
- A `*` symbol with only one adjacent number is NOT a gear
- A `*` symbol with more than two adjacent numbers is also NOT a gear

### Algorithm Modifications
Two approaches work well:

**Approach 1 (Number-centric):** For each number, find all adjacent `*` symbols and build a map of `*` position to list of adjacent numbers. Then iterate through the map and compute gear ratios where exactly 2 numbers are present.

**Approach 2 (Symbol-centric):** For each `*` in the grid, scan the pre-computed list of numbers to find which ones are adjacent. If exactly 2 numbers are adjacent, compute the gear ratio.

## Algorithmic Approach

### Key Insight
Numbers in the grid span multiple cells horizontally, so adjacency must be checked for every cell the number occupies. The bounding box around a number `N` at row `r` spanning columns `[c_start, c_end]` includes all cells from `(r-1, c_start-1)` to `(r+1, c_end+1)`.

### Data Structures
- **2D Grid:** Store the raw schematic
- **Number List/Array:** Store tuples of `(value, row, start_col, end_col)` for each number found
- **Map/Dictionary (Part 2):** Map gear positions to lists of adjacent numbers

### Time Complexity
- **Parsing:** O(R x C) where R = rows, C = columns
- **Part 1:** O(N x W) where N = number of numbers, W = average number width (checking adjacency)
- **Part 2:** O(N x G) or O(G x N) depending on approach, where G = number of `*` symbols
- **Overall:** O(R x C) since N and G are bounded by grid size

### Space Complexity
- **Grid storage:** O(R x C)
- **Number list:** O(N) where N is the count of numbers
- **Gear map (Part 2):** O(G) where G is the count of `*` symbols

## Programming Techniques Highlighted

### CS Concepts Tested
1. **2D Grid Traversal:** Fundamental technique for spatial problems
2. **Boundary Checking:** Avoiding out-of-bounds access when checking neighbors
3. **Multi-cell Entity Tracking:** Numbers span multiple columns and must be treated as single entities
4. **Adjacency Detection:** 8-directional neighbor checking (including diagonals)
5. **Grouping/Aggregation:** Collecting numbers by their adjacent gear positions

### Mathematical Properties
- Adjacency is symmetric but numbers occupy multiple cells
- Gear ratio is simply multiplication of two integers
- No overflow concerns with typical puzzle sizes (numbers are 1-3 digits)

## Language-Specific Implementation Notes

### Naturally Suited Languages

**C/C++/Zig/Rust:** These systems languages excel here due to:
- Efficient 2D array handling with direct memory access
- No garbage collection overhead
- Character-level operations are natural and fast
- Strong performance for grid scanning (5-7ms range)

**Go:** Clean syntax for structs and maps makes the gear-to-numbers mapping elegant. The `Point` struct with map keys is idiomatic Go.

### Languages Requiring Workarounds

**Bash:** Grid manipulation in pure Bash is challenging. Requires string slicing with `${line:col:1}` syntax and lacks native 2D arrays. Significantly slower (1,340ms) but achieves correctness.

**Clojure:** Functional approach requires different thinking - building immutable data structures for numbers and using reduce for aggregation. Higher memory usage (1,042 MB) due to persistent data structures.

**ColdFusion:** Not designed for this type of algorithmic work. Array handling is verbose and the JVM startup plus runtime overhead shows (3,487ms).

### Notable Differences

**Python:** The most readable implementation but among the slower interpreted solutions (230ms). List comprehensions and `isdigit()` make the code concise.

**Common Lisp:** Surprisingly fast (38.6ms) among the dynamic languages. Efficient character handling with `digit-char-p` and array operations.

**Perl:** Strong text processing heritage helps here. Regular expressions could simplify number extraction, though the implementation uses character iteration.

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.3          | 1.9         |
| C++         | 5.6          | 1.9         |
| Zig         | 5.7          | 1.9         |
| Rust        | 5.9          | 1.9         |
| ARM64 asm   | 6.1          | 1.9         |
| Lisp        | 38.6         | 44.5        |
| Perl        | 42.5         | 6.3         |
| Java        | 50.6         | 47.6        |
| Node.js     | 57.1         | 51.8        |
| PHP         | 68.0         | 24.8        |
| Go          | 70.7         | 26.7        |
| Ruby        | 70.4         | 28.1        |
| Python      | 230.0        | 15.9        |
| Clojure     | 958.7        | 1,042.7     |
| Bash        | 1,340.8      | 2.8         |
| ColdFusion  | 3,487.9      | 1,119.8     |

### Performance Analysis

**Top Tier (< 10ms):** C, C++, Zig, Rust, ARM64 assembly all cluster around 5-6ms. The problem is simple enough that optimized compiled code runs almost identically across these languages. Memory footprint is minimal at 1.9 MB.

**Mid Tier (30-80ms):** Common Lisp, Perl, Java, Node.js, PHP, Go, and Ruby. This range shows the overhead of managed runtimes and interpreted execution while still being very fast in absolute terms.

**Lower Tier (> 200ms):** Python, Clojure, Bash, and ColdFusion. Python's slowness here (230ms vs typical ~30ms for simpler problems) suggests the nested loops for adjacency checking hit Python's interpretation overhead. Clojure's memory usage (1 GB+) reflects JVM plus Clojure's persistent data structures.

## Answers

- **Part 1:** 520135
- **Part 2:** 72514855

## Implementation Files

All 16 standard language implementations are available:
- `arm64/solution.s` - ARM64 Assembly
- `c/solution.c` - C
- `cpp/solution.cpp` - C++
- `rust/` - Rust (if present, uses Cargo)
- `zig/solution.zig` - Zig
- `go/solution.go` - Go
- `java/` - Java
- `node/solution.js` - Node.js
- `python/solution.py` - Python
- `ruby/solution.rb` - Ruby
- `php/solution.php` - PHP
- `perl/solution.pl` - Perl
- `bash/solution.sh` - Bash
- `clojure/solution.clj` - Clojure
- `lisp/solution.lisp` - Common Lisp
- `cfml/solution.cfm` - ColdFusion
