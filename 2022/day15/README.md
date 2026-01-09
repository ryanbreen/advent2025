# Day 15: Beacon Exclusion Zone

## Problem Summary

Sensors deployed in underground tunnels each detect their closest beacon using Manhattan distance. You need to analyze sensor coverage to find where the distress beacon might be hiding.

**Part 1**: Count how many positions in row y=2,000,000 cannot contain a beacon (are covered by at least one sensor but don't contain a known beacon).

**Part 2**: Find the one position in the range [0, 4,000,000] for both x and y that is NOT covered by any sensor. Return its "tuning frequency" (x * 4,000,000 + y).

## Input Format

Lines describing sensor and beacon positions:
```
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
```

## Algorithmic Approach

### Manhattan Distance Coverage

Each sensor has a detection radius equal to the Manhattan distance to its closest beacon. The sensor covers all positions within this radius, forming a diamond shape.

### Part 1: Row Coverage

For a specific row:
1. Calculate which x-range each sensor covers at that row
2. Range width = `dist - |sy - row|` (remaining "budget" after vertical distance)
3. Merge overlapping ranges to avoid double-counting
4. Sum total coverage, subtract beacons on that row

### Part 2: Two Approaches

**Brute Force (Row Scanning)**: Scan each of 4 million rows, check for gaps in coverage. O(rows × sensors × log(sensors)) - slow but straightforward.

**Optimized (Line Intersection)**: The distress beacon must be at distance d+1 from some sensor (just outside coverage). Each sensor's boundary forms 4 diagonal lines. Check intersections of these boundary lines for candidate positions. O(sensors²) - much faster.

### Key Insight

The diamond-shaped coverage regions have edges along lines of the form:
- `y - x = b` (positive slope)
- `y + x = b` (negative slope)

The uncovered point lies at an intersection of boundary lines from different sensors.

### Complexity

- **Part 1**: O(s × log(s)) for s sensors (parsing + range merge)
- **Part 2 Brute Force**: O(4M × s × log(s)) - billions of operations
- **Part 2 Optimized**: O(s²) - thousands of operations

## Programming Techniques Highlighted

- **Interval merging**: Sort ranges by start, merge overlapping
- **Manhattan geometry**: Diamond-shaped regions, diagonal boundaries
- **Line intersection**: For the optimized Part 2 approach
- **Large coordinate handling**: Coordinates up to 4 million, products up to 16 trillion

## Language-Specific Notes

### Algorithm Choice Impact

**Bash achieved fastest time** (27ms) by using the line intersection optimization, while most other implementations used the brute force row scanning approach.

**Zig was slowest** (42s) despite being a systems language, likely due to inefficient algorithm or implementation issues.

### Performance Groups

- **Fast (using optimizations or efficient implementations)**: Bash (27ms), ARM64 (242ms), C (251ms)
- **Moderate compiled**: Rust (471ms), C++ (581ms), Java (935ms)
- **Slow interpreted with row scanning**: Python (10s), Ruby (15s), Perl (27s)
- **Very slow**: ColdFusion (36s), Zig (42s - needs optimization)

### Memory Usage

- Compiled languages: ~2 MB
- Go, Bash, Perl, Python: 5-15 MB
- Node.js, Ruby, PHP, Lisp: 26-90 MB
- Java, Clojure, ColdFusion: 1+ GB (JVM overhead)

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Bash        | 26.7         | 7.1         |
| ARM64 asm   | 242.4        | 1.9         |
| C           | 250.9        | 1.9         |
| Rust        | 471.2        | 1.9         |
| C++         | 581.0        | 1.9         |
| Java        | 934.9        | 1,072.5     |
| Node.js     | 1,235.3      | 54.3        |
| Go          | 1,536.9      | 10.3        |
| Common Lisp | 2,041.4      | 89.8        |
| Clojure     | 6,661.5      | 1,319.2     |
| PHP         | 8,573.2      | 26.1        |
| Python      | 10,297.4     | 15.0        |
| Ruby        | 15,559.0     | 28.6        |
| Perl        | 27,578.1     | 5.7         |
| ColdFusion  | 36,493.2     | 1,080.6     |
| Zig         | 42,717.9     | 1.9         |

Note: Bash's exceptional performance is due to using the line intersection optimization rather than row scanning.

## Answers

- Part 1: 5299855
- Part 2: 13615843289729
