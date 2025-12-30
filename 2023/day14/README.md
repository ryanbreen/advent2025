# Day 14: Parabolic Reflector Dish

## Problem Summary

You discover a large parabolic reflector dish that needs to be focused by moving rocks on a platform. The platform has rounded rocks (`O`) that roll when tilted, and cube-shaped rocks (`#`) that stay fixed.

**Part 1**: Tilt the platform north so all rounded rocks roll to the top. Calculate the total load on the north support beams, where each rock's load equals its distance from the south edge.

**Part 2**: Run 1,000,000,000 "spin cycles" (tilt N→W→S→E in sequence) and calculate the final load.

## Algorithmic Approach

### Part 1: Single Tilt

For tilting in any direction, use a "write position" pointer:
1. Scan in the tilt direction
2. Track where the next rolling rock should land
3. When hitting a fixed rock `#`, reset the write position
4. When hitting a rolling rock `O`, move it to the write position and advance

```python
def tilt_north(grid):
    for col in range(cols):
        write_pos = 0
        for row in range(rows):
            if grid[row][col] == '#':
                write_pos = row + 1
            elif grid[row][col] == 'O':
                grid[row][col] = '.'
                grid[write_pos][col] = 'O'
                write_pos += 1
```

### Part 2: Cycle Detection

Running 1 billion cycles directly is infeasible. The key insight is that the grid state eventually repeats, forming a cycle.

1. After each spin cycle, hash the grid state
2. Store seen states in a hash map with their cycle number
3. When a repeated state is found:
   - `cycle_length = current_cycle - first_seen_cycle`
   - `remaining = (1,000,000,000 - current_cycle) % cycle_length`
   - Run only the remaining cycles

### Complexity

- **Part 1**: O(rows × cols) - single pass through grid
- **Part 2**: O(C × rows × cols) where C is the cycle length (typically ~100-200)
- **Space**: O(rows × cols) for storing seen states

## Programming Techniques Highlighted

- **Simulation**: Modeling physical rock rolling
- **Cycle detection**: Floyd's or hash-based detection
- **Hash maps**: For efficient state lookup
- **Grid manipulation**: In-place 2D array updates

## Language-Specific Notes

- **Fast performers**: C, C++, Zig, ARM64, Go - efficient memory access patterns
- **JVM startup**: Java benefits from JIT compilation but has warm-up overhead
- **Scripting languages**: Python/Ruby/Perl slower due to interpretation overhead
- **Clojure**: Immutable data structures add overhead for grid mutation
- **Bash**: String manipulation in shell is extremely slow for this problem

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 13.0         | 1.9         |
| C++         | 15.5         | 3.2         |
| Zig         | 15.7         | 4.2         |
| ARM64       | 17.2         | 1.9         |
| Go          | 17.5         | 9.5         |
| Rust        | 28.5         | 8.7         |
| Java        | 81.8         | 53.1        |
| Node.js     | 88.6         | 50.7        |
| Common Lisp | 117.8        | 53.0        |
| PHP         | 272.5        | 28.3        |
| Python      | 410.2        | 29.6        |
| Ruby        | 555.9        | 32.3        |
| Perl        | 669.1        | 7.4         |
| Clojure     | 1278.7       | 1309.3      |
| ColdFusion  | 4663.3       | 1082.9      |
| Bash        | 32063.2      | 9.4         |

## Answers

- Part 1: **111339**
- Part 2: **93736**
