# Day 6: Tuning Trouble

## Problem Summary

The Elves give you a malfunctioning communication device that needs to lock onto their signal. The device receives a datastream buffer of seemingly-random characters, and you need to find specific marker positions.

**Part 1 (Start-of-Packet Marker)**: Find the first position where the last 4 characters are all different.

**Part 2 (Start-of-Message Marker)**: Find the first position where the last 14 characters are all different.

## Input Format

A single line of lowercase letters representing the datastream buffer:
```
mjqjpqmgbljsphdztnvjfqwrcgsmlb
```

## Algorithmic Approach

### Key Insight

This is a classic **sliding window** problem. We maintain a window of the last N characters and check if all characters in that window are unique.

### Simple Approach (O(n * w))

For each position from N to end:
1. Extract the last N characters as a window
2. Check if all characters are unique (using a set)
3. If unique, return the current position

```python
def find_marker(data, window_size):
    for i in range(window_size, len(data) + 1):
        window = data[i - window_size:i]
        if len(set(window)) == window_size:
            return i
    return -1
```

### Optimized Approach (O(n))

Instead of rebuilding the set each time, maintain a frequency array as the window slides:
1. Keep a `freq[26]` array counting occurrences of each letter
2. Track `unique_count` - characters with exactly one occurrence
3. As window slides: add new char, remove old char, update counts
4. When `unique_count == window_size`, we found the marker

This is implemented in the C solution for optimal performance.

### Time Complexity

- Simple approach: O(n * w) where n = input length, w = window size
- Optimized approach: O(n)

### Space Complexity

- O(w) for the window/set
- O(1) for the frequency array approach (fixed 26-element array)

## Programming Techniques Highlighted

- **Sliding Window**: Classic technique for problems involving contiguous subarrays/substrings
- **Set Operations**: Using sets to check uniqueness efficiently
- **Frequency Counting**: Alternative approach using character frequency arrays
- **Bitmask Optimization**: ARM64/C implementations can use a 32-bit bitmask for the 26 letters

## Language-Specific Notes

### Fast Implementations (< 10ms)
- **C**: Uses sliding window with frequency array for O(n) performance
- **Zig**: Boolean array for tracking seen characters
- **ARM64**: Bitmask-based uniqueness checking (bits 0-25 for 'a'-'z')
- **Rust**: HashSet with byte iteration

### Scripting Languages (22-28ms)
- **Perl**: Hash-based character counting
- **Python**: `set()` with slice for simple implementation
- **Common Lisp**: `remove-duplicates` to check uniqueness

### Interpreted/VM Languages (47-70ms)
- **Node.js**: `Set` with spread operator for window
- **Java**: `HashSet<Character>` for uniqueness check
- **Ruby**: `chars.uniq.length` for elegant uniqueness test
- **PHP**: `array_unique(str_split())` pattern
- **Go**: Map-based frequency counting

### Slow (Bash special case: 409ms)
- **Bash**: Associative arrays for character counting; inherently slower due to shell overhead for string manipulation

### Heavy Runtimes (408-2500ms)
- **Clojure**: `partition` with step 1 for sliding window, `set` for uniqueness
- **ColdFusion**: Struct-based character tracking with string iteration

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.2          | 1.9         |
| Zig         | 5.3          | 1.9         |
| ARM64 asm   | 5.3          | 1.9         |
| Rust        | 6.5          | 1.9         |
| C++         | 9.4          | 1.9         |
| Perl        | 21.9         | 5.9         |
| Python      | 24.7         | 14.6        |
| Common Lisp | 27.3         | 42.5        |
| Node.js     | 47.1         | 42.6        |
| Java        | 49.4         | 49.2        |
| Ruby        | 66.6         | 28.1        |
| PHP         | 66.7         | 25.4        |
| Go          | 70.2         | 27.5        |
| Bash        | 408.8        | 6.7         |
| Clojure     | 408.3        | 140.0       |
| ColdFusion  | 2,523.0      | 1,110.6     |

## Answers

- Part 1: 1300
- Part 2: 3986
