# Day 25: Full of Hot Air

## Problem Summary

Convert numbers in the SNAFU numeral system (a balanced base-5 system) to decimal, sum them, and convert the result back to SNAFU.

**Part 1**: Calculate the sum of all fuel requirements in SNAFU format.

**Part 2**: No Part 2 - Day 25 concludes when you collect all 49 other stars!

## Input Format

List of SNAFU numbers, one per line:
```
1=-0-2
12111
2=0=
21
```

## SNAFU Number System

SNAFU (Special Numeral-Analogue Fuel Units) is a balanced base-5 positional numeral system:

| Digit | Value |
|-------|-------|
| `2`   | 2     |
| `1`   | 1     |
| `0`   | 0     |
| `-`   | -1    |
| `=`   | -2    |

Each position represents a power of 5 (1s, 5s, 25s, 125s, etc.), but digit values range from -2 to 2 instead of 0 to 4.

### Examples

| SNAFU   | Decimal Calculation                            | Result |
|---------|------------------------------------------------|--------|
| `1`     | 1                                              | 1      |
| `2=`    | 2×5 + (-2)×1 = 10 - 2                          | 8      |
| `1=0`   | 1×25 + (-2)×5 + 0×1 = 25 - 10                  | 15     |
| `1-0`   | 1×25 + (-1)×5 + 0×1 = 25 - 5                   | 20     |
| `1=11-2`| 1×3125 + (-2)×625 + 1×125 + 1×25 + (-1)×5 + 2×1| 2022   |

## Algorithmic Approach

### SNAFU to Decimal

Simple positional evaluation, processing left-to-right:

```
result = 0
for each digit in snafu:
    result = result × 5 + digit_value
```

### Decimal to SNAFU

The key insight is handling the "borrow" when converting:

```
digits = []
while n > 0:
    remainder = n % 5
    if remainder <= 2:
        digit = remainder  # Use '0', '1', or '2'
    elif remainder == 3:
        digit = '='        # -2 + carry
        n += 2             # Effectively borrow from next position
    else:  # remainder == 4
        digit = '-'        # -1 + carry
        n += 1             # Effectively borrow from next position
    digits.append(digit)
    n = n // 5
return reverse(digits)
```

The trick: when remainder is 3, we output `=` (worth -2) and add 5 to get the same value at this position (3 = 5 + (-2)). Similarly for remainder 4: output `-` (worth -1) and add 5 (4 = 5 + (-1)).

### Complexity

- **Time**: O(n × d) where n = number of SNAFU values, d = average digits per value
- **Space**: O(d) for the longest SNAFU number

## Programming Techniques Highlighted

- **Balanced numeral systems**: Non-standard positional notation
- **Base conversion**: Converting between number bases
- **Modular arithmetic**: Handling remainders with negative adjustments

## Language-Specific Notes

### Performance Characteristics

Day 25 is a simple string processing problem. All languages perform well.

**Fast (< 10ms)**:
- **C**: 5.3ms - Direct string manipulation
- **ARM64**: 5.5ms - Register-efficient implementation
- **C++**: 5.8ms - String operations
- **Rust**: 5.8ms - Iterator-based conversion
- **Zig**: 7.2ms - Clean slice operations
- **Go**: 8.5ms - Efficient string handling

**Moderate (10-100ms)**:
- **Perl**: 16.3ms - Fast hash lookups
- **Bash**: 16.2ms - AWK is surprisingly quick for simple problems
- **Python**: 23.4ms - Dictionary-based digit mapping
- **Common Lisp**: 27.4ms - Character-based processing
- **Node.js**: 44.6ms - V8 string operations
- **Ruby**: 58.5ms - Hash-based conversion
- **Java**: 61.7ms - JVM startup overhead
- **PHP**: 69.6ms - Array-based approach

**Slow (> 100ms)**:
- **Clojure**: 403.9ms - JVM startup + persistent data structures
- **ColdFusion**: 2,680.6ms - Heavy runtime overhead

### Implementation Notes

- Most implementations use a hash/map for digit-to-value conversion
- The balanced base-5 conversion is the trickiest part
- No special libraries needed - just string manipulation

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 5.3          | 1.9         |
| ARM64 asm   | 5.5          | 1.9         |
| C++         | 5.8          | 1.9         |
| Rust        | 5.8          | 1.9         |
| Zig         | 7.2          | 1.9         |
| Go          | 8.5          | 4.1         |
| Perl        | 16.3         | 5.9         |
| Bash        | 16.2         | 6.7         |
| Python      | 23.4         | 14.7        |
| Common Lisp | 27.4         | 39.6        |
| Node.js     | 44.6         | 37.8        |
| Ruby        | 58.5         | 27.8        |
| Java        | 61.7         | 46.6        |
| PHP         | 69.6         | 25.5        |
| Clojure     | 403.9        | 125.9       |
| ColdFusion  | 2,680.6      | 1,006.2     |

## Answers

- Part 1: 20=022=21--=2--12=-2
- Part 2: No Part 2 (automatic with 49 stars)
