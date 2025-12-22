# Day 22: Monkey Market

## Problem Summary

Monkeys on the Monkey Exchange Market use pseudorandom numbers to determine their prices. Each buyer has an initial "secret number" that evolves through a deterministic process, and you need to predict market behavior.

### Part 1: Secret Number Simulation
Each buyer generates 2000 new secret numbers using a three-step transformation:
1. Multiply by 64, XOR with secret, modulo 16777216 (prune)
2. Divide by 32, XOR with secret, modulo 16777216 (prune)
3. Multiply by 2048, XOR with secret, modulo 16777216 (prune)

Sum all 2000th secret numbers across all buyers.

### Part 2: Optimal Trading Sequence
The **price** is the last digit (0-9) of each secret number. Find the 4-change sequence (consecutive price differences) that, when waited for across all buyers, yields the maximum total bananas.

Key constraint: You can only specify ONE 4-change sequence that all buyers must follow.

## Algorithmic Approach

### Part 1 Algorithm

Straightforward simulation with bitwise operations:
```
secret ^= (secret << 6)   // multiply by 64, mix
secret &= 0xFFFFFF        // prune to 24 bits
secret ^= (secret >> 5)   // divide by 32, mix
secret &= 0xFFFFFF        // prune
secret ^= (secret << 11)  // multiply by 2048, mix
secret &= 0xFFFFFF        // prune
```

- **Time Complexity**: O(buyers × iterations) = O(n × 2000)
- **Space Complexity**: O(1) per buyer

### Part 2 Algorithm

1. For each buyer, generate all 2001 secrets and compute prices (mod 10)
2. Compute 2000 price changes (differences between consecutive prices)
3. For each 4-change sequence, record the price when it **first** appears for this buyer
4. Aggregate: sum prices across all buyers for each unique sequence
5. Find the sequence with maximum total

**Key Data Structure**: Hash map from 4-change sequence → total bananas

**Optimization**: Use a "seen" set per buyer to ensure only the first occurrence counts.

- **Time Complexity**: O(buyers × iterations) = O(n × 2000)
- **Space Complexity**: O(19^4) ≈ 130,321 possible sequences

### Key Insights

1. **Bit Operations**: The mix/prune operations are equivalent to XOR and AND with a 24-bit mask. Using shifts instead of multiply/divide is crucial for performance.

2. **Sequence Indexing**: Changes range from -9 to +9 (19 values). A 4-change sequence can be packed into a single integer:
   - Base-19 encoding: `(c0+9)*19^3 + (c1+9)*19^2 + (c2+9)*19 + (c3+9)`
   - Bit packing: 5 bits per change (values 0-18), 20 bits total

3. **First Occurrence Only**: The monkey sells on the FIRST match of the sequence for each buyer. This requires tracking seen sequences per buyer.

4. **Array vs Hash Map**: Using a fixed-size array indexed by packed sequence is faster than a hash map for this problem.

## Programming Techniques Highlighted

- **Bitwise Operations**: XOR, shifts, AND for pseudorandom generation
- **Hash Maps/Dictionaries**: Tracking sequence → total mapping
- **Set/Bitmap**: Tracking first occurrence per buyer
- **Integer Packing**: Encoding tuples as single integers for efficient indexing
- **Modular Arithmetic**: Both for pruning (% 16777216 = & 0xFFFFFF) and price extraction (% 10)

## Language-Specific Notes

### Fast Performers
- **ARM64 Assembly** (60ms): Direct syscalls, manual memory management, bit operations map directly to CPU instructions. Fixed-size arrays avoid heap allocation overhead.
- **C/C++** (180-182ms): Efficient array-based implementations with direct memory access. C++ uses packed integer keys and stack-allocated arrays.
- **Rust** (206ms): Zero-cost abstractions, efficient HashMap implementation.

### Moderate Performers
- **Zig** (265ms): Excellent for this problem with its efficient standard library.
- **Go** (411ms): Good performance with maps, GC overhead minimal for this workload.
- **Java** (835ms): Reasonable but high memory usage (732MB) due to object overhead.

### Slower Performers
- **Common Lisp** (34.5s): Interpreted nature and dynamic typing add overhead.
- **Ruby** (17.6s): Dynamic language overhead for this computation-heavy problem.
- **Perl** (6.1s): Hash-heavy operations with dynamic typing.

### Memory Considerations
- **Zig/Rust**: Most memory-efficient (2-3 MB) with stack allocation and efficient data structures.
- **Java/Clojure**: Highest memory (700-1300 MB) due to JVM overhead and boxed types.

### Bash Limitations
Bash Part 2 times out because:
1. No native 64-bit arithmetic for large numbers
2. Array operations are slow
3. Hash map simulation via associative arrays is O(n) lookup
4. Function call overhead is significant

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 59.6         | 9.5         |
| C           | 182.1        | 24.3        |
| C++         | 180.7        | 4.3         |
| Rust        | 206.4        | 3.1         |
| Zig         | 265.2        | 2.4         |
| Go          | 411.0        | 13.7        |
| Java        | 834.7        | 732.1       |
| Node.js     | 886.9        | 59.7        |
| PHP         | 1,490.4      | 29.0        |
| Python      | 3,662.4      | 23.3        |
| Clojure     | 4,785.4      | 1,317.6     |
| Perl        | 6,148.8      | 10.9        |
| ColdFusion  | 14,862.1     | 1,067.6     |
| Ruby        | 17,609.8     | 37.3        |
| Common Lisp | 34,551.6     | 98.5        |
| Bash        | >120,000*    | -           |

*Bash solution works but Part 2 exceeds practical time limits.

## Answers

- **Part 1**: 19854248602
- **Part 2**: 2223
