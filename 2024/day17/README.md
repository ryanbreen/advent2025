# Day 17: Chronospatial Computer

## Problem Summary

A 3-bit virtual machine with three registers (A, B, C) and eight instructions. The program is a list of 3-bit numbers (0-7), with each pair representing an opcode and operand.

### Part 1: Run the Program
Execute the program with the given initial register values and collect all outputs from the `out` instruction. Return the outputs as a comma-separated string.

### Part 2: Self-Replicating Program
Find the smallest initial value for register A that causes the program to output an exact copy of itself (a quine).

## Algorithmic Approach

### Part 1: VM Emulation

Straightforward instruction interpretation with an instruction pointer:

**Instructions:**
| Opcode | Name | Operation |
|--------|------|-----------|
| 0 | adv | A = A >> combo_operand |
| 1 | bxl | B = B XOR literal_operand |
| 2 | bst | B = combo_operand & 7 |
| 3 | jnz | if A != 0: IP = literal_operand |
| 4 | bxc | B = B XOR C |
| 5 | out | output(combo_operand & 7) |
| 6 | bdv | B = A >> combo_operand |
| 7 | cdv | C = A >> combo_operand |

**Combo operands:** 0-3 are literal, 4=A, 5=B, 6=C, 7=reserved

**Complexity**: O(instructions * iterations) - typically O(n) where n is log(A)/3

### Part 2: Reverse Engineering

The key insight is understanding the program's structure. Most AoC Day 17 Part 2 programs follow a pattern:

```
loop:
    B = A & 7           (bst)
    [some XOR/shift operations on B using A]
    output B & 7        (out)
    A = A >> 3          (adv)
    if A != 0: goto loop (jnz)
```

This means:
1. Each iteration outputs one digit
2. A is right-shifted by 3 bits each iteration
3. The program halts when A becomes 0
4. To output N digits, A must have roughly N*3 bits

**Backward Search Algorithm:**

Instead of trying all possible A values (infeasible for 16-digit output requiring ~48-bit A), work backwards:

1. Start from the last output digit
2. For each position, try all 8 possible 3-bit values (0-7)
3. Build A by prepending 3 bits: `new_A = (current_A << 3) | bits`
4. Run the VM and check if output matches the expected suffix
5. Recursively search for earlier digits
6. Return the first (smallest) valid A found

**Why this works:**
- The program processes A from lowest to highest bits
- Each output digit depends primarily on the current low 3 bits of A
- By building A backwards, we constrain the search space dramatically

**Complexity**: O(8^N * N * VM_cost) worst case, but pruning makes it much faster in practice

### Key Insight

The program's structure (dividing A by 8 each iteration, outputting one digit per iteration) means we can decompose the problem. Each digit of output corresponds to a 3-bit "window" of A, though with some dependencies on higher bits. The backward search exploits this structure.

## Data Structures Used

- **Registers**: Three integer variables (A, B, C) - must support 64-bit for Part 2
- **Program**: Array/vector of integers (0-7)
- **Output**: Dynamic array/list to collect outputs
- **Recursion stack**: For backward search in Part 2

## Complexity

- **Time**:
  - Part 1: O(log(A)) iterations
  - Part 2: O(8^N) worst case, but heavy pruning makes it feasible
- **Space**: O(N) for output and recursion depth where N = program length

## Programming Techniques Highlighted

- **Virtual machine/interpreter**: Classic opcode dispatch pattern
- **Bit manipulation**: XOR, AND, right shifts are core operations
- **Reverse engineering**: Understanding program behavior to solve Part 2
- **Backtracking search**: Building the solution incrementally with pruning
- **Divide and conquer**: Breaking the 48-bit search into 16 3-bit searches

## Language-Specific Notes

### 64-bit Integer Requirements
Part 2's answer (~258 trillion) requires 64-bit integers:
- **C/C++**: `int64_t` or `uint64_t`
- **Rust**: `u64`
- **Go**: `int64` or `uint64`
- **Java**: `long`
- **JavaScript**: `BigInt` (Number loses precision above 2^53)
- **PHP**: Native 64-bit on 64-bit systems
- **Perl**: Native integers usually sufficient
- **Bash**: Native arithmetic handles 64-bit

### Fast Performers
- **C/Rust/Zig**: Direct bit operations, minimal overhead
- **Go**: Efficient with simple data types

### Interesting Implementations
- **JavaScript**: Must use BigInt for Part 2 to avoid precision loss
- **ARM64 Assembly**: Great for bit manipulation - XOR, AND, LSR are single instructions
- **Clojure**: Functional approach with recursion; `bit-shift-right` for division by powers of 2

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 5.12         | 1.92        |
| Go          | 5.24         | 1.91        |
| Rust        | 5.59         | 1.92        |
| C++         | 7.13         | 1.91        |
| ARM64 asm   | 7.24         | 1.89        |
| C           | 7.62         | 1.89        |
| Perl        | 27.86        | 6.83        |
| Common Lisp | 31.39        | 43.27       |
| Python      | 33.41        | 15.83       |
| Node.js     | 52.02        | 45.69       |
| Java        | 53.99        | 46.28       |
| PHP         | 75.27        | 24.66       |
| Ruby        | 93.70        | 28.34       |
| Bash        | 252.70       | 6.69        |
| Clojure     | 465.05       | 133.05      |
| ColdFusion  | 3104.10      | 1110.56     |

## Answers

- Part 1: 2,1,4,0,7,4,0,2,3
- Part 2: 258394985014171
