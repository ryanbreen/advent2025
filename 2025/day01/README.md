# Day 1: Secret Entrance

## Problem Summary

You arrive at the North Pole's secret entrance but the password has changed. A safe with a combination dial holds the new password, and you have instructions for operating the dial.

### Input Format
The input is a sequence of rotation instructions, one per line. Each instruction consists of:
- A direction: `L` (left, toward lower numbers) or `R` (right, toward higher numbers)
- A distance: an integer indicating how many clicks to rotate

Example:
```
L68
R48
L5
```

### The Dial
- Numbers 0-99 arranged in a circle
- Starts pointing at position 50
- Wrapping behavior: L from 0 goes to 99, R from 99 goes to 0

### What We Compute
- **Part 1**: Count how many times the dial **ends** at position 0 after completing a rotation
- **Part 2**: Count how many times the dial **passes through or lands on** position 0 during all rotations (every click counts)

---

## Part 1 Analysis

### What Part 1 Asks
After following each rotation instruction, check if the dial points at 0. Count the total number of times this happens.

### Algorithm Overview
1. Initialize position to 50
2. For each instruction, parse direction and distance
3. Calculate new position using modular arithmetic
4. If new position is 0, increment counter
5. Return final count

### Key Data Structures
- Single integer for current position
- Simple counter for zeros encountered

### Implementation Pattern
```python
position = 50
for instruction in instructions:
    direction, distance = parse(instruction)
    if direction == 'L':
        position = (position - distance) % 100
    else:
        position = (position + distance) % 100
    if position == 0:
        count += 1
```

---

## Part 2 Analysis

### How Part 2 Changes the Problem
Instead of only counting when we **end** at 0, we must count every time we **click through** position 0. A single large rotation like `R1000` from position 50 would pass through 0 ten times!

### Additional Complexity
The key insight is that we cannot simply simulate every click (that would be inefficient for large distances). Instead, we must mathematically determine how many times we cross position 0 during a rotation.

### Algorithm Modifications

For a rotation of distance `D`:

**Moving Left from position P:**
- First hit 0 after `P` steps (if `P > 0`)
- Then hit 0 every 100 steps after that
- Formula: `1 + (D - P) // 100` if `D >= P` and `P > 0`
- Special case when `P == 0`: we hit 0 again after 100 steps, so `D // 100`

**Moving Right from position P:**
- First hit 0 after `100 - P` steps (if `P > 0`)
- Then hit 0 every 100 steps after that
- Formula: `1 + (D - (100 - P)) // 100` if `D >= 100 - P` and `P > 0`
- Special case when `P == 0`: we hit 0 again after 100 steps, so `D // 100`

---

## Algorithmic Approach

### Key Insight
The crucial realization is that crossing zero is a **periodic event**. Once you understand how many steps it takes to reach 0 from your current position, every subsequent crossing is exactly 100 steps apart. This transforms an O(D) simulation into an O(1) calculation per instruction.

### Data Structures
- **Integer position**: Current dial position (0-99)
- **Integer counter**: Running total of zero crossings
- No complex data structures needed; this is purely arithmetic

### Time Complexity
- **Part 1**: O(N) where N is the number of instructions
- **Part 2**: O(N) - each instruction is processed in constant time using the mathematical formula

### Space Complexity
- O(1) additional space (just tracking position and count)
- O(N) if we store all instructions in memory (though streaming is possible)

---

## Programming Techniques Highlighted

### Computer Science Concepts
1. **Modular Arithmetic**: The dial wraps around at 100, making this a perfect application for the modulo operator. Understanding how negative modulo works across languages is critical.

2. **Simulation vs. Mathematical Analysis**: Part 1 can be solved by simple simulation. Part 2 forces you to recognize the periodic pattern and compute directly rather than simulating each click.

3. **Floor Division**: The formula `1 + (D - threshold) // 100` elegantly counts how many complete cycles of 100 occur after the first crossing.

### Mathematical Properties Exploited
- **Periodicity**: Zero crossings occur at regular 100-step intervals
- **Integer Division**: Floor division naturally counts complete cycles

### Edge Cases to Consider
- Starting position is 0 (don't count the start, only count when we return)
- Distance is exactly the steps needed to reach 0 (counts once)
- Distance is less than steps needed (counts zero times)
- Very large distances (the mathematical formula handles these efficiently)

---

## Language-Specific Implementation Notes

### Naturally Suited Languages

**Python** excels here due to its handling of negative modulo. In Python, `(-5) % 100` returns `95`, which is mathematically correct for this problem. The solution is clean and readable:
```python
position = (position - distance) % 100  # Just works!
```

**Go** and **Java** require extra care because their modulo can return negative values. Go's solution shows the typical pattern:
```go
position = (position - distance) % 100
if position < 0 {
    position += 100
}
```

### Languages Requiring Workarounds

**C/C++** have implementation-defined behavior for negative modulo in older standards. The C solution uses a while loop to handle negative wraparound:
```c
int new_pos = position - distance;
while (new_pos < 0) new_pos += 100;
position = new_pos;
```

**ARM64 Assembly** represents the most challenging implementation. It requires manual handling of:
- String parsing (extracting direction character and distance integer)
- Division and modulo operations (no native modulo instruction)
- Conditional logic for left vs. right rotation

### Performance Characteristics

| Language Family | Characteristics |
|-----------------|-----------------|
| **Systems (C, C++, Zig)** | Fastest execution, but more verbose modulo handling |
| **Compiled GC (Go, Java)** | Good performance, explicit negative modulo handling |
| **Scripting (Python, Ruby, Perl)** | Cleaner modulo semantics, slower but irrelevant for this problem size |
| **Shell (Bash)** | Surprisingly capable using `$(( ))` arithmetic expansion |
| **Functional (Clojure, Common Lisp)** | Natural fit for the transformation pipeline |

### Notable Differences

1. **Modulo Behavior**: The biggest cross-language difference. Python, Ruby, and Perl use floored division (always positive result with positive divisor). C, Java, and Go use truncated division (can be negative).

2. **Integer Parsing**: Some languages (Python, Ruby) make this trivial with slicing. Others (C, Assembly) require more explicit parsing.

3. **No Special Libraries Needed**: This problem is solvable with just basic arithmetic in every language - no special data structures or algorithms required.

---

## Answers

- **Part 1**: 1150
- **Part 2**: 6738
