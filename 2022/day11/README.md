# Day 11: Monkey in the Middle

## Problem Summary

Simulate monkeys passing items around based on worry levels. Each monkey:
- Inspects items, applies an operation to the worry level
- Tests divisibility to decide which monkey to throw to
- Tracks how many items it inspects

**Part 1**: Run 20 rounds. After each inspection, divide worry by 3 (relief). Return product of the two highest inspection counts.

**Part 2**: Run 10000 rounds. No worry relief (don't divide by 3). Return product of the two highest inspection counts.

## Input Format

Monkey definitions with starting items, operation, divisibility test, and target monkeys:
```
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
```

## Algorithmic Approach

### Key Insight - Modular Arithmetic

In Part 2 without worry relief, values grow exponentially (`old * old` operations). The trick:
- We only care about divisibility tests (mod N)
- Multiply all divisors together to get a common modulus
- Apply `worry % product_of_all_divisors` after each operation
- This preserves divisibility properties while keeping numbers manageable

### Part 1 Algorithm

```
for 20 rounds:
    for each monkey:
        for each item:
            apply operation
            worry = worry / 3
            if worry % divisor == 0:
                throw to if_true monkey
            else:
                throw to if_false monkey
            increment inspection count
return top_two_inspections[0] * top_two_inspections[1]
```

### Part 2 Algorithm

Same as Part 1, but:
- 10000 rounds instead of 20
- No division by 3
- Apply `worry % (product of all divisors)` to prevent overflow

### Data Structures

- **Monkey struct**: items (queue), operation, divisor, targets, inspection count
- **Items queue**: Dynamic list/deque per monkey

### Complexity

- Time: O(rounds × monkeys × items_per_round)
- Space: O(monkeys × items) - items shuffle between monkeys

## Programming Techniques Highlighted

- **Modular Arithmetic**: LCM/product trick to prevent overflow
- **Simulation**: Round-based state evolution
- **Dynamic Operations**: Parsing and executing `old * old`, `old + N`, etc.
- **Big Integer Handling**: Some languages need special handling for Part 2

## Language-Specific Notes

### Fast Performers (8-18ms)
- **Zig, C, Rust, ARM64**: Native 64-bit arithmetic, minimal overhead
- **Go, C++**: Slightly slower but still excellent

### Mid-Tier (56-388ms)
- **Common Lisp**: Fast bignum handling (56ms)
- **Java, Node.js**: JIT compilation helps (~77-95ms)
- **Python, Ruby, PHP**: Interpreted but adequate (240-310ms)
- **Perl**: Slowest of scripting languages (387ms)

### Slow (830ms-3.5s)
- **Clojure**: Immutable data structures add overhead (830ms)
- **ColdFusion**: CFML runtime overhead (3.5s)

### Bash Considerations
- Uses `bc` for arbitrary precision arithmetic
- 10000 rounds is extremely slow in shell

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 7.8          | 1.9         |
| C           | 9.1          | 1.9         |
| ARM64 asm   | 9.7          | 1.9         |
| Rust        | 9.8          | 1.9         |
| Go          | 16.6         | 9.7         |
| C++         | 18.0         | 1.9         |
| Common Lisp | 56.3         | 60.0        |
| Java        | 77.3         | 67.3        |
| Node.js     | 95.2         | 45.4        |
| Python      | 239.9        | 15.0        |
| Ruby        | 288.2        | 28.8        |
| PHP         | 308.8        | 26.3        |
| Perl        | 387.3        | 4.8         |
| Clojure     | 830.0        | 1,020.0     |
| ColdFusion  | 3,486.1      | 1,041.4     |
| Bash        | 574,626.6    | 6.7         |

## Answers

- Part 1: 107822
- Part 2: 27267163742
