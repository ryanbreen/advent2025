# Day 20: Pulse Propagation

## Problem Summary

The puzzle simulates a network of communication modules connected by cables. When a button is pressed, pulses propagate through the network. There are three module types:

- **Broadcaster**: Forwards incoming pulses to all destinations
- **Flip-flop (`%`)**: Toggles on/off when receiving a low pulse, ignores high pulses. Sends high when turning on, low when turning off.
- **Conjunction (`&`)**: Remembers last pulse from each input. Sends low if all inputs were high, otherwise sends high.

## Part 1: Pulse Counting

Simulate 1000 button presses and count the total number of low and high pulses sent. Return the product of these counts.

### Algorithm
- Use a BFS queue to process pulses in order
- Track module state (flip-flop on/off, conjunction memory)
- Count pulses as they're sent
- Repeat for 1000 presses

## Part 2: Finding the Magic Press Count

Determine the minimum number of button presses required for module `rx` to receive a single low pulse.

### Key Insight

The `rx` module is fed by a single conjunction module. For a conjunction to send low, ALL its inputs must have most recently sent high. By analyzing the network structure:

1. Find the conjunction feeding `rx`
2. Identify all modules that feed into that conjunction
3. These form independent cycles - each sends high at regular intervals
4. The answer is the LCM (Least Common Multiple) of all cycle lengths

This is much faster than simulating ~243 trillion button presses.

## Algorithmic Approach

### Data Structures
- **Hash map**: Module name -> module configuration (type, destinations, state)
- **Queue**: BFS queue of (source, destination, pulse_type) tuples
- **Set**: Track which watched nodes sent high pulses

### Complexity
- **Part 1**: O(1000 * P) where P is the number of pulses per press
- **Part 2**: O(C * P) where C is the max cycle length (typically ~4000)
- **Space**: O(M) where M is the number of modules

## Programming Techniques Highlighted

- **BFS/Queue-based simulation**: Process events in order
- **State machines**: Flip-flops and conjunctions have state
- **Cycle detection**: Part 2 requires identifying periodic behavior
- **LCM computation**: Combine cycle lengths using GCD-based LCM

## Language Notes

- **Fast performers**: ARM64, C++, Go, C - efficient hash maps and minimal overhead
- **Zig slower than expected**: The hash map implementation has higher overhead for this use case
- **Common Lisp strong**: Shows that Lisp can be competitive for simulation tasks
- **BigInt handling**: Part 2 answer exceeds 32-bit range, requiring 64-bit or arbitrary precision:
  - Node.js: `BigInt` for LCM calculation
  - Bash: `bc` for arbitrary precision arithmetic
  - Python: Native arbitrary precision

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64       | 7.1          | 1.9         |
| C++         | 15.5         | 1.9         |
| Go          | 20.6         | 9.7         |
| C           | 22.9         | 1.9         |
| Rust        | 33.0         | 1.9         |
| Java        | 42.0         | 47.8        |
| Common Lisp | 54.5         | 71.5        |
| Zig         | 71.1         | 2.4         |
| Node.js     | 72.3         | 47.7        |
| Python      | 98.4         | 16.0        |
| PHP         | 111.7        | 25.8        |
| Ruby        | 146.6        | 27.9        |
| Perl        | 189.4        | 5.0         |
| Clojure     | 535.6        | 316.3       |
| ColdFusion  | 3,284.6      | 1,099.8     |
| Bash        | 11,619.5     | 7.0         |

## Answers

- **Part 1**: 777666211
- **Part 2**: 243081086866483
