# Day 7: Camel Cards

## Problem Summary

While riding a camel across Desert Island, you learn to play **Camel Cards**, a poker-like game where hands are ranked by type and then by individual card values.

**Input format**: Lines of `HAND BID` where HAND is 5 cards (A, K, Q, J, T, 9-2) and BID is an integer.

### Part 1: Standard Ranking

Rank hands by type (strongest to weakest):
1. Five of a kind (AAAAA)
2. Four of a kind (AA8AA)
3. Three of a kind (TTT98)
4. Full house (23332)
5. Two pair (23432)
6. One pair (A23A4)
7. High card (23456)

For hands of the same type, compare cards left-to-right using strength order: A > K > Q > J > T > 9 > ... > 2.

Calculate total winnings: sum of (rank × bid) for all hands after sorting weakest to strongest.

### Part 2: Joker Wildcards

J cards become jokers (wildcards) that can act as any card to maximize hand type. However, for tiebreaking, J is now the **weakest** card: A > K > Q > T > 9 > ... > 2 > J.

## Algorithmic Approach

### Key Insight

The joker optimization is greedy: always add jokers to whichever card type you already have the most of. This guarantees the strongest possible hand type because:
- 5 jokers → five of a kind
- 4 jokers + 1 other → five of a kind (jokers become that card)
- 3 jokers + 2 same → five of a kind
- etc.

You never benefit from splitting jokers across different card types.

### Data Structures Used

- **Hash map/Counter**: Count occurrences of each card
- **Sorted array**: Sort count values descending to identify hand type pattern
- **Comparison key**: Tuple of (hand_type, card1_strength, card2_strength, ...)

### Algorithm

```
Part 1:
1. For each hand, count card frequencies
2. Sort counts descending → pattern identifies type
   [5] = five of a kind, [4,1] = four of a kind, etc.
3. Create sort key: (type_rank, card_strengths...)
4. Sort all hands by key, compute sum(rank × bid)

Part 2:
1. Count non-joker cards only
2. Add joker count to highest existing count
3. Use weakened J strength for tiebreaking
```

### Complexity

- **Time**: O(n log n) where n = number of hands (dominated by sorting)
- **Space**: O(n) to store hands with their sort keys

## Programming Techniques Highlighted

- **Counting/Histograms**: Using frequency counts to classify patterns
- **Multi-key sorting**: Primary sort by type, secondary by card values
- **Greedy optimization**: Jokers always enhance the most common card

## Language-Specific Notes

### Fast Performers
- **C, Zig, ARM64**: Direct memory manipulation, minimal overhead. Hand type detection via simple conditionals on sorted counts.
- **Rust**: Zero-cost abstractions with `sort_by_key` and iterators.

### Mid-tier Performance
- **Python**: Clean implementation with `Counter` from collections. List comparison works naturally for multi-key sorting.
- **Common Lisp**: Efficient hash tables and `sort` with custom predicates.

### Notable Implementations
- **Bash**: String manipulation with `grep -o` to count characters. Very slow due to subprocess overhead for each character count.
- **ARM64 Assembly**: Manual implementation of sorting and counting. Uses registers efficiently for the small fixed hand size (5 cards).

### Sorting Approaches
- Most languages: Create a sort key tuple/array, use built-in stable sort
- C/Zig: Custom comparison function with `qsort`
- ARM64: Bubble sort (acceptable for ~1000 hands)

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 6.2          | 1.9         |
| C           | 6.4          | 1.9         |
| ARM64 asm   | 7.8          | 1.9         |
| Rust        | 12.0         | 1.9         |
| C++         | 17.7         | 1.9         |
| Python      | 28.9         | 16.2        |
| Lisp        | 40.7         | 61.7        |
| PHP         | 55.7         | 26.7        |
| Go          | 60.2         | 27.4        |
| Ruby        | 61.5         | 28.3        |
| Java        | 71.3         | 66.1        |
| Node.js     | 76.6         | 58.8        |
| Perl        | 83.1         | 6.8         |
| Clojure     | 473.4        | 242.5       |
| ColdFusion  | 7,809.3      | 1,146.7     |
| Bash        | 23,419.0     | 2.1         |

## Answers

- **Part 1**: 241344943
- **Part 2**: 243101568
