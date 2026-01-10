# Day 19: Not Enough Minerals

## Problem Summary

Optimize robot factory production to maximize geode cracking. Start with 1 ore robot and build additional ore, clay, obsidian, and geode robots using collected resources.

**Part 1**: Find maximum geodes each blueprint can produce in 24 minutes. Sum quality levels (blueprint_id * geodes).

**Part 2**: For first 3 blueprints, find maximum geodes in 32 minutes. Return the product.

## Input Format

Each line contains a blueprint with robot costs:
```
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 7 clay. Each geode robot costs 4 ore and 13 obsidian.
```

## Robot Types and Resources

- **Ore robot**: Costs ore, produces ore
- **Clay robot**: Costs ore, produces clay
- **Obsidian robot**: Costs ore + clay, produces obsidian
- **Geode robot**: Costs ore + obsidian, produces geodes

Each robot produces 1 resource per minute. Building takes 1 minute.

## Algorithmic Approach

### Core Algorithm: DFS with Aggressive Pruning

This is a computationally hard optimization problem. Naive BFS/DFS will timeout. Critical optimizations:

1. **Upper bound pruning**: At any state, calculate maximum possible geodes assuming we build a geode robot every remaining minute:
   ```
   upper_bound = geodes + geo_robots * remaining + remaining*(remaining-1)/2
   ```
   If upper_bound <= best_so_far, prune this branch.

2. **Resource capping**: Don't store more resources than we can possibly use:
   ```
   max_ore_needed = remaining * max(ore_costs)
   ore = min(ore, max_ore_needed)
   ```

3. **Robot limits**: Never build more robots than the maximum we can use per minute:
   ```
   max_ore_robots = max(ore cost of all robots)
   max_clay_robots = clay cost of obsidian robot
   max_obsidian_robots = obsidian cost of geode robot
   ```

4. **Geode priority**: If we can build a geode robot, ALWAYS build it (greedy choice that works for this problem).

5. **State memoization**: Cache (time, capped_resources, robot_counts) -> best_geodes to avoid recomputation.

### Complexity

- **Time**: O(B * S) where B = blueprints, S = unique states (exponential but heavily pruned)
- **Space**: O(S) for memoization

## Programming Techniques Highlighted

- **Optimization/Search**: DFS with branch and bound
- **Pruning**: Upper bound calculation, resource limits
- **State space reduction**: Capping resources, limiting robot counts
- **Memoization**: Caching visited states

## Language-Specific Notes

### Performance Characteristics

Day 19 is one of the slowest AoC problems. Execution times vary greatly by language efficiency.

**Fast (< 2s)**:
- **C**: 632ms - efficient DFS implementation
- **Go**: 791ms - good hash map performance
- **Rust**: 882ms - competitive with hash map
- **C++**: 986ms - STL unordered_map

**Moderate (2-10s)**:
- **Java**: 1.9s - HashMap overhead
- **Common Lisp**: 4.6s - hash tables work well
- **PHP**: 6.3s - associative arrays
- **Clojure**: 8.0s - JVM startup + functional overhead
- **Node.js**: 8.2s - V8 performance

**Slow (10-30s)**:
- **ARM64**: 13.9s - assembly implementation
- **Perl**: 23.1s - hash performance
- **Zig**: 24.5s - implementation could be optimized

**Very slow (30s+)**:
- **Python**: 46.8s - interpreter overhead
- **Ruby**: 47.3s - similar to Python
- **ColdFusion**: 53.8s - JVM overhead
- **Bash**: 242.1s (~4 min) - AWK-based DFS with hash table memoization

### Implementation Notes

- ARM64 assembly is surprisingly slow due to state space explosion in assembly
- C and Go excel here due to efficient hash maps and low overhead
- Scripting languages struggle with the deep recursion and many hash lookups
- State memoization is critical - without it, solutions timeout

## Benchmarks

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 632.0        | 61.7        |
| Go          | 790.6        | 78.3        |
| Rust        | 882.0        | 152.3       |
| C++         | 985.8        | 108.0       |
| Java        | 1,934.9      | 1,319.2     |
| Common Lisp | 4,556.0      | 711.6       |
| PHP         | 6,308.7      | 176.9       |
| Clojure     | 7,973.8      | 2,215.4     |
| Node.js     | 8,172.2      | 889.1       |
| ARM64 asm   | 13,896.4     | 1.3         |
| Perl        | 23,143.3     | 869.4       |
| Zig         | 24,483.8     | 1.5         |
| Python      | 46,846.7     | 2,680.1     |
| Ruby        | 47,328.9     | 417.6       |
| ColdFusion  | 53,752.6     | 2,899.7     |
| Bash        | 242,100.0    | 375.6       |

## Answers

- Part 1: 1565
- Part 2: 10672
