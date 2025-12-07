# Day 2: Cube Conundrum

## Problem Summary

You arrive at Snow Island where an Elf proposes a guessing game with colored cubes. The Elf has a bag containing red, green, and blue cubes. For each game, the Elf reaches into the bag multiple times, shows you a handful of cubes, and puts them back. Your puzzle input is a record of these games.

### Input Format

Each line represents one game with the format:
```
Game X: <draw1>; <draw2>; <draw3>; ...
```

Where each draw is a comma-separated list of cube counts:
```
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
```

- Game ID is extracted from "Game X"
- Draws are separated by semicolons
- Each draw contains color counts separated by commas
- Colors are "red", "green", or "blue"

### What We're Computing

- **Part 1**: Sum of game IDs where all draws are possible with 12 red, 13 green, and 14 blue cubes
- **Part 2**: Sum of the "power" (product of minimum required cubes) for each game

## Part 1 Analysis

### What Does Part 1 Ask?

Determine which games would have been possible if the bag contained only:
- 12 red cubes
- 13 green cubes
- 14 blue cubes

A game is "possible" if every single draw in that game shows at most the maximum allowed count for each color. If any draw exceeds the limit for any color, the entire game is impossible.

### Algorithm Overview

1. Parse each game line to extract the game ID and all draws
2. For each draw, check if any color count exceeds the maximum (12/13/14)
3. If all draws in a game are valid, add the game ID to the sum
4. Early exit optimization: stop checking a game once any invalid draw is found

### Key Data Structures

- **Dictionary/Map**: Maps color names to their maximum allowed counts
- **Simple iteration**: No complex data structures needed; linear scan through draws

## Part 2 Analysis

### How Does Part 2 Change the Problem?

Instead of checking against fixed limits, Part 2 asks: "What is the minimum number of cubes of each color that must have been in the bag for this game to be possible?"

### Key Insight

The minimum number of cubes of each color needed is simply the **maximum** count of that color seen across all draws. If the Elf showed 20 red cubes in one draw, the bag must contain at least 20 red cubes.

### Algorithm Modifications

1. For each game, track the maximum count seen for each color (red, green, blue)
2. These maximums are the "minimum cubes needed"
3. Calculate the "power" as the product: `min_red * min_green * min_blue`
4. Sum all powers

## Algorithmic Approach

### Key Insight

Both parts rely on the same fundamental observation: each draw gives us information about the minimum contents of the bag. The bag must contain **at least** as many cubes of each color as the maximum shown in any single draw.

### Data Structures

- **String parsing**: Split on `:`, `;`, `,` to extract game structure
- **Color tracking**: Either a dictionary/map or three integer variables for red/green/blue counts
- No advanced data structures required (no graphs, trees, queues, etc.)

### Time Complexity

- **O(n * m * k)** where:
  - n = number of games
  - m = average draws per game
  - k = average cubes per draw
- In practice, this is effectively **O(n)** since m and k are small constants

### Space Complexity

- **O(1)** additional space beyond input storage
- Only need to track 3 color counts per game

## Programming Techniques Highlighted

### CS Concepts Tested

1. **String Parsing**: Multi-level tokenization (colon, semicolon, comma, space)
2. **Constraint Checking**: Verifying values against fixed bounds
3. **Aggregation**: Finding maximum values across a stream of data
4. **Early Exit Optimization**: Short-circuiting when a constraint is violated

### Mathematical Properties

- **Max operation**: The minimum bag size equals the maximum draw for each color
- **Product calculation**: The "power" is a simple multiplication of three values
- No complex math required (no modular arithmetic, no combinatorics)

## Language-Specific Implementation Notes

### Languages Naturally Suited to This Problem

**Python** excels here due to:
- Easy string splitting with `split()`
- Dictionary for color mapping: `{"red": 12, "green": 13, "blue": 14}`
- Clean `max()` function for tracking minimums

**Rust** provides strong type safety:
- Custom `CubeSet` struct for type-safe color tracking
- Pattern matching for color strings
- Functional iterator chains with `filter()`, `map()`, `sum()`

### Languages Requiring More Effort

**C** requires manual:
- String tokenization with `strtok_r()` for thread safety
- Careful memory management (`strdup`/`free` for string copies)
- Manual whitespace handling with `isspace()` and `isdigit()`

**ARM64 Assembly** must handle:
- Custom string comparison routines
- Register management for tracking three color counts
- Manual stack frame setup for function calls

**Bash** suffers from:
- Performance overhead of spawning processes for string operations
- Awkward parsing requiring sed/awk/cut combinations
- Slowest by far at 15+ seconds vs <10ms for compiled languages

### Performance Characteristics

**Compiled Systems Languages** (ARM64, C, C++, Rust, Zig):
- All cluster around 5-10ms
- Memory footprint minimal at ~2MB
- String parsing overhead negligible at this scale

**Scripting Languages** (Python, Perl, Ruby, PHP, Node.js):
- Range from 25-90ms
- Higher memory footprint (15-50MB)
- Interpreter startup dominates runtime

**JVM Languages** (Java, Clojure):
- JVM startup adds overhead
- Clojure particularly slow due to additional runtime

**ColdFusion and Bash**:
- Orders of magnitude slower
- Bash's 15+ seconds is due to spawning subprocesses for parsing

## Benchmark Results

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 5.9          | 1.9         |
| Rust        | 7.0          | 1.9         |
| C++         | 8.9          | 1.9         |
| C           | 9.1          | 1.9         |
| Zig         | 9.2          | 1.9         |
| Perl        | 24.7         | 6.1         |
| Python      | 27.7         | 15.5        |
| Lisp        | 28.1         | 40.7        |
| Node.js     | 47.0         | 40.4        |
| Java        | 63.6         | 50.5        |
| PHP         | 68.5         | 24.6        |
| Ruby        | 85.5         | 28.3        |
| Go          | 136.6        | 26.4        |
| Clojure     | 477.8        | 127.3       |
| ColdFusion  | 3,542.7      | 1,138.6     |
| Bash        | 15,625.7     | 2.3         |

### Notable Observations

1. **ARM64 Assembly** is fastest at 5.9ms despite the parsing complexity
2. **Go** is surprisingly slow (136.6ms) compared to other compiled languages, likely due to regex or reflection usage
3. **Bash** is extremely slow because string parsing requires many subprocess spawns
4. Memory usage follows expected patterns: compiled (~2MB) < interpreted (~15-50MB) < JVM (~50-500MB)

## Answers

- **Part 1**: 2416
- **Part 2**: 63307
