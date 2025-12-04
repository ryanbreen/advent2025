# Advent of Code 2025

Solutions implemented in 16 languages.

## Progress

| Day | Stars | Languages |
|-----|-------|-----------|
| 1   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 2   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 3   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 4   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |

## Benchmarks

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

**Note on runtime overhead**: Some languages (ColdFusion, Clojure, Java) have significant startup overhead that dominates these benchmarks. ColdFusion runs via CommandBox/Lucee, which spins up a full JVM and servlet engine for each execution (~2.5s baseline). These languages are designed for long-running server processes, not CLI scripts, so the benchmarks reflect startup cost more than computational efficiency.

### Day 1: Secret Entrance

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 5.6          | 1.9         |
| C           | 6.9          | 1.9         |
| C++         | 6.9          | 1.9         |
| ARM64 asm   | 7.0          | 1.9         |
| Rust        | 7.3          | 1.9         |
| Go          | 8.3          | 4.2         |
| Perl        | 16.1         | 5.4         |
| Lisp        | 27.5         | 39.7        |
| Python      | 30.8         | 16.1        |
| Java        | 43.2         | 45.5        |
| Node.js     | 50.9         | 41.5        |
| PHP         | 53.4         | 24.8        |
| Ruby        | 59.5         | 28.7        |
| Bash        | 98.6         | 2.2         |
| Clojure     | 431.0        | 132.8       |
| ColdFusion  | 2,590.5      | 1,202.0     |

### Day 2: Gift Shop

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 37           | 1.9         |
| ARM64 asm   | 65           | 1.9         |
| Rust        | 136          | 1.9         |
| C++         | 198          | 1.9         |
| C           | 211          | 1.9         |
| Go          | 217          | 10.0        |
| Java        | 289          | 596.0       |
| Node.js     | 315          | 87.6        |
| Lisp        | 587          | 89.4        |
| PHP         | 610          | 24.4        |
| Python      | 1,192        | 15.6        |
| Clojure     | 1,210        | 1,298       |
| Ruby        | 2,092        | 28.2        |
| Perl        | 2,461        | 4.3         |
| ColdFusion  | 6,909.3      | 1,141.8     |
| Bash        | 90,930       | 1.5         |

### Day 3: Lobby

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C++         | 6.4          | 1.9         |
| C           | 6.5          | 1.9         |
| ARM64 asm   | 6.5          | 1.9         |
| Zig         | 9.3          | 1.9         |
| Perl        | 22.7         | 4.6         |
| Lisp        | 29.2         | 41.7        |
| Python      | 34.9         | 15.6        |
| Rust        | 49.0         | 1.9         |
| Node.js     | 49.5         | 45.7        |
| Java        | 66.7         | 47.1        |
| PHP         | 67.2         | 24.6        |
| Ruby        | 67.7         | 28.3        |
| Go          | 103.3        | 59.7        |
| Clojure     | 664.1        | 932.6       |
| ColdFusion  | 2,808.6      | 1,080.5     |
| Bash        | 7,360.1      | 1.9         |

### Day 4: Printing Department

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 6.8          | 1.9         |
| Rust        | 7.5          | 2.1         |
| C++         | 7.9          | 1.9         |
| Zig         | 7.9          | 2.0         |
| Go          | 8.6          | 4.6         |
| ARM64 asm   | 10.3         | 1.9         |
| Java        | 67.8         | 47.4        |
| Perl        | 68.7         | 20.1        |
| Lisp        | 74.9         | 40.6        |
| PHP         | 78.5         | 35.3        |
| Python      | 80.2         | 26.1        |
| Node.js     | 81.6         | 66.6        |
| Ruby        | 171.8        | 36.9        |
| Clojure     | 658.6        | 537.5       |
| ColdFusion  | 3,613.0      | 1,119.9     |
| Bash        | 7,068        | 8.5         |
