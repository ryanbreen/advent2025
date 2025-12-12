# Advent of Code 2025

Solutions implemented in 16 languages.

## Progress

| Day | Stars | Languages |
|-----|-------|-----------|
| 1   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 2   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 3   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 4   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 5   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 6   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 7   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 8   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 9   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 10  | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 11  | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 12  | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |

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

### Day 5: Cafeteria

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 6.6          | 1.9         |
| Zig         | 7.4          | 1.9         |
| C++         | 16.2         | 1.9         |
| C           | 24.6         | 1.9         |
| Common Lisp | 27.6         | 40.9        |
| Python      | 37.0         | 15.9        |
| Node.js     | 51.8         | 43.8        |
| Ruby        | 75.3         | 28.2        |
| Perl        | 72.8         | 7.1         |
| Rust        | 68.7         | 1.9         |
| Java        | 95.5         | 48.1        |
| PHP         | 147.5        | 24.7        |
| Go          | 9.2          | 4.1         |
| Clojure     | 839.3        | 133.8       |
| ColdFusion  | 3,500.0      | 1,100.0     |

### Day 6: Trash Compactor

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C++         | 6.1          | 1.9         |
| Zig         | 6.3          | 1.9         |
| ARM64 asm   | 6.9          | 1.9         |
| C           | 7.0          | 18.0        |
| Go          | 9.5          | 4.4         |
| Rust        | 24.7         | 1.9         |
| Python      | 33.9         | 16.3        |
| Common Lisp | 41.3         | 52.5        |
| Node.js     | 49.3         | 44.7        |
| PHP         | 61.3         | 24.9        |
| Java        | 62.9         | 50.3        |
| Perl        | 69.7         | 14.3        |
| Ruby        | 69.6         | 28.8        |
| Clojure     | 498.7        | 194.4       |
| ColdFusion  | 2,706.7      | 1,111.1     |
| Bash        | 29,648.6     | 3.6         |

### Day 7: Laboratories

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 6.6          | 1.9         |
| ARM64 asm   | 6.7          | 1.9         |
| Rust        | 8.0          | 1.9         |
| C++         | 10.2         | 1.9         |
| Zig         | 10.4         | 1.9         |
| Common Lisp | 30.8         | 42.4        |
| Python      | 30.7         | 15.7        |
| Node.js     | 48.7         | 40.6        |
| Java        | 55.1         | 48.6        |
| Ruby        | 66.3         | 28.7        |
| PHP         | 69.5         | 24.7        |
| Perl        | 72.2         | 12.3        |
| Go          | 83.1         | 26.9        |
| Clojure     | 443.8        | 137.7       |
| ColdFusion  | 2,919.1      | 1,099.2     |
| Bash        | 7,410.8      | 2.4         |

### Day 8: Playground

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Rust        | 39.55        | 33.42       |
| C++         | 57.27        | 21.09       |
| C           | 86.48        | 9.08        |
| ARM64 asm   | 88.67        | 9.03        |
| Zig         | 96.11        | 12.92       |
| Go          | 226.75       | 61.34       |
| Java        | 230.99       | 100.56      |
| Node.js     | 541.04       | 173.38      |
| Clojure     | 1,103.47     | 364.78      |
| Python      | 1,172.98     | 98.31       |
| Bash        | 1,526.76     | 78.47       |
| Common Lisp | 1,556.30     | 99.09       |
| PHP         | 2,118.14     | 169.45      |
| Ruby        | 2,577.36     | 87.59       |
| Perl        | 2,833.25     | 117.23      |
| ColdFusion  | 9,772.06     | 1,872.66    |

### Day 9: Movie Theater

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 13.91        | 1.92        |
| C           | 13.99        | 1.33        |
| ARM64       | 14.36        | 1.50        |
| Rust        | 14.87        | 1.64        |
| C++         | 16.09        | 1.36        |
| Go          | 47.29        | 4.23        |
| Java        | 110.02       | 72.48       |
| Common Lisp | 227.00       | 53.73       |
| Node.js     | 231.00       | 52.52       |
| PHP         | 300.25       | 24.91       |
| Python      | 557.54       | 15.92       |
| Ruby        | 774.62       | 28.78       |
| Clojure     | 1,176.69     | 1,393.48    |
| Perl        | 1,312.77     | 5.25        |
| ColdFusion  | 3,943.18     | 1,034.75    |
| Bash        | 4,781.25     | 1.98        |

### Day 10: Factory

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 38.19        | 1.89        |
| C++         | 44.77        | 2.33        |
| Zig         | 51.50        | 5.19        |
| Java        | 69.15        | 47.86       |
| Rust        | 71.43        | 2.12        |
| ARM64 asm   | 122.33       | 1.89        |
| Common Lisp | 267.90       | 88.98       |
| Node.js     | 593.11       | 118.50      |
| Clojure     | 1,690.80     | 1,366.59    |
| Ruby        | 1,698.00     | 28.55       |
| Go          | 2,276.81     | 10.83       |
| Python      | 4,808.24     | 17.67       |
| PHP         | 6,493.87     | 25.17       |
| Perl        | 9,525.33     | 6.97        |
| ColdFusion  | 10,485.87    | 1,034.81    |
| Bash        | 15,418.12    | 16.05       |

### Day 11: Reactor

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 6.36         | 1.78        |
| ARM64 asm   | 7.00         | 1.92        |
| C           | 7.31         | 1.58        |
| Go          | 7.71         | 4.42        |
| C++         | 8.42         | 1.72        |
| Rust        | 13.32        | 2.08        |
| Python      | 27.33        | 15.16       |
| Common Lisp | 30.30        | 42.30       |
| Perl        | 51.32        | 13.44       |
| Java        | 54.12        | 51.27       |
| Node.js     | 57.23        | 46.91       |
| PHP         | 64.16        | 24.75       |
| Bash        | 115.79       | 7.25        |
| Ruby        | 124.39       | 28.19       |
| Clojure     | 413.22       | 131.09      |
| ColdFusion  | 2,700.00     | 1,140.00    |

### Day 12: Christmas Tree Farm

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Rust        | 5.58         | 1.89        |
| ARM64 asm   | 5.61         | 1.89        |
| Zig         | 5.77         | 1.89        |
| C           | 6.06         | 1.67        |
| C++         | 7.77         | 1.97        |
| Perl        | 11.76        | 5.69        |
| Common Lisp | 26.44        | 42.75       |
| Python      | 27.43        | 15.09       |
| Node.js     | 47.56        | 44.17       |
| Go          | 50.89        | 27.14       |
| PHP         | 51.13        | 25.30       |
| Java        | 56.61        | 53.28       |
| Ruby        | 62.60        | 28.36       |
| Bash        | 117.09       | 6.62        |
| Clojure     | 412.89       | 133.34      |
| ColdFusion  | 2,514.18     | 1,195.98    |
