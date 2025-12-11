# Advent of Code 2024

Solutions implemented in 16 languages.

## Progress

| Day | Stars | Languages |
|-----|-------|-----------|
| 1   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 2   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 3   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 4   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion + Kotlin, Scala, AWK |
| 5   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 6   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 7   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 8   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |

## Benchmarks

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs.

### Day 1: Historian Hysteria

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 6.7          | 1.9         |
| ARM64 asm   | 6.6          | 1.9         |
| Rust        | 7.2          | 2.1         |
| C++         | 7.3          | 1.9         |
| Go          | 7.7          | 4.2         |
| Perl        | 14.5         | 5.1         |
| Zig         | 16.6         | 1.9         |
| Python      | 28.9         | 15.9        |
| Lisp        | 40.0         | 38.3        |
| Node.js     | 45.5         | 42.2        |
| Java        | 59.1         | 51.5        |
| Ruby        | 97.5         | 28.3        |
| PHP         | 110.5        | 24.8        |
| Clojure     | 405.4        | 129.2       |
| Bash        | 3,381.8      | 2.1         |
| ColdFusion  | 4,654.0      | 1,012.9     |

### Day 2: Red-Nosed Reports

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 5.7          | 1.9         |
| C           | 5.8          | 1.9         |
| Go          | 8.1          | 4.7         |
| C++         | 8.6          | 1.9         |
| Rust        | 9.5          | 1.9         |
| Lisp        | 23.6         | 39.2        |
| Perl        | 29.7         | 6.6         |
| Python      | 35.1         | 15.6        |
| Zig         | 35.6         | 2.0         |
| Node.js     | 50.1         | 44.3        |
| Java        | 59.0         | 55.2        |
| PHP         | 69.0         | 24.6        |
| Ruby        | 71.7         | 28.4        |
| Clojure     | 432.0        | 150.0       |
| Bash        | 932.0        | 4.8         |
| ColdFusion  | 2,705.6      | 1,149.0     |

### Day 3: Mull It Over

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 6.5          | 1.3         |
| Zig         | 6.5          | 1.9         |
| C           | 7.4          | 1.9         |
| Rust        | 7.8          | 1.9         |
| Go          | 8.2          | 4.7         |
| C++         | 10.8         | 1.9         |
| Perl        | 17.6         | 6.4         |
| Lisp        | 27.9         | 39.9        |
| Python      | 29.4         | 15.9        |
| Node.js     | 46.2         | 39.6        |
| PHP         | 60.8         | 25.5        |
| Ruby        | 61.8         | 28.3        |
| Bash        | 78.6         | 1.9         |
| Java        | 335.6        | 127.0       |
| Clojure     | 424.7        | 127.7       |
| ColdFusion  | 2,726.8      | 1,135.2     |

### Day 4: Ceres Search

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Zig         | 6.1          | 1.9         |
| C++         | 6.5          | 1.9         |
| ARM64 asm   | 6.8          | 1.9         |
| C           | 6.9          | 1.9         |
| Rust        | 7.4          | 1.9         |
| Node.js     | 57.7         | 48.5        |
| Python      | 60.7         | 15.6        |
| Java        | 68.8         | 47.4        |
| Go          | 71.9         | 26.7        |
| PHP         | 73.5         | 24.5        |
| Perl        | 75.1         | 5.7         |
| Kotlin      | 84.3         | 55.8        |
| Lisp        | 99.6         | 40.1        |
| Ruby        | 157.9        | 28.3        |
| AWK         | 227.6        | 3.7         |
| Clojure     | 494.7        | 213.9       |
| Scala       | 1,765.9      | 266.1       |
| ColdFusion  | 2,772.2      | 1,119.8     |
| Bash        | 23,480.7     | 2.1         |

### Day 5: Print Queue

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Go          | 4.9          | 1.9         |
| Zig         | 7.2          | 1.9         |
| C++         | 7.9          | 1.9         |
| Rust        | 8.5          | 1.9         |
| C           | 16.0         | 1.9         |
| Perl        | 17.5         | 5.0         |
| ARM64 asm   | 22.6         | 1.9         |
| Lisp        | 28.5         | 42.6        |
| Python      | 32.1         | 15.8        |
| Node.js     | 49.8         | 44.7        |
| PHP         | 63.2         | 24.8        |
| Java        | 65.1         | 51.5        |
| Ruby        | 68.4         | 29.0        |
| Clojure     | 421.8        | 145.8       |
| ColdFusion  | 2,602.9      | 1,122.0     |
| Bash        | 78,992.4     | 3.8         |

### Day 6: Guard Gallivant

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| ARM64 asm   | 4.68         | 1.88        |
| C           | 19.61        | 7.98        |
| Python      | 31.14        | 16.67       |
| Perl        | 32.41        | 41.19       |
| Common Lisp | 44.16        | 18.53       |
| Node.js     | 49.55        | 46.86       |
| PHP         | 61.44        | 26.56       |
| C++         | 69.34        | 45.45       |
| Ruby        | 70.04        | 30.09       |
| Go          | 71.19        | 27.31       |
| Rust        | 255.18       | 109.81      |
| Java        | 358.15       | 114.05      |
| Clojure     | 437.48       | 140.64      |
| Zig         | 3,279.09     | 231.64      |
| Bash        | 3,635.02     | 4.52        |
| ColdFusion  | 4.68         | 1.88        |

### Day 7: Bridge Repair

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 352          | 1.9         |
| Zig         | 363          | 1.9         |
| ARM64 asm   | 473          | 1.9         |
| Rust        | 476          | 1.9         |
| C++         | 579          | 1.9         |
| Java        | 1,486        | 1,274       |
| Node.js     | 3,094        | 65          |
| Go          | 3,416        | 85          |
| Common Lisp | 4,060        | 130         |
| PHP         | 6,063        | 117         |
| Python      | 9,385        | 16          |
| Ruby        | 11,487       | 28          |
| Clojure     | 17,571       | 1,320       |
| Perl        | 24,006       | 196         |
| ColdFusion  | 42,807       | 1,304       |
| Bash        | >2 hrs*      | -           |

*Bash solution is correct but impractically slow for Part 2 (3^n operator combinations)

### Day 8: Resonant Collinearity

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Go          | 6.6          | 4.1         |
| C++         | 6.6          | 1.9         |
| C           | 7.3          | 1.9         |
| ARM64 asm   | 7.3          | 1.9         |
| Rust        | 8.5          | 1.9         |
| Zig         | 11.2         | 1.9         |
| Perl        | 13.3         | 4.6         |
| Python      | 26.8         | 15.0        |
| Common Lisp | 27.7         | 41.0        |
| Node.js     | 46.9         | 39.8        |
| Bash        | 51.9         | 6.8         |
| PHP         | 62.2         | 24.6        |
| Ruby        | 64.5         | 28.3        |
| Java        | 138.4        | 46.5        |
| Clojure     | 465.9        | 142.0       |
| ColdFusion  | 2,930.5      | 1,143.8     |
