# Advent of Code 2025

Solutions to [Advent of Code 2025](https://adventofcode.com/2025) implemented in multiple programming languages, solved collaboratively with Claude.

## Approach

This project takes a unique approach to Advent of Code:

1. **Multi-language solutions**: Each day's puzzle is solved in 16 languages (ARM64 Assembly, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, and ColdFusion) to validate correctness through independent implementations.

2. **Parallel agent solving**: Solutions are developed by independent AI agents working in parallel, each implementing the solution in their assigned language without seeing other implementations. When all agents converge on the same answer, we have high confidence in correctness.

3. **Performance benchmarking**: Every solution is benchmarked for runtime, memory usage, and CPU time, allowing comparison across languages and paradigms.

4. **Automated infrastructure**: A Playwright-based runner handles:
   - Session management (authenticated via GitHub)
   - Problem and input extraction
   - Answer submission

## Progress

| Day | Stars | Languages |
|-----|-------|-----------|
| 1   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 2   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |
| 3   | ⭐⭐   | ARM64, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion |

## Benchmarks

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs. Times shown in milliseconds for precision.

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
| Clojure     | 491.7        | 150.1       |
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

## Project Structure

```
advent2025/
├── README.md
├── CLAUDE.md              # Project guidelines for Claude
├── runner/                # AoC automation tools
│   ├── session.js         # Login/session management
│   ├── extract.js         # Problem & input extraction
│   └── submit.js          # Answer submission
└── dayXX/
    ├── problem.md         # Extracted problem statement
    ├── input.txt          # Puzzle input
    ├── arm64/solution.s   # ARM64 assembly (macOS)
    ├── c/solution.c
    ├── cpp/solution.cpp
    ├── rust/src/main.rs
    ├── zig/solution.zig
    ├── go/solution.go
    ├── java/Solution.java
    ├── node/solution.js
    ├── python/solution.py
    ├── ruby/solution.rb
    ├── php/solution.php
    ├── perl/solution.pl
    ├── bash/solution.sh
    ├── clojure/solution.clj
    ├── lisp/solution.lisp
    └── coldfusion/solution.cfm
```

## Running Solutions

### Setup

```bash
cd runner
npm install
npx playwright install chromium

# Login to AoC (opens browser for GitHub auth)
node session.js login
```

### Extract a Day's Problem

```bash
node runner/extract.js --day 1
```

### Run Solutions

```bash
# Compiled languages (build first)
cd day01/arm64 && make && ./solution    # ARM64 assembly (macOS only)
gcc -O2 -o solution day01/c/solution.c && ./solution
g++ -std=c++17 -O2 -o solution day01/cpp/solution.cpp && ./solution
cd day01/rust && cargo run --release
cd day01/zig && zig build-exe solution.zig -O ReleaseFast -femit-bin=solution && ./solution
cd day01/go && go run solution.go
cd day01/java && javac Solution.java && java Solution

# Interpreted languages
node day01/node/solution.js
python3 day01/python/solution.py
ruby day01/ruby/solution.rb
perl day01/perl/solution.pl
bash day01/bash/solution.sh
clojure -M day01/clojure/solution.clj
sbcl --script day01/lisp/solution.lisp
box day01/coldfusion/solution.cfm  # Requires CommandBox
```

### Submit an Answer

```bash
node runner/submit.js <day> <part> <answer>
# e.g., node runner/submit.js 1 1 1150
```

## Philosophy

The multi-language, parallel-agent approach serves several purposes:

- **Verification**: When 3+ independent implementations in different languages produce the same answer, bugs are unlikely
- **Learning**: Seeing the same algorithm expressed in different paradigms (functional Clojure, imperative C, pure Bash) is educational
- **Fun**: Solving puzzles in pure Bash or ARM64 assembly is an entertaining challenge

---

Built with [Claude Code](https://claude.com/claude-code)
