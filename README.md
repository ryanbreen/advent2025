# Advent of Code 2025

Solutions to [Advent of Code 2025](https://adventofcode.com/2025) implemented in multiple programming languages, solved collaboratively with Claude.

## Approach

This project takes a unique approach to Advent of Code:

1. **Multi-language solutions**: Each day's puzzle is solved in 11 languages (C, C++, Rust, Go, Node.js, Python, Perl, Bash, Clojure, Common Lisp, and Brainfuck) to validate correctness through independent implementations.

2. **Parallel agent solving**: Solutions are developed by independent AI agents working in parallel, each implementing the solution in their assigned language without seeing other implementations. When all agents converge on the same answer, we have high confidence in correctness.

3. **Performance benchmarking**: Every solution is benchmarked for runtime, memory usage, and CPU time, allowing comparison across languages and paradigms.

4. **Automated infrastructure**: A Playwright-based runner handles:
   - Session management (authenticated via GitHub)
   - Problem and input extraction
   - Answer submission

## Progress

| Day | Stars | Languages |
|-----|-------|-----------|
| 1   | ⭐⭐   | C, C++, Rust, Go, Node.js, Python, Perl, Bash, Clojure, Common Lisp, Brainfuck |
| 2   | ⭐⭐   | C, C++, Rust, Go, Node.js, Python, Perl, Bash, Clojure, Common Lisp, Brainfuck |

## Benchmarks

All benchmarks run on Apple Silicon (M-series), measuring wall-clock time, peak memory, and CPU time.

### Day 1: Secret Entrance

| Language | Runtime (s) | Memory (MB) | CPU (s) |
|----------|-------------|-------------|---------|
| C        | 0.00        | 0.94        | 0.00    |
| C++      | 0.00        | 1.05        | 0.00    |
| Rust     | 0.00        | 1.13        | 0.00    |
| Go       | 0.00        | 1.05        | 0.00    |
| Perl     | 0.01        | 3.23        | 0.00    |
| Brainfuck| 0.02        | 7.08        | 0.01    |
| Lisp     | 0.02        | 38.93       | 0.01    |
| Python   | 0.04        | 8.08        | 0.01    |
| Node.js  | 0.04        | 12.99       | 0.03    |
| Bash     | 0.08        | 1.42        | 0.08    |
| Clojure  | 0.45        | 126.84      | 1.22    |

### Day 2: Gift Shop

| Language | Runtime (s) | Memory (MB) | CPU (s) |
|----------|-------------|-------------|---------|
| Rust     | 0.12        | 1.00        | 0.12    |
| C        | 0.14        | 0.94        | 0.14    |
| C++      | 0.17        | 0.94        | 0.17    |
| Go       | 0.19        | 8.08        | 0.19    |
| Node.js  | 0.27        | 59.82       | 0.28    |
| Lisp     | 0.54        | 88.83       | 0.52    |
| Brainfuck| 1.05        | 6.97        | 1.04    |
| Python   | 1.09        | 7.75        | 1.08    |
| Clojure  | 1.16        | 1277.32     | 2.20    |
| Perl     | 2.20        | 2.24        | 2.20    |
| Bash     | 90.93       | 1.52        | 90.52   |

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
    ├── c/solution.c
    ├── cpp/solution.cpp
    ├── rust/src/main.rs
    ├── go/solution.go
    ├── node/solution.js
    ├── python/solution.py
    ├── perl/solution.pl
    ├── bash/solution.sh
    ├── clojure/solution.clj
    ├── lisp/solution.lisp
    └── brainfuck/...
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
gcc -O2 -o solution day01/c/solution.c && ./solution
g++ -std=c++17 -O2 -o solution day01/cpp/solution.cpp && ./solution
cd day01/rust && cargo run --release
cd day01/go && go run solution.go

# Interpreted languages
node day01/node/solution.js
python3 day01/python/solution.py
perl day01/perl/solution.pl
bash day01/bash/solution.sh
clojure -M day01/clojure/solution.clj
sbcl --script day01/lisp/solution.lisp
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
- **Fun**: Solving puzzles in Brainfuck or pure Bash is a entertaining challenge

---

Built with [Claude Code](https://claude.com/claude-code)
