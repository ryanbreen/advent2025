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

All benchmarks run on Apple Silicon (M-series), averaged over multiple runs. Times shown in milliseconds for precision.

### Day 1: Secret Entrance

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| C           | 6.9          | 1.9         |
| C++         | 6.9          | 1.9         |
| Rust        | 7.3          | 1.9         |
| Go          | 8.3          | 4.2         |
| Perl        | 16.1         | 5.4         |
| Lisp        | 27.5         | 39.7        |
| Brainfuck   | 29.2         | 14.8        |
| Python      | 30.8         | 16.1        |
| Node.js     | 50.9         | 41.5        |
| Bash        | 98.6         | 2.2         |
| Clojure     | 491.7        | 150.1       |

### Day 2: Gift Shop

| Language    | Runtime (ms) | Memory (MB) |
|-------------|--------------|-------------|
| Rust        | 136          | 1.9         |
| C++         | 198          | 1.9         |
| C           | 211          | 1.9         |
| Go          | 217          | 10.0        |
| Node.js     | 315          | 87.6        |
| Lisp        | 587          | 89.4        |
| Brainfuck   | 1,131        | 14.8        |
| Python      | 1,192        | 15.6        |
| Clojure     | 1,210        | 1,298       |
| Perl        | 2,461        | 4.3         |
| Bash        | 90,930       | 1.5         |

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
