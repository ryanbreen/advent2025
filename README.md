# Advent of Code 2025

Solutions to [Advent of Code 2025](https://adventofcode.com/2025) implemented in multiple programming languages, solved collaboratively with Claude.

## Approach

This project takes a unique approach to Advent of Code:

1. **Multi-language solutions**: Each day's puzzle is solved in multiple languages (Node.js, Python, Clojure, C, Rust, Bash, Perl, and sometimes esoteric languages like Brainfuck) to validate correctness through independent implementations.

2. **Parallel agent solving**: Solutions are developed by independent AI agents working in parallel, each implementing the solution in their assigned language without seeing other implementations. When all agents converge on the same answer, we have high confidence in correctness.

3. **Automated infrastructure**: A Playwright-based runner handles:
   - Session management (authenticated via GitHub)
   - Problem and input extraction
   - Answer submission

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
    ├── node/solution.js   # Node.js solution
    ├── python/solution.py # Python solution
    ├── clojure/solution.clj
    ├── c/solution.c
    ├── rust/src/main.rs
    ├── bash/solution.sh
    ├── perl/solution.pl
    └── ...
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
# Node.js
node day01/node/solution.js

# Python
python day01/python/solution.py

# Clojure
clj day01/clojure/solution.clj

# C
gcc -o day01/c/solution day01/c/solution.c && ./day01/c/solution

# Rust
cd day01/rust && cargo run --release

# Bash
bash day01/bash/solution.sh

# Perl
perl day01/perl/solution.pl
```

### Submit an Answer

```bash
node runner/submit.js <day> <part> <answer>
# e.g., node runner/submit.js 1 1 1150
```

## Progress

| Day | Stars | Languages |
|-----|-------|-----------|
| 1   | ⭐⭐   | Node.js, Python, Clojure, C, Rust, Bash, Perl, Brainfuck |
| 2   | ⭐⭐   | Node.js, Python, Clojure, C, Rust, Bash, Perl, Brainfuck |

## Philosophy

The multi-language, parallel-agent approach serves several purposes:

- **Verification**: When 3+ independent implementations in different languages produce the same answer, bugs are unlikely
- **Learning**: Seeing the same algorithm expressed in different paradigms (functional Clojure, imperative C, pure Bash) is educational
- **Fun**: Solving puzzles in Brainfuck or pure Bash is a entertaining challenge

---

Built with [Claude Code](https://claude.com/claude-code)
