# Advent of Code

Solutions to [Advent of Code](https://adventofcode.com/) challenges across multiple years, implemented in 16 programming languages, solved collaboratively with Claude.

## Years

| Year | Days | Status |
|------|------|--------|
| [2025](2025/) | 6 | In progress |
| [2024](2024/) | 6 | In progress |
| [2023](2023/) | 3 | In progress |

## Approach

This project takes a unique approach to Advent of Code:

1. **Multi-language solutions**: Each day's puzzle is solved in a minimum of 16 standard languages to validate correctness through independent implementations. Additional languages may be added for variety.

   **Standard 16 (required):** ARM64 Assembly, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion

   **Bonus languages (optional):** Kotlin, Scala, AWK, and others may appear on some days

2. **Parallel agent solving**: Solutions are developed by independent AI agents working in parallel, each implementing the solution in their assigned language without seeing other implementations. When agents converge on the same answer, we have high confidence in correctness.

3. **Performance benchmarking**: Every solution is benchmarked for runtime and memory usage, allowing comparison across languages and paradigms.

4. **Automated infrastructure**: A Playwright-based runner handles:
   - Session management (authenticated via GitHub)
   - Problem and input extraction
   - Answer submission

## Project Structure

```
advent/
├── README.md              # This file
├── CLAUDE.md              # Project guidelines
├── runner/                # AoC automation tools (shared)
│   ├── session.js         # Login/session management
│   ├── extract.js         # Problem & input extraction
│   ├── submit.js          # Answer submission
│   └── benchmark.py       # Performance benchmarking
├── 2023/
│   ├── README.md          # 2023 progress & benchmarks
│   └── dayXX/             # Solutions by day
├── 2024/
│   ├── README.md          # 2024 progress & benchmarks
│   └── dayXX/             # Solutions by day
└── 2025/
    ├── README.md          # 2025 progress & benchmarks
    └── dayXX/             # Solutions by day
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
node runner/extract.js --year 2024 --day 1
node runner/extract.js 2024 1  # Shorthand
```

### Submit an Answer

```bash
node runner/submit.js 2024 1 1 2000468    # Year Day Part Answer
node runner/submit.js --year 2024 --day 1 --part 1 --answer 2000468
```

### Run Solutions

```bash
# From year directories
python3 2024/day01/python/solution.py
cd 2024/day01/c && gcc -O2 -o solution solution.c && ./solution
```

## Philosophy

The multi-language, parallel-agent approach serves several purposes:

- **Verification**: When 3+ independent implementations in different languages produce the same answer, bugs are unlikely
- **Learning**: Seeing the same algorithm expressed in different paradigms (functional Clojure, imperative C, pure Bash) is educational
- **Fun**: Solving puzzles in pure Bash or ARM64 assembly is an entertaining challenge

---

Built with [Claude Code](https://claude.com/claude-code)
