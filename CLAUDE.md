# Advent of Code - Project Guidelines

## Overview
This project solves Advent of Code challenges across multiple years in 16 languages, with automated problem extraction via Playwright.

## Standard Language Set (Required)

Every day's solution **MUST** be implemented in these 16 languages:

1. **ARM64 Assembly** - Low-level, macOS ARM64
2. **C** - Systems programming
3. **C++** - Systems programming with abstractions
4. **Rust** - Memory-safe systems programming
5. **Zig** - Modern systems programming
6. **Go** - Compiled, garbage-collected
7. **Java** - JVM, object-oriented
8. **Node.js** - JavaScript runtime
9. **Python** - Scripting, rapid prototyping
10. **Ruby** - Scripting, expressive syntax
11. **PHP** - Scripting
12. **Perl** - Text processing, scripting
13. **Bash** - Shell scripting
14. **Clojure** - JVM, functional Lisp
15. **Common Lisp** - Classic Lisp
16. **ColdFusion** - CFML scripting

**Additional languages** (Kotlin, Scala, AWK, etc.) may be added for variety, but the 16 above are the minimum required set for every day.

## Project Structure
```
advent/
├── CLAUDE.md
├── README.md
├── runner/                    # AoC runner utilities (shared)
│   ├── package.json
│   ├── session.js            # Playwright session management
│   ├── extract.js            # Problem/input extraction
│   ├── benchmark.py          # Benchmarking script
│   └── auth-state.json       # Persisted browser session (gitignored)
├── 2024/
│   ├── day01/
│   │   ├── problem.md
│   │   ├── input.txt
│   │   ├── node/solution.js
│   │   ├── python/solution.py
│   │   └── ... (other languages)
│   ├── day02/
│   └── ...
├── 2025/
│   ├── day01/
│   ├── day02/
│   └── ...
└── ...
```

## Coding Standards

### Node.js
- Use ES module syntax (`import`/`export`)
- Use top-level `async`/`await` - no raw Promise chains
- File extension: `.js` with `"type": "module"` in package.json

### Python
- Python 3.10+ assumed
- Use type hints where practical
- Standard library preferred; external deps only when necessary

## Advent of Code Structure
Each day has two parts:
1. **Part 1**: Solve to unlock Part 2
2. **Part 2**: Complete to finish the day

Both parts typically use the same input but ask different questions.

## IMPORTANT: Preserving Part 1 When Implementing Part 2

**DO NOT delete or modify Part 1 code when adding Part 2.**

Each solution file must be able to run BOTH parts independently. When implementing Part 2:
- Keep the `part1()` function intact and working
- Add a separate `part2()` function for the new logic
- The solution should output both answers when run

Example structure:
```javascript
// Part 1
function part1() {
  // Original Part 1 logic - DO NOT MODIFY
  return answer1;
}

// Part 2
function part2() {
  // New Part 2 logic
  return answer2;
}

console.log('Part 1:', part1());
console.log('Part 2:', part2());
```

This ensures we can always verify both parts of any day's solution at any time.

## IMPORTANT: Agent Tasking Guidelines

When dispatching agents to implement solutions in new languages:

**DO NOT provide expected outputs or answers to the agent.**

Agents must work honestly from the problem description and algorithm logic alone. Providing expected answers creates the risk of:
- Agents gaming their output to match expected values
- Hardcoded answers instead of genuine implementations
- False confidence that a solution is correct

**Correct approach:**
- Point agents to the `problem.md` file for the problem description
- Reference existing implementations for algorithm logic
- Let agents run their solution and report what output they get
- Verify correctness yourself by comparing against known-good implementations

**Example - DO NOT do this:**
```
Implement Day 1 in Go.
Expected output: Part 1: 1150, Part 2: 6738
```

**Example - DO this:**
```
Implement Day 1 in Go.
Read 2025/day01/problem.md for the problem description.
Reference 2025/day01/python/solution.py for the algorithm.
Run your solution and report the output.
```

## Answer Submission with Multi-Agent Approach

When running multiple agents in parallel to implement solutions across languages, **submit the answer once three independent implementations agree on the same output**. This provides high confidence in correctness through independent verification.

Once three agents report matching answers, it's safe to submit without waiting for all implementations to complete. Continue running remaining agents in the background to fill out the full language set.

## IMPORTANT: Benchmarking Requirements

Every solution must be benchmarked after implementation. When an agent completes a solution, they are responsible for:

1. **Running the benchmark** using the benchmark script
2. **Recording metrics** in the README.md benchmark tables
3. **Metrics to capture**:
   - **Runtime**: Total wall-clock time (in milliseconds)
   - **Memory**: Peak memory footprint (in MB)

**Benchmark command format:**

Use the high-precision benchmark script for accurate millisecond timing:
```bash
# From the project root - runs command 3-5 times and averages
python3 runner/benchmark.py "cd 2025/day01/c && ./solution" 5
python3 runner/benchmark.py "cd 2025/day01/python && python3 solution.py" 5
```

The script outputs:
- Average runtime in milliseconds
- Standard deviation
- Min/Max times
- Peak memory usage

**Updating README.md:**
After benchmarking, update the appropriate table in README.md with the results. Each day has a benchmark table organized by language showing Part 1+2 combined performance.

Solutions should be run from their respective directories with the input file at `../input.txt`.

## Playwright Session Management
- Session state stored in `runner/auth-state.json`
- User manually logs in via GitHub on first run
- Session is reused for all subsequent problem/input fetching
- Never commit auth-state.json (add to .gitignore)

## Workflow
1. Launch Playwright browser for login (if needed)
2. Extract problem statement to `<year>/dayXX/problem.md`
3. Extract input to `<year>/dayXX/input.txt`
4. Implement solution in Python and Node.js first
5. Run and verify answers match across implementations
6. **SUBMIT ANSWERS** to Advent of Code (see Answer Submission below)
7. Dispatch agents to implement remaining 14 languages in parallel
8. Run benchmarks and update README.md

## IMPORTANT: Answer Submission

**ALWAYS submit answers to Advent of Code after verifying correctness.**

This is a critical step that must not be skipped. Use the submit script:

```bash
# Submit answer for a specific day/part
node runner/submit.js <year> <day> <part> <answer>

# Examples:
node runner/submit.js 2024 3 1 175700056
node runner/submit.js 2024 3 2 71668682
```

**Submission workflow:**
1. Implement Part 1, verify answer with 2+ implementations
2. Submit Part 1 answer immediately
3. Implement Part 2, verify answer with 2+ implementations
4. Submit Part 2 answer immediately
5. Then proceed with remaining language implementations

Note: AoC has rate limiting (~5 seconds between submissions). If you get a rate limit error, wait and retry.

**IMPORTANT: Re-submitting already-solved answers returns "INCORRECT"**

When you submit an answer for a puzzle that has ALREADY been solved correctly, AoC does NOT say "you already solved this" - it returns "INCORRECT" or "That's not the right answer". This is confusing but expected behavior.

Before assuming an answer is wrong:
1. Check the AoC page directly (use Playwright with `headless: false` to visually inspect)
2. Look for "Your puzzle answer was <code>X</code>" on the puzzle page
3. If the answer is shown on the page, the puzzle is already solved - don't keep re-submitting

## Commands
```bash
# Start login session (opens browser for manual GitHub auth)
node runner/session.js login

# Extract problem for a specific day and year
node runner/extract.js --year 2024 --day 1
node runner/extract.js 2024 1  # Shorthand

# Extract for current year
node runner/extract.js --day 1

# Run solutions (from year directory)
node 2025/day01/node/solution.js
python 2025/day01/python/solution.py
```
