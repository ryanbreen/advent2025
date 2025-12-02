# Advent of Code 2025 - Project Guidelines

## Overview
This project solves Advent of Code 2025 challenges in both Node.js and Python, with automated problem extraction via Playwright.

## Project Structure
```
advent2025/
├── CLAUDE.md
├── runner/                    # AoC runner utilities
│   ├── package.json
│   ├── session.js            # Playwright session management
│   ├── extract.js            # Problem/input extraction
│   └── auth-state.json       # Persisted browser session (gitignored)
├── day01/
│   ├── problem.md            # Extracted problem statement (both parts)
│   ├── input.txt             # Puzzle input for this account
│   ├── node/
│   │   ├── solution.js       # Node.js solution
│   │   └── package.json
│   └── python/
│       └── solution.py       # Python solution
├── day02/
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
Read day01/problem.md for the problem description.
Reference day01/python/solution.py for the algorithm.
Run your solution and report the output.
```

## IMPORTANT: Benchmarking Requirements

Every solution must be benchmarked after implementation. When an agent completes a solution, they are responsible for:

1. **Running the benchmark** using `/usr/bin/time -l` (macOS) or equivalent
2. **Recording metrics** in the README.md benchmark tables
3. **Metrics to capture**:
   - **Runtime**: Total wall-clock time (real time in seconds)
   - **Memory**: Peak memory footprint (in MB)
   - **CPU**: User + System time (in seconds)

**Benchmark command format:**

Use the high-precision benchmark script for accurate millisecond timing:
```bash
# From the project root - runs command 3-5 times and averages
python3 runner/benchmark.py "cd day01/c && ./solution" 5
python3 runner/benchmark.py "cd day01/python && python3 solution.py" 5
```

The script outputs:
- Average runtime in milliseconds
- Standard deviation
- Min/Max times
- Peak memory usage

**Updating README.md:**
After benchmarking, update the appropriate table in README.md with the results. Each day has a benchmark table organized by language showing Part 1+2 combined performance.

**Example benchmark table format:**
```markdown
| Language | Runtime (s) | Memory (MB) | CPU (s) |
|----------|-------------|-------------|---------|
| C        | 0.02        | 1.2         | 0.01    |
| Rust     | 0.03        | 2.1         | 0.02    |
| Python   | 0.45        | 12.3        | 0.44    |
```

Solutions should be run from their respective directories with the input file at `../input.txt`.

## Playwright Session Management
- Session state stored in `runner/auth-state.json`
- User manually logs in via GitHub on first run
- Session is reused for all subsequent problem/input fetching
- Never commit auth-state.json (add to .gitignore)

## Workflow
1. Launch Playwright browser for login (if needed)
2. Extract problem statement to `dayXX/problem.md`
3. Extract input to `dayXX/input.txt`
4. Implement solution in both `node/` and `python/`
5. Run and verify answers match

## Commands
```bash
# Start login session (opens browser for manual GitHub auth)
node runner/session.js login

# Extract problem for a specific day
node runner/extract.js --day 1

# Run Node solution
node day01/node/solution.js

# Run Python solution
python day01/python/solution.py
```
