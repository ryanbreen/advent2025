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
