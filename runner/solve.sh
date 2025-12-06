#!/bin/bash
#
# Agentic Advent of Code Solver
#
# Orchestrates Claude Code sessions to solve AoC problems across 16 languages.
#
# Usage:
#   ./solve.sh <year> <day> [--part 1|2] [--step 1|2] [--skip-extract]
#
# Examples:
#   ./solve.sh 2024 7                    # Solve Day 7 (both parts), run both steps
#   ./solve.sh 2024 7 --part 1           # Solve only Part 1
#   ./solve.sh 2024 7 --step 1           # Only generate implementations (skip quality review)
#   ./solve.sh 2024 7 --step 2           # Only run quality review (assumes step 1 done)
#   ./solve.sh 2024 7 --skip-extract     # Skip problem extraction (already have problem.md)
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Default values
YEAR=""
DAY=""
PART=""  # Empty means both parts
STEP=""  # Empty means both steps
SKIP_EXTRACT=false

# Parse arguments
parse_args() {
    if [[ $# -lt 2 ]]; then
        echo -e "${RED}Error: Year and day are required${NC}"
        echo "Usage: ./solve.sh <year> <day> [--part 1|2] [--step 1|2] [--skip-extract]"
        exit 1
    fi

    YEAR="$1"
    DAY="$2"
    shift 2

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --part)
                PART="$2"
                if [[ "$PART" != "1" && "$PART" != "2" ]]; then
                    echo -e "${RED}Error: --part must be 1 or 2${NC}"
                    exit 1
                fi
                shift 2
                ;;
            --step)
                STEP="$2"
                if [[ "$STEP" != "1" && "$STEP" != "2" ]]; then
                    echo -e "${RED}Error: --step must be 1 or 2${NC}"
                    exit 1
                fi
                shift 2
                ;;
            --skip-extract)
                SKIP_EXTRACT=true
                shift
                ;;
            *)
                echo -e "${RED}Error: Unknown option $1${NC}"
                exit 1
                ;;
        esac
    done

    # Pad day with zero if needed
    DAY=$(printf "%02d" "$DAY")
}

# Extract problem if needed
extract_problem() {
    local day_dir="$PROJECT_ROOT/$YEAR/day$DAY"

    if [[ "$SKIP_EXTRACT" == true ]]; then
        echo -e "${YELLOW}Skipping problem extraction (--skip-extract)${NC}"
        return
    fi

    if [[ -f "$day_dir/problem.md" && -f "$day_dir/input.txt" ]]; then
        echo -e "${YELLOW}Problem files already exist. Use --skip-extract or delete to re-extract.${NC}"
        read -p "Re-extract problem? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            return
        fi
    fi

    echo -e "${BLUE}Extracting problem for $YEAR Day $DAY...${NC}"
    node "$SCRIPT_DIR/extract.js" "$YEAR" "${DAY#0}"
}

# Build the Step 1 prompt
build_step1_prompt() {
    local part_instruction=""
    if [[ -n "$PART" ]]; then
        part_instruction="Only solve Part $PART of the problem."
    else
        part_instruction="Solve both Part 1 and Part 2 of the problem."
    fi

    cat <<PROMPT
# Advent of Code $YEAR Day $DAY - Implementation Task

$part_instruction

## Your Mission

You are the orchestrator for solving Advent of Code $YEAR Day $DAY. Your job is to:

1. **Read the problem**: Study \`$YEAR/day$DAY/problem.md\` to understand what needs to be solved.

2. **Implement in 16 languages in parallel**: Dispatch agents (using the Task tool) to implement solutions in ALL 16 required languages simultaneously:
   - ARM64 Assembly, C, C++, Rust, Zig, Go, Java, Node.js, Python, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion

3. **Achieve consensus**: Wait for at least 3 independent implementations to return the same answer before considering it correct. This provides high confidence through independent verification.

4. **Submit the answer**: Once you have consensus on the answer, use the submit script:
   \`node runner/submit.js $YEAR ${DAY#0} <part> <answer>\`

5. **Complete all implementations**: Continue until all 16 languages have working solutions.

6. **Benchmark all solutions**: After implementations are complete, benchmark each solution using:
   \`python3 runner/benchmark.py "<command>" 5\`

7. **Update documentation**: Update \`$YEAR/README.md\` with:
   - Progress table entry for Day $DAY
   - Benchmark results table for Day $DAY

## Agent Dispatch Guidelines

When dispatching implementation agents:
- Use the Task tool with subagent_type="general-purpose"
- Each agent should work on ONE language
- DO NOT provide expected answers to agents - they must derive answers from the algorithm
- Agents should read \`$YEAR/day$DAY/problem.md\` for the problem description
- Agents can reference other implementations for algorithm understanding (after first few complete)
- Each agent must run their solution and report the output they get

## Directory Structure

Solutions go in: \`$YEAR/day$DAY/<language>/solution.<ext>\`

Examples:
- \`$YEAR/day$DAY/python/solution.py\`
- \`$YEAR/day$DAY/node/solution.js\`
- \`$YEAR/day$DAY/c/solution.c\`

Input file is at: \`$YEAR/day$DAY/input.txt\`

## Important Rules from CLAUDE.md

- Solutions must output BOTH Part 1 and Part 2 answers (format: "Part 1: X" / "Part 2: Y")
- Never provide expected outputs to agents - they must compute answers honestly
- Submit answers to AoC after verification with 3+ implementations
- Benchmark every solution and record in README.md

Begin by reading the problem, then dispatch all 16 implementation agents in parallel.
PROMPT
}

# Build the Step 2 prompt (quality review)
build_step2_prompt() {
    cat <<PROMPT
# Advent of Code $YEAR Day $DAY - Quality Review Task

## Your Mission

You are the quality reviewer for the Day $DAY solutions. All 16 language implementations should already exist. Your job is to:

1. **Assess each implementation** on three criteria (scale 1-10):

   a) **Algorithmic Correctness** (1-10): Is the algorithm sound? Are there edge cases missed? Could it fail on different inputs? Is it efficient?

   b) **Honesty/Authenticity** (1-10): Does this implementation genuinely solve the problem in the specified language? Or does it cheat by:
      - Shelling out to other languages
      - Using system calls to run external programs
      - Hardcoding answers
      - Reading from other solutions

   c) **Idiomaticity** (1-10): Would a skilled developer in this language recognize this as native, well-written code? Does it use:
      - Language-appropriate constructs and patterns
      - Standard library features effectively
      - Proper naming conventions
      - Idiomatic error handling
      - Language-specific best practices

2. **Identify improvements**: For any implementation scoring below 7 on any criterion, document specific improvements needed.

3. **Dispatch improvement agents**: For implementations needing work, dispatch agents to fix the issues. Prioritize:
   - Honesty issues (critical - must fix)
   - Algorithmic issues (high priority)
   - Idiomaticity issues (medium priority - improve if time allows)

4. **Re-benchmark if changed**: If any solution is modified, re-run benchmarks and update README.md.

## Assessment Process

For each of the 16 languages, read the solution and evaluate:

\`\`\`
Language: [name]
Path: $YEAR/day$DAY/[lang]/solution.[ext]

Algorithmic Correctness: [1-10]
- [specific observations]

Honesty/Authenticity: [1-10]
- [specific observations]

Idiomaticity: [1-10]
- [specific observations]

Overall: [average]
Action: [PASS / NEEDS_IMPROVEMENT]
Improvements needed: [if any]
\`\`\`

## Languages to Review

1. ARM64 Assembly (\`arm64/solution.s\`)
2. C (\`c/solution.c\`)
3. C++ (\`cpp/solution.cpp\`)
4. Rust (\`rust/solution.rs\`)
5. Zig (\`zig/solution.zig\`)
6. Go (\`go/solution.go\`)
7. Java (\`java/Solution.java\`)
8. Node.js (\`node/solution.js\`)
9. Python (\`python/solution.py\`)
10. Ruby (\`ruby/solution.rb\`)
11. PHP (\`php/solution.php\`)
12. Perl (\`perl/solution.pl\`)
13. Bash (\`bash/solution.sh\`)
14. Clojure (\`clojure/solution.clj\`)
15. Common Lisp (\`lisp/solution.lisp\`)
16. ColdFusion (\`coldfusion/Solution.cfc\`)

## Output Format

After completing all assessments and improvements, provide a summary:

\`\`\`
=== Quality Review Summary for $YEAR Day $DAY ===

| Language     | Correctness | Honesty | Idiomaticity | Action |
|--------------|-------------|---------|--------------|--------|
| ARM64        | X/10        | X/10    | X/10         | PASS/IMPROVED |
| C            | X/10        | X/10    | X/10         | PASS/IMPROVED |
...

Improvements Made: [count]
Agents Dispatched: [count]
\`\`\`

Begin by reading all 16 implementations, then perform assessments.
PROMPT
}

# Run Step 1: Generate implementations and submit
run_step1() {
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${CYAN}  Step 1: Generate Implementations & Submit Answer${NC}"
    echo -e "${CYAN}  Year: $YEAR  Day: $DAY  Part: ${PART:-"both"}${NC}"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo

    local prompt
    prompt=$(build_step1_prompt)

    # Write prompt to temp file for debugging
    local prompt_file="$PROJECT_ROOT/$YEAR/day$DAY/.step1_prompt.md"
    mkdir -p "$PROJECT_ROOT/$YEAR/day$DAY"
    echo "$prompt" > "$prompt_file"
    echo -e "${YELLOW}Prompt saved to: $prompt_file${NC}"

    echo -e "${GREEN}Launching Claude Code session for Step 1...${NC}"
    echo

    # Run Claude with the prompt
    cd "$PROJECT_ROOT"
    echo "$prompt" | claude --dangerously-skip-permissions

    echo
    echo -e "${GREEN}Step 1 complete!${NC}"
}

# Run Step 2: Quality review
run_step2() {
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${CYAN}  Step 2: Quality Review & Improvements${NC}"
    echo -e "${CYAN}  Year: $YEAR  Day: $DAY${NC}"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo

    local day_dir="$PROJECT_ROOT/$YEAR/day$DAY"

    # Check that implementations exist
    if [[ ! -d "$day_dir" ]]; then
        echo -e "${RED}Error: Day directory does not exist: $day_dir${NC}"
        echo "Run Step 1 first to generate implementations."
        exit 1
    fi

    local impl_count
    impl_count=$(find "$day_dir" -type d -mindepth 1 -maxdepth 1 | wc -l | tr -d ' ')

    if [[ "$impl_count" -lt 10 ]]; then
        echo -e "${YELLOW}Warning: Only $impl_count language directories found. Expected 16+.${NC}"
        read -p "Continue anyway? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi

    local prompt
    prompt=$(build_step2_prompt)

    # Write prompt to temp file for debugging
    local prompt_file="$day_dir/.step2_prompt.md"
    echo "$prompt" > "$prompt_file"
    echo -e "${YELLOW}Prompt saved to: $prompt_file${NC}"

    echo -e "${GREEN}Launching Claude Code session for Step 2...${NC}"
    echo

    # Run Claude with the prompt
    cd "$PROJECT_ROOT"
    echo "$prompt" | claude --dangerously-skip-permissions

    echo
    echo -e "${GREEN}Step 2 complete!${NC}"
}

# Main
main() {
    parse_args "$@"

    echo -e "${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║          Agentic Advent of Code Solver                    ║${NC}"
    echo -e "${BLUE}║                                                           ║${NC}"
    echo -e "${BLUE}║  Year: $YEAR    Day: $DAY                                     ║${NC}"
    echo -e "${BLUE}║  Part: ${PART:-"both"}     Step: ${STEP:-"both"}                                   ║${NC}"
    echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"
    echo

    # Extract problem if running step 1
    if [[ -z "$STEP" || "$STEP" == "1" ]]; then
        extract_problem
    fi

    # Run requested steps
    if [[ -z "$STEP" ]]; then
        # Both steps
        run_step1
        echo
        echo -e "${YELLOW}Pausing before Step 2. Press Enter to continue or Ctrl+C to stop.${NC}"
        read -r
        run_step2
    elif [[ "$STEP" == "1" ]]; then
        run_step1
    else
        run_step2
    fi

    echo
    echo -e "${GREEN}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║                    All steps complete!                    ║${NC}"
    echo -e "${GREEN}╚═══════════════════════════════════════════════════════════╝${NC}"
}

main "$@"
