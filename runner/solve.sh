#!/bin/bash
#
# Agentic Advent of Code Solver - Multi-Model Architecture
#
# Architecture:
#   - Bash: Orchestrates phases, manages state files, handles I/O
#   - Haiku (Coordinator): Dispatches implementers, tracks progress, handles escalation
#   - Sonnet (Implementer): Implements solutions in each language (default)
#   - Opus (Reviewer): Reviews all implementations for quality, can fix critical issues
#
# State is persisted to allow resuming interrupted runs.
#
# Usage:
#   ./solve.sh <year> <day> [--part 1|2] [--step 1|2|3] [--resume] [--skip-extract]
#
# Steps:
#   1. Implementation: Haiku coordinates Sonnet agents to implement all languages
#   2. Review: Opus reviews all implementations for quality
#   3. Improvement: Sonnet fixes issues identified by Opus (Opus for stuck cases)
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# Standard languages
LANGUAGES=(
    "arm64:ARM64 Assembly:solution.s"
    "c:C:solution.c"
    "cpp:C++:solution.cpp"
    "rust:Rust:solution.rs"
    "zig:Zig:solution.zig"
    "go:Go:solution.go"
    "java:Java:Solution.java"
    "node:Node.js:solution.js"
    "python:Python:solution.py"
    "ruby:Ruby:solution.rb"
    "php:PHP:solution.php"
    "perl:Perl:solution.pl"
    "bash:Bash:solution.sh"
    "clojure:Clojure:solution.clj"
    "lisp:Common Lisp:solution.lisp"
    "cfml:ColdFusion:solution.cfm"
)

# Defaults
YEAR=""
DAY=""
PART=""
STEP=""
RESUME=false
SKIP_EXTRACT=false

# Parse arguments
parse_args() {
    if [[ $# -lt 2 ]]; then
        echo -e "${RED}Error: Year and day required${NC}"
        echo "Usage: ./solve.sh <year> <day> [--part 1|2] [--step 1|2|3] [--resume] [--skip-extract]"
        exit 1
    fi

    YEAR="$1"
    DAY="$2"
    shift 2

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --part) PART="$2"; shift 2 ;;
            --step) STEP="$2"; shift 2 ;;
            --resume) RESUME=true; shift ;;
            --skip-extract) SKIP_EXTRACT=true; shift ;;
            *) echo -e "${RED}Unknown option: $1${NC}"; exit 1 ;;
        esac
    done

    DAY=$(printf "%02d" "$DAY")
}

# Initialize state directory and files
init_state() {
    local day_dir="$PROJECT_ROOT/$YEAR/day$DAY"
    local state_dir="$day_dir/.state"

    mkdir -p "$state_dir"

    # Initialize state file if not resuming or doesn't exist
    if [[ "$RESUME" != true ]] || [[ ! -f "$state_dir/solve.json" ]]; then
        cat > "$state_dir/solve.json" << EOF
{
    "year": "$YEAR",
    "day": "$DAY",
    "phase": "init",
    "consensus_answer_p1": null,
    "consensus_answer_p2": null,
    "submitted_p1": false,
    "submitted_p2": false,
    "languages": {}
}
EOF
        # Initialize per-language state
        for lang_info in "${LANGUAGES[@]}"; do
            IFS=':' read -r lang_id lang_name lang_file <<< "$lang_info"
            cat > "$state_dir/${lang_id}.json" << EOF
{
    "id": "$lang_id",
    "name": "$lang_name",
    "file": "$lang_file",
    "status": "pending",
    "attempts": 0,
    "model": "sonnet",
    "output_p1": null,
    "output_p2": null,
    "error": null,
    "review_score": null,
    "review_notes": null
}
EOF
        done
    fi

    echo "$state_dir"
}

# Extract problem
extract_problem() {
    local day_dir="$PROJECT_ROOT/$YEAR/day$DAY"

    if [[ "$SKIP_EXTRACT" == true ]]; then
        echo -e "${YELLOW}Skipping extraction (--skip-extract)${NC}"
        return
    fi

    if [[ -f "$day_dir/problem.md" && -f "$day_dir/input.txt" ]]; then
        echo -e "${YELLOW}Problem files exist. Re-extract? (y/N)${NC}"
        read -r -n 1 reply
        echo
        [[ ! $reply =~ ^[Yy]$ ]] && return
    fi

    echo -e "${BLUE}Extracting problem...${NC}"
    node "$SCRIPT_DIR/extract.js" "$YEAR" "${DAY#0}"
}

# Build the Haiku coordinator prompt
build_coordinator_prompt() {
    local state_dir="$1"
    local day_dir="$PROJECT_ROOT/$YEAR/day$DAY"

    cat << 'PROMPT'
# Advent of Code Implementation Coordinator

You are a COORDINATOR agent (running on Haiku) responsible for orchestrating the implementation of an Advent of Code solution across 16 programming languages.

## Your Role
- Dispatch Sonnet agents to implement solutions in parallel
- Track progress and collect results
- Handle failures with retries (escalate to Opus after 2 failures)
- Achieve consensus (3+ matching answers) before submitting
- Update state files to enable resume capability

## Critical Rules
1. Use the Task tool with `model: "sonnet"` for implementation agents (default)
2. After 2 failed attempts for a language, escalate: `model: "opus"`
3. DO NOT provide expected answers to implementation agents
4. Submit answer via `node runner/submit.js` once 3+ implementations agree
5. Update state files after each significant event

## State Management
PROMPT

    echo "State directory: $state_dir"
    echo "Current state:"
    cat "$state_dir/solve.json"
    echo ""
    echo "Language states:"
    for f in "$state_dir"/*.json; do
        [[ "$(basename "$f")" == "solve.json" ]] && continue
        echo "--- $(basename "$f") ---"
        cat "$f"
    done

    cat << PROMPT

## Problem Location
- Problem: $YEAR/day$DAY/problem.md
- Input: $YEAR/day$DAY/input.txt

## Languages to Implement
PROMPT

    for lang_info in "${LANGUAGES[@]}"; do
        IFS=':' read -r lang_id lang_name lang_file <<< "$lang_info"
        echo "- $lang_name ($lang_id): $YEAR/day$DAY/$lang_id/$lang_file"
    done

    cat << 'PROMPT'

## Implementation Agent Prompt Template
When dispatching an implementation agent, use this prompt structure:

```
Implement the Advent of Code solution in [LANGUAGE].

Problem: Read YEAR/dayDD/problem.md
Input: YEAR/dayDD/input.txt
Output to: YEAR/dayDD/LANG_ID/FILENAME

Requirements:
1. Read and parse the input file
2. Implement both Part 1 and Part 2
3. Output format: "Part 1: X" and "Part 2: Y" on separate lines
4. Run your solution and report the actual output
5. Handle edge cases properly

DO NOT guess answers. Compute them from the algorithm.
```

## Workflow
1. Read problem.md to understand the challenge
2. Check state files for any completed implementations (if resuming)
3. Dispatch implementation agents for pending languages (Sonnet by default)
4. Collect results, update state files
5. If a language fails twice, redispatch with Opus
6. Once 3+ agree on answers, submit via runner/submit.js
7. Continue until all 16 languages complete
8. Run benchmarks: `python3 runner/benchmark.py "command" 5`
9. Update YEAR/README.md with progress and benchmarks

## Escalation to Sonnet for Yourself
If coordination becomes complex (many failures, edge cases), you can escalate your own work to Sonnet by noting it in your response. The bash wrapper will handle re-invoking with a higher-capability model.

Begin by reading the problem and checking current state.
PROMPT
}

# Build the Opus reviewer prompt
build_reviewer_prompt() {
    local state_dir="$1"
    local day_dir="$PROJECT_ROOT/$YEAR/day$DAY"

    cat << 'PROMPT'
# Advent of Code Quality Reviewer

You are a REVIEWER agent (running on Opus) responsible for assessing the quality of all implementations and ensuring they meet high standards.

## Your Role
- Review ALL 16 language implementations
- Assess each on three criteria (1-10 scale):
  1. **Algorithmic Correctness**: Sound algorithm, handles edge cases, efficient
  2. **Honesty/Authenticity**: Genuinely solves problem in that language (no cheating)
  3. **Idiomaticity**: Uses language-appropriate patterns, conventions, best practices
- Dispatch Sonnet agents to fix issues (Opus for stuck cases after 2 Sonnet failures)
- Re-benchmark any modified solutions

## Critical Rules
1. Be rigorous - we want genuinely excellent implementations
2. Flag any solution that shells out to other languages or hardcodes answers
3. Dispatch Sonnet for fixes by default, escalate to Opus only if Sonnet fails twice
4. Update state files with review scores and notes
5. Re-run benchmarks for any modified solution

## Assessment Template
For each language:
```
Language: [name]
Path: YEAR/dayDD/[lang]/solution.[ext]

Algorithmic Correctness: [1-10]
- [observations]

Honesty/Authenticity: [1-10]
- [observations]

Idiomaticity: [1-10]
- [observations]

Action: PASS / NEEDS_FIX
Fix needed: [description if applicable]
```

## State Location
PROMPT

    echo "State directory: $state_dir"
    echo ""
    echo "Language states:"
    for f in "$state_dir"/*.json; do
        [[ "$(basename "$f")" == "solve.json" ]] && continue
        echo "--- $(basename "$f") ---"
        cat "$f"
    done

    cat << PROMPT

## Implementations to Review
All solutions are in: $YEAR/day$DAY/[language]/

## Workflow
1. Read each implementation file
2. Assess against all three criteria
3. For any scoring < 7 on any criterion, dispatch fix agent
4. Track fixes in state files
5. After fixes, re-verify and re-benchmark
6. Update $YEAR/README.md with final benchmarks
7. Produce final summary report

Begin by reviewing the first implementation.
PROMPT
}

# Run Phase 1: Implementation (Haiku coordinator with Sonnet implementers)
run_implementation_phase() {
    local state_dir="$1"

    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${CYAN}  Phase 1: Implementation${NC}"
    echo -e "${CYAN}  Coordinator: Haiku | Implementers: Sonnet (→Opus on fail)${NC}"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo

    local prompt
    prompt=$(build_coordinator_prompt "$state_dir")

    # Save prompt for debugging
    echo "$prompt" > "$state_dir/coordinator_prompt.md"

    echo -e "${GREEN}Launching Haiku coordinator...${NC}"
    cd "$PROJECT_ROOT"
    echo "$prompt" | claude --model haiku --dangerously-skip-permissions

    echo -e "${GREEN}Phase 1 complete${NC}"
}

# Run Phase 2: Review (Opus reviewer)
run_review_phase() {
    local state_dir="$1"

    echo -e "${MAGENTA}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${MAGENTA}  Phase 2: Quality Review${NC}"
    echo -e "${MAGENTA}  Reviewer: Opus | Fixers: Sonnet (→Opus on fail)${NC}"
    echo -e "${MAGENTA}═══════════════════════════════════════════════════════════${NC}"
    echo

    local prompt
    prompt=$(build_reviewer_prompt "$state_dir")

    # Save prompt for debugging
    echo "$prompt" > "$state_dir/reviewer_prompt.md"

    echo -e "${GREEN}Launching Opus reviewer...${NC}"
    cd "$PROJECT_ROOT"
    echo "$prompt" | claude --model opus --dangerously-skip-permissions

    echo -e "${GREEN}Phase 2 complete${NC}"
}

# Main
main() {
    parse_args "$@"

    echo -e "${BLUE}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║     Agentic Advent of Code Solver (Multi-Model)           ║${NC}"
    echo -e "${BLUE}║                                                           ║${NC}"
    echo -e "${BLUE}║  Year: $YEAR    Day: $DAY                                     ║${NC}"
    echo -e "${BLUE}║  Resume: $RESUME                                            ║${NC}"
    echo -e "${BLUE}╚═══════════════════════════════════════════════════════════╝${NC}"
    echo

    # Initialize state
    local state_dir
    state_dir=$(init_state)
    echo -e "${YELLOW}State directory: $state_dir${NC}"

    # Extract problem if needed
    extract_problem

    # Run phases
    if [[ -z "$STEP" || "$STEP" == "1" ]]; then
        run_implementation_phase "$state_dir"
    fi

    if [[ -z "$STEP" || "$STEP" == "2" ]]; then
        # No pause between phases - run continuously
        run_review_phase "$state_dir"
    fi

    echo
    echo -e "${GREEN}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║                    All phases complete!                   ║${NC}"
    echo -e "${GREEN}╚═══════════════════════════════════════════════════════════╝${NC}"
}

main "$@"
