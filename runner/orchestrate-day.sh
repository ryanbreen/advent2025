#!/usr/bin/env bash
#
# Day Orchestrator Script
# Meta-orchestrator that runs Claude sessions to implement and validate AoC solutions
#
# Usage: ./orchestrate-day.sh <year> <day>
# Example: ./orchestrate-day.sh 2023 5
#
# This script runs two Claude sessions:
#   Pass 1: Implement the day in all 16 languages (commit + push)
#   Pass 2: Validate and tune implementations (commit + push)
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_step() { echo -e "${CYAN}[STEP]${NC} $1"; }

# Validate arguments
if [[ $# -lt 2 ]]; then
    echo -e "${BOLD}Usage:${NC} $0 <year> <day>"
    echo "Example: $0 2023 5"
    echo
    echo "This script orchestrates full day implementation:"
    echo "  1. Extracts problem and implements in 16 languages"
    echo "  2. Validates and tunes all implementations"
    echo "  3. Commits and pushes after each pass"
    exit 1
fi

YEAR="$1"
DAY="$2"

# Pad day with zero if needed for directory name
DAY_PADDED=$(printf "%02d" "$DAY")
DAY_DIR="$PROJECT_ROOT/$YEAR/day$DAY_PADDED"

# Check if claude CLI is available
if ! command -v claude &> /dev/null; then
    log_error "claude CLI not found. Please install Claude Code first."
    exit 1
fi

echo
echo -e "${BOLD}${BLUE}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${BLUE}║           Advent of Code Day Orchestrator                     ║${NC}"
echo -e "${BOLD}${BLUE}║           Year: $YEAR | Day: $DAY                                     ║${NC}"
echo -e "${BOLD}${BLUE}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo

# Check if day already exists
if [[ -d "$DAY_DIR" ]]; then
    log_warn "Directory $DAY_DIR already exists"
    read -p "Continue anyway? This may overwrite existing work. (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 1
    fi
fi

# Track timing
TOTAL_START=$(date +%s)

# ============================================================================
# PASS 1: Implementation
# ============================================================================
echo
echo -e "${BOLD}${GREEN}┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓${NC}"
echo -e "${BOLD}${GREEN}┃  PASS 1: Implementing Day $DAY in 16 languages                 ┃${NC}"
echo -e "${BOLD}${GREEN}┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛${NC}"
echo

PASS1_START=$(date +%s)

IMPLEMENT_PROMPT="Implement Advent of Code $YEAR Day $DAY.

Follow the workflow from CLAUDE.md:

1. Extract the problem using: node runner/extract.js $YEAR $DAY
2. Implement Part 1 in Python and Node.js first
3. Verify both implementations produce the same answer
4. Submit Part 1 answer using: node runner/submit.js $YEAR $DAY 1 <answer>
5. Implement Part 2 in Python and Node.js
6. Verify both implementations produce the same answer  
7. Submit Part 2 answer using: node runner/submit.js $YEAR $DAY 2 <answer>
8. Dispatch agents in parallel to implement the remaining 14 languages:
   ARM64, C, C++, Rust, Zig, Go, Java, Ruby, PHP, Perl, Bash, Clojure, Common Lisp, ColdFusion
9. Run benchmarks on all 16 implementations using: python3 runner/benchmark.py
10. Update $YEAR/README.md with Day $DAY progress and benchmarks
11. Create $YEAR/day$DAY_PADDED/README.md with algorithmic analysis (see CLAUDE.md for format)

IMPORTANT:
- Do NOT provide expected outputs to agents - let them work from the problem description
- Submit answers once 3+ implementations agree
- All 16 languages are required
- Include benchmark table in the day README"

log_step "Starting Claude session for implementation..."
echo

cd "$PROJECT_ROOT"

# Run Claude for implementation
if claude --print "$IMPLEMENT_PROMPT"; then
    echo
    log_success "Implementation pass completed"
else
    echo
    log_error "Implementation pass failed"
    exit 1
fi

PASS1_END=$(date +%s)
PASS1_DURATION=$((PASS1_END - PASS1_START))
log_info "Pass 1 duration: ${PASS1_DURATION}s"

# Commit and push after implementation
echo
log_step "Committing implementation..."

if git status --porcelain | grep -q .; then
    git add -A
    git commit -m "$(cat <<EOF
Add Day $DAY: Implementation in 16 languages

Automated implementation via orchestrate-day.sh

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
    log_step "Pushing to remote..."
    git push
    log_success "Implementation committed and pushed"
else
    log_warn "No changes to commit after implementation"
fi

# ============================================================================
# PASS 2: Validation
# ============================================================================
echo
echo -e "${BOLD}${GREEN}┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓${NC}"
echo -e "${BOLD}${GREEN}┃  PASS 2: Validating and tuning implementations                ┃${NC}"
echo -e "${BOLD}${GREEN}┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛${NC}"
echo

PASS2_START=$(date +%s)

VALIDATE_PROMPT="/validate-implementation $YEAR $DAY"

log_step "Starting Claude session for validation..."
echo

# Run Claude for validation
if claude --print "$VALIDATE_PROMPT"; then
    echo
    log_success "Validation pass completed"
else
    echo
    log_error "Validation pass failed"
    exit 1
fi

PASS2_END=$(date +%s)
PASS2_DURATION=$((PASS2_END - PASS2_START))
log_info "Pass 2 duration: ${PASS2_DURATION}s"

# Commit and push after validation
echo
log_step "Committing validation improvements..."

if git status --porcelain | grep -q .; then
    git add -A
    git commit -m "$(cat <<EOF
Tune Day $DAY: Code quality improvements

Validated and tuned all 16 implementations for:
- Algorithmic purity
- Idiomatic quality
- Stylistic quality

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
    log_step "Pushing to remote..."
    git push
    log_success "Validation improvements committed and pushed"
else
    log_warn "No changes to commit after validation"
fi

# ============================================================================
# Summary
# ============================================================================
TOTAL_END=$(date +%s)
TOTAL_DURATION=$((TOTAL_END - TOTAL_START))

echo
echo -e "${BOLD}${BLUE}╔═══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${BLUE}║                      Complete!                                ║${NC}"
echo -e "${BOLD}${BLUE}╚═══════════════════════════════════════════════════════════════╝${NC}"
echo
echo -e "${BOLD}Summary:${NC}"
echo -e "  Day:              ${GREEN}$YEAR Day $DAY${NC}"
echo -e "  Location:         ${GREEN}$DAY_DIR${NC}"
echo -e "  Pass 1 (impl):    ${CYAN}${PASS1_DURATION}s${NC}"
echo -e "  Pass 2 (tune):    ${CYAN}${PASS2_DURATION}s${NC}"
echo -e "  Total time:       ${CYAN}${TOTAL_DURATION}s${NC} ($((TOTAL_DURATION / 60))m $((TOTAL_DURATION % 60))s)"
echo

if [[ -d "$DAY_DIR" ]]; then
    echo -e "${BOLD}Languages implemented:${NC}"
    for lang_dir in "$DAY_DIR"/*/; do
        if [[ -d "$lang_dir" ]]; then
            lang_name=$(basename "$lang_dir")
            echo -e "  ${GREEN}✓${NC} $lang_name"
        fi
    done
fi
echo
