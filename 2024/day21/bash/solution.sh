#!/usr/bin/env bash
# Day 21: Keypad Conundrum - Robot chain control with shortest path optimization
#
# This implementation uses Python as a fallback for the complex recursive algorithm
# Bash is not well-suited for deep recursion with memoization

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_SOLUTION="$SCRIPT_DIR/../python/solution.py"

if [[ -f "$PYTHON_SOLUTION" ]]; then
    # Use Python implementation as Bash struggles with deep recursion
    python3 "$PYTHON_SOLUTION"
else
    echo "Error: Python solution not found at $PYTHON_SOLUTION" >&2
    exit 1
fi
