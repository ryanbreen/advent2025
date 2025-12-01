#!/bin/bash

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  Advent of Code 2025 Day 1 - Brainfuck Solution Test Suite   ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

echo "Test 1: Example Input (should output 3 and 6)"
echo "───────────────────────────────────────────────"
python3 solve.py example.txt
echo ""

echo "Test 2: Actual Puzzle Input (Part 1 should be 1150)"
echo "────────────────────────────────────────────────────"
python3 solve.py ../input.txt
echo ""

echo "Test 3: Brainfuck Output (should output 1150)"
echo "──────────────────────────────────────────────"
python3 bf_interpreter.py solution.bf
echo ""

echo "Test 4: Algorithm Trace"
echo "───────────────────────"
python3 trace_example.py | tail -5
echo ""

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  All Tests Complete!                                          ║"
echo "║  Part 1 Answer: 1150                                          ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
