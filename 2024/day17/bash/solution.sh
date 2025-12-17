#!/bin/bash
# Day 17: Chronospatial Computer - 3-bit VM emulator

set -e

# Parse input
INPUT_FILE="${1:-../input.txt}"

# Read registers and program
REG_A=$(grep "Register A:" "$INPUT_FILE" | sed 's/Register A: //')
REG_B=$(grep "Register B:" "$INPUT_FILE" | sed 's/Register B: //')
REG_C=$(grep "Register C:" "$INPUT_FILE" | sed 's/Register C: //')
PROGRAM_STR=$(grep "Program:" "$INPUT_FILE" | sed 's/Program: //')

# Parse program into array
IFS=',' read -ra PROGRAM <<< "$PROGRAM_STR"
PROGRAM_LEN=${#PROGRAM[@]}

# Run the VM with given registers, output stored in OUTPUT array
run_program() {
    local a=$1
    local b=$2
    local c=$3
    local ip=0
    OUTPUT=()

    while (( ip < PROGRAM_LEN )); do
        local opcode=${PROGRAM[$ip]}
        local operand=${PROGRAM[$((ip + 1))]}

        # Combo operand function inline
        local combo
        if (( operand <= 3 )); then
            combo=$operand
        elif (( operand == 4 )); then
            combo=$a
        elif (( operand == 5 )); then
            combo=$b
        elif (( operand == 6 )); then
            combo=$c
        fi

        case $opcode in
            0)  # adv - A = A >> combo
                a=$((a >> combo))
                ;;
            1)  # bxl - B = B XOR literal
                b=$((b ^ operand))
                ;;
            2)  # bst - B = combo % 8
                b=$((combo & 7))
                ;;
            3)  # jnz - jump if A != 0
                if (( a != 0 )); then
                    ip=$operand
                    continue
                fi
                ;;
            4)  # bxc - B = B XOR C
                b=$((b ^ c))
                ;;
            5)  # out - output combo % 8
                OUTPUT+=($((combo & 7)))
                ;;
            6)  # bdv - B = A >> combo
                b=$((a >> combo))
                ;;
            7)  # cdv - C = A >> combo
                c=$((a >> combo))
                ;;
        esac

        ip=$((ip + 2))
    done
}

# Part 1: Run with initial registers and output comma-separated result
part1() {
    run_program "$REG_A" "$REG_B" "$REG_C"
    local result=""
    for val in "${OUTPUT[@]}"; do
        if [[ -n "$result" ]]; then
            result="$result,$val"
        else
            result="$val"
        fi
    done
    echo "$result"
}

# Part 2: Find initial A that makes program output itself
# Work backwards, building A 3 bits at a time
part2() {
    local result
    result=$(search_a $((PROGRAM_LEN - 1)) 0)
    echo "$result"
}

# Recursive search for A value
search_a() {
    local target_idx=$1
    local current_a=$2

    # Base case: found complete solution
    if (( target_idx < 0 )); then
        echo "$current_a"
        return 0
    fi

    # Try all 8 possible 3-bit values
    local bits
    for bits in 0 1 2 3 4 5 6 7; do
        local candidate_a=$(((current_a << 3) | bits))

        # Skip if A would be 0 at the start
        if (( candidate_a == 0 && target_idx == PROGRAM_LEN - 1 )); then
            continue
        fi

        run_program "$candidate_a" "$REG_B" "$REG_C"

        # Check if output matches expected suffix of program
        local output_len=${#OUTPUT[@]}
        local expected_len=$((PROGRAM_LEN - target_idx))

        if (( output_len == expected_len )); then
            local match=1
            local i
            for (( i=0; i<output_len; i++ )); do
                if (( OUTPUT[i] != PROGRAM[target_idx + i] )); then
                    match=0
                    break
                fi
            done

            if (( match == 1 )); then
                local result
                result=$(search_a $((target_idx - 1)) "$candidate_a")
                if [[ -n "$result" ]]; then
                    echo "$result"
                    return 0
                fi
            fi
        fi
    done

    return 1
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
