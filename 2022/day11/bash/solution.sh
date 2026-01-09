#!/bin/bash

# Day 11: Monkey in the Middle
# Parse monkey definitions, simulate rounds of item throwing
# Uses bc only when needed for overflow prevention

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_FILE="$SCRIPT_DIR/../input.txt"

# Arrays to store monkey data (indexed by monkey number)
declare -a ITEMS         # Space-separated list of items for each monkey
declare -a OPERATORS     # + or *
declare -a OPERANDS      # number or 'old'
declare -a DIVISORS      # divisibility test value
declare -a IF_TRUE       # target monkey if divisible
declare -a IF_FALSE      # target monkey if not divisible
declare -a INSPECTIONS   # count of inspections

NUM_MONKEYS=0

parse_input() {
    local monkey_num=-1
    local line

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" =~ ^Monkey\ ([0-9]+): ]]; then
            monkey_num=${BASH_REMATCH[1]}
            ITEMS[$monkey_num]=""
            INSPECTIONS[$monkey_num]=0
            NUM_MONKEYS=$((monkey_num + 1))
        elif [[ "$line" =~ Starting\ items:\ (.+) ]]; then
            local items_str="${BASH_REMATCH[1]}"
            items_str="${items_str//,/ }"
            items_str="${items_str// / }"
            ITEMS[$monkey_num]=$(echo "$items_str" | tr -s ' ')
        elif [[ "$line" =~ Operation:\ new\ =\ old\ ([+*])\ (.+) ]]; then
            OPERATORS[$monkey_num]="${BASH_REMATCH[1]}"
            OPERANDS[$monkey_num]="${BASH_REMATCH[2]}"
        elif [[ "$line" =~ Test:\ divisible\ by\ ([0-9]+) ]]; then
            DIVISORS[$monkey_num]="${BASH_REMATCH[1]}"
        elif [[ "$line" =~ If\ true:\ throw\ to\ monkey\ ([0-9]+) ]]; then
            IF_TRUE[$monkey_num]="${BASH_REMATCH[1]}"
        elif [[ "$line" =~ If\ false:\ throw\ to\ monkey\ ([0-9]+) ]]; then
            IF_FALSE[$monkey_num]="${BASH_REMATCH[1]}"
        fi
    done < "$INPUT_FILE"
}

# Simulate one round with inline arithmetic
# For part 2, mod_value keeps numbers small enough for bash arithmetic
simulate_round_part1() {
    for ((m = 0; m < NUM_MONKEYS; m++)); do
        local items_list="${ITEMS[$m]}"
        ITEMS[$m]=""
        [[ -z "$items_list" ]] && continue

        local operator="${OPERATORS[$m]}"
        local operand="${OPERANDS[$m]}"
        local divisor="${DIVISORS[$m]}"
        local target_true="${IF_TRUE[$m]}"
        local target_false="${IF_FALSE[$m]}"

        for item in $items_list; do
            ((INSPECTIONS[m]++))

            local op_val="$operand"
            [[ "$operand" == "old" ]] && op_val="$item"

            local new_val
            if [[ "$operator" == "+" ]]; then
                new_val=$((item + op_val))
            else
                new_val=$((item * op_val))
            fi

            # Divide by 3 for part 1
            new_val=$((new_val / 3))

            local target
            if ((new_val % divisor == 0)); then
                target="$target_true"
            else
                target="$target_false"
            fi

            if [[ -z "${ITEMS[$target]}" ]]; then
                ITEMS[$target]="$new_val"
            else
                ITEMS[$target]="${ITEMS[$target]} $new_val"
            fi
        done
    done
}

# For part 2, we need bc because squaring can overflow before modulo
simulate_round_part2() {
    local mod_value="$1"

    for ((m = 0; m < NUM_MONKEYS; m++)); do
        local items_list="${ITEMS[$m]}"
        ITEMS[$m]=""
        [[ -z "$items_list" ]] && continue

        local operator="${OPERATORS[$m]}"
        local operand="${OPERANDS[$m]}"
        local divisor="${DIVISORS[$m]}"
        local target_true="${IF_TRUE[$m]}"
        local target_false="${IF_FALSE[$m]}"

        for item in $items_list; do
            ((INSPECTIONS[m]++))

            local op_val="$operand"
            [[ "$operand" == "old" ]] && op_val="$item"

            local new_val
            if [[ "$operator" == "+" ]]; then
                # Addition is safe in bash
                new_val=$((item + op_val))
                new_val=$((new_val % mod_value))
            else
                # Multiplication may overflow, use bc
                new_val=$(echo "($item * $op_val) % $mod_value" | bc)
            fi

            local target
            if ((new_val % divisor == 0)); then
                target="$target_true"
            else
                target="$target_false"
            fi

            if [[ -z "${ITEMS[$target]}" ]]; then
                ITEMS[$target]="$new_val"
            else
                ITEMS[$target]="${ITEMS[$target]} $new_val"
            fi
        done
    done
}

calc_mod_value() {
    local result=1
    for ((m = 0; m < NUM_MONKEYS; m++)); do
        result=$((result * DIVISORS[m]))
    done
    echo "$result"
}

calc_monkey_business() {
    local sorted
    sorted=$(for ((m = 0; m < NUM_MONKEYS; m++)); do
        echo "${INSPECTIONS[$m]}"
    done | sort -rn)

    local top1 top2
    top1=$(echo "$sorted" | head -n 1)
    top2=$(echo "$sorted" | head -n 2 | tail -n 1)

    echo "$top1 * $top2" | bc
}

reset_monkeys() {
    parse_input
}

part1() {
    reset_monkeys

    for ((round = 1; round <= 20; round++)); do
        simulate_round_part1
    done

    calc_monkey_business
}

part2() {
    reset_monkeys

    local mod_value
    mod_value=$(calc_mod_value)

    for ((round = 1; round <= 10000; round++)); do
        simulate_round_part2 "$mod_value"
    done

    calc_monkey_business
}

# Main
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"
