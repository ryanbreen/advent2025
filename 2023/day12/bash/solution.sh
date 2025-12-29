#!/usr/bin/env bash
# Advent of Code 2023 Day 12: Hot Springs
#
# This solution uses a memoized dynamic programming approach to count valid
# arrangements of operational (.) and damaged (#) springs matching group patterns.
#
# DESIGN NOTE: Stack-Based Iterative DP
# =====================================
# Bash has a severe performance limitation: subshells. Each $(...) command
# substitution spawns a new process, making recursive solutions prohibitively
# slow (O(n) process spawns per recursion depth).
#
# To avoid this, we simulate recursion using an explicit stack:
# - Instead of recursive function calls, we push work items onto a stack array
# - Instead of return values, we store results in an associative array
# - When a computation needs sub-results, it re-pushes itself to wait, then
#   pushes the sub-computations
#
# This achieves true memoization within a single process, making the solution
# feasible for Part 2's unfolded patterns.

# Global variables for current problem instance
# These are set per-line to avoid passing large arrays through the stack
declare -a groups          # Array of group sizes (e.g., [1, 1, 3])
declare -A memo            # Memoization cache: "pos,group_idx,run" -> count
pattern_str=""             # Current pattern string (e.g., "???.###")
pattern_len=0              # Length of pattern_str
num_groups=0               # Number of groups

# Count valid arrangements using memoized iterative DP
#
# DP State: (position, group_idx, current_run)
#   - position:    Current index in pattern_str (0 to pattern_len)
#   - group_idx:   Index of the group we're currently filling (0 to num_groups)
#   - current_run: Length of the current contiguous run of '#' characters
#
# Transitions:
#   - On '.' (or '?' treated as '.'): End current run if it matches group size
#   - On '#' (or '?' treated as '#'): Extend current run if within group limit
#
# Base case: At end of pattern, valid if all groups are satisfied
count_arrangements_iterative() {
    local input_pattern="$1"
    local groups_csv="$2"

    # Initialize global state for this problem instance
    pattern_str="$input_pattern"
    pattern_len=${#pattern_str}
    IFS=',' read -ra groups <<< "$groups_csv"
    num_groups=${#groups[@]}
    memo=()

    # Stack-based recursion simulation
    # Stack entries: "pos,group_idx,current_run,return_slot"
    #   - return_slot: Key where this computation should store its result
    declare -a stack
    declare -A results
    local stack_idx=0

    # Variables declared at function scope (not inside loops)
    local entry position group_idx current_run return_slot
    local state_key character
    local dot_result_key hash_result_key
    local have_dot have_hash need_dot need_hash
    local sub_state_key final_result base_case_value

    # Push initial call: start at position 0, group 0, no current run
    stack[stack_idx]="0,0,0,main"
    ((stack_idx++))

    while (( stack_idx > 0 )); do
        # Pop top of stack
        ((stack_idx--))
        entry="${stack[$stack_idx]}"
        IFS=',' read -r position group_idx current_run return_slot <<< "$entry"

        state_key="${position},${group_idx},${current_run}"

        # Check memoization cache
        if [[ -v memo["$state_key"] ]]; then
            results["$return_slot"]="${memo[$state_key]}"
            continue
        fi

        # Base case: reached end of pattern
        if (( position == pattern_len )); then
            base_case_value=0
            # Valid if: all groups consumed and no pending run
            if (( group_idx == num_groups && current_run == 0 )); then
                base_case_value=1
            # Valid if: on last group and current run exactly matches it
            elif (( group_idx == num_groups - 1 && num_groups > 0 && groups[group_idx] == current_run )); then
                base_case_value=1
            fi
            memo["$state_key"]="$base_case_value"
            results["$return_slot"]="$base_case_value"
            continue
        fi

        character="${pattern_str:$position:1}"

        # Keys for storing sub-computation results
        dot_result_key="${return_slot}_dot"
        hash_result_key="${return_slot}_hash"

        # Determine which transitions are valid
        have_dot=0
        have_hash=0
        need_dot=0
        need_hash=0

        # Check if treating current character as '.' is valid
        if [[ "$character" == "." || "$character" == "?" ]]; then
            if (( current_run == 0 )); then
                # No active run, can place '.' freely
                need_dot=1
            elif (( group_idx < num_groups && groups[group_idx] == current_run )); then
                # End of run matches current group size
                need_dot=1
            fi
        fi

        # Check if treating current character as '#' is valid
        if [[ "$character" == "#" || "$character" == "?" ]]; then
            if (( group_idx < num_groups && current_run < groups[group_idx] )); then
                # Can extend run if within current group's limit
                need_hash=1
            fi
        fi

        # Check if we already have the needed sub-results
        if (( need_dot )); then
            if [[ -v results["$dot_result_key"] ]]; then
                have_dot=1
            fi
        else
            # Transition not needed, treat as zero contribution
            have_dot=1
            results["$dot_result_key"]=0
        fi

        if (( need_hash )); then
            if [[ -v results["$hash_result_key"] ]]; then
                have_hash=1
            fi
        else
            # Transition not needed, treat as zero contribution
            have_hash=1
            results["$hash_result_key"]=0
        fi

        # Aggregate results if all sub-computations are complete
        if (( have_dot && have_hash )); then
            final_result=$((results["$dot_result_key"] + results["$hash_result_key"]))
            memo["$state_key"]="$final_result"
            results["$return_slot"]="$final_result"
            # Clean up intermediate results to save memory
            unset results["$dot_result_key"]
            unset results["$hash_result_key"]
        else
            # Not ready yet: re-push self to aggregate later, then push sub-calls
            stack[stack_idx]="$entry"
            ((stack_idx++))

            # Push '.' transition sub-call if needed
            if (( need_dot && !have_dot )); then
                if (( current_run == 0 )); then
                    # Continue without consuming a group
                    sub_state_key="$((position + 1)),${group_idx},0"
                    if [[ -v memo["$sub_state_key"] ]]; then
                        results["$dot_result_key"]="${memo[$sub_state_key]}"
                    else
                        stack[stack_idx]="$((position + 1)),${group_idx},0,${dot_result_key}"
                        ((stack_idx++))
                    fi
                else
                    # End current run, advance to next group
                    sub_state_key="$((position + 1)),$((group_idx + 1)),0"
                    if [[ -v memo["$sub_state_key"] ]]; then
                        results["$dot_result_key"]="${memo[$sub_state_key]}"
                    else
                        stack[stack_idx]="$((position + 1)),$((group_idx + 1)),0,${dot_result_key}"
                        ((stack_idx++))
                    fi
                fi
            fi

            # Push '#' transition sub-call if needed
            if (( need_hash && !have_hash )); then
                # Extend current run by one
                sub_state_key="$((position + 1)),${group_idx},$((current_run + 1))"
                if [[ -v memo["$sub_state_key"] ]]; then
                    results["$hash_result_key"]="${memo[$sub_state_key]}"
                else
                    stack[stack_idx]="$((position + 1)),${group_idx},$((current_run + 1)),${hash_result_key}"
                    ((stack_idx++))
                fi
            fi
        fi
    done

    echo "${results[main]}"
}

# Process a single input line and return the arrangement count
# Arguments:
#   $1 - Input line (e.g., "???.### 1,1,3")
#   $2 - Unfold flag (0 = Part 1, 1 = Part 2)
process_line() {
    local line="$1"
    local unfold="$2"

    # Parse line into pattern and groups
    local pattern_str="${line%% *}"
    local groups_csv="${line##* }"

    # Declare loop variables at function scope
    local unfolded_pattern unfolded_groups

    # Part 2: Unfold pattern 5x with '?' separators, groups 5x with ',' separators
    if (( unfold == 1 )); then
        unfolded_pattern="$pattern_str"
        unfolded_groups="$groups_csv"
        for _ in {1..4}; do
            unfolded_pattern="${unfolded_pattern}?${pattern_str}"
            unfolded_groups="${unfolded_groups},${groups_csv}"
        done
        pattern_str="$unfolded_pattern"
        groups_csv="$unfolded_groups"
    fi

    count_arrangements_iterative "$pattern_str" "$groups_csv"
}

# Main entry point
main() {
    local input_file="../input.txt"

    if [[ ! -f "$input_file" ]]; then
        echo "Error: input file not found: $input_file" >&2
        exit 1
    fi

    # Declare variables at function scope
    local part1_total=0
    local part2_total=0
    local line count

    # Part 1: Count arrangements for each row
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue
        count=$(process_line "$line" 0)
        part1_total=$((part1_total + count))
    done < "$input_file"

    echo "Part 1: $part1_total"

    # Part 2: Unfold patterns 5x and count arrangements
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -z "$line" ]] && continue
        count=$(process_line "$line" 1)
        part2_total=$((part2_total + count))
    done < "$input_file"

    echo "Part 2: $part2_total"
}

# Run if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    cd "$(dirname "$0")" || exit 1
    main
fi
