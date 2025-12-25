#!/usr/bin/env bash

set -euo pipefail

# Parse input and simulate logic gates
parse_and_simulate() {
    local input_file="$1"
    declare -gA wires
    declare -ga gates_in1 gates_op gates_in2 gates_out

    local in_initial=1
    local idx=0

    while IFS= read -r line; do
        if [[ -z "$line" ]]; then
            in_initial=0
            continue
        fi

        if [[ $in_initial -eq 1 ]]; then
            # Parse initial wire values: "x00: 1"
            local name="${line%%:*}"
            local val="${line##*: }"
            wires[$name]=$val
        else
            # Parse gates: "x00 AND y00 -> z00"
            read -r in1 op in2 arrow out <<< "$line"
            gates_in1[$idx]=$in1
            gates_op[$idx]=$op
            gates_in2[$idx]=$in2
            gates_out[$idx]=$out
            idx=$((idx + 1))
        fi
    done < "$input_file"
}

# Simulate the circuit
simulate() {
    local -a pending=()
    local i
    for ((i=0; i<${#gates_in1[@]}; i++)); do
        pending+=($i)
    done

    while [[ ${#pending[@]} -gt 0 ]]; do
        local -a new_pending=()
        local made_progress=0

        for idx in "${pending[@]+"${pending[@]}"}"; do
            [[ -z "$idx" ]] && continue
            local in1="${gates_in1[$idx]}"
            local in2="${gates_in2[$idx]}"
            local op="${gates_op[$idx]}"
            local out="${gates_out[$idx]}"

            if [[ -n "${wires[$in1]:-}" && -n "${wires[$in2]:-}" ]]; then
                local v1="${wires[$in1]}"
                local v2="${wires[$in2]}"
                local result

                case "$op" in
                    AND)
                        result=$(( v1 & v2 ))
                        ;;
                    OR)
                        result=$(( v1 | v2 ))
                        ;;
                    XOR)
                        result=$(( v1 ^ v2 ))
                        ;;
                esac

                wires[$out]=$result
                made_progress=1
            else
                new_pending+=($idx)
            fi
        done

        pending=("${new_pending[@]+"${new_pending[@]}"}")

        if [[ $made_progress -eq 0 && ${#pending[@]} -gt 0 ]]; then
            echo "Error: Circuit stuck" >&2
            exit 1
        fi
    done
}

# Get decimal value from z wires
get_z_value() {
    local -a z_wires=()

    # Collect all z wires
    for wire in "${!wires[@]}"; do
        if [[ $wire == z* ]]; then
            z_wires+=("$wire")
        fi
    done

    # Sort z wires in reverse order (z45, z44, ..., z01, z00)
    IFS=$'\n' z_wires=($(sort -r <<< "${z_wires[*]}"))

    # Build binary number
    local result=0
    for z in "${z_wires[@]}"; do
        result=$(( (result << 1) | wires[$z] ))
    done

    echo "$result"
}

# Part 1: Simulate and get z value
part1() {
    simulate
    get_z_value
}

# Part 2: Find swapped wires
part2() {
    declare -A gate_by_output
    declare -A gate_by_inputs_op
    declare -A swapped

    # Build lookups
    local i
    for ((i=0; i<${#gates_in1[@]}; i++)); do
        local in1="${gates_in1[$i]}"
        local in2="${gates_in2[$i]}"
        local op="${gates_op[$i]}"
        local out="${gates_out[$i]}"

        # Output -> gate info
        gate_by_output[$out]="$in1|$op|$in2"

        # Inputs+op -> output (sorted inputs for lookup)
        if [[ "$in1" < "$in2" ]]; then
            gate_by_inputs_op["$in1,$in2|$op"]=$out
        else
            gate_by_inputs_op["$in2,$in1|$op"]=$out
        fi
    done

    # Helper function to find gate with inputs a,b and operation op
    find_gate() {
        local a="$1"
        local b="$2"
        local op="$3"
        local key

        if [[ "$a" < "$b" ]]; then
            key="$a,$b|$op"
        else
            key="$b,$a|$op"
        fi

        echo "${gate_by_inputs_op[$key]:-}"
    }

    # Find max bit number
    local max_bit=0
    for ((i=0; i<${#gates_out[@]}; i++)); do
        local out="${gates_out[$i]}"
        if [[ $out == z* ]]; then
            local bit_num="${out#z}"
            bit_num=$((10#$bit_num))  # Force base 10
            if [[ $bit_num -gt $max_bit ]]; then
                max_bit=$bit_num
            fi
        fi
    done

    # Check rules for each gate
    for ((i=0; i<${#gates_in1[@]}; i++)); do
        local in1="${gates_in1[$i]}"
        local in2="${gates_in2[$i]}"
        local op="${gates_op[$i]}"
        local out="${gates_out[$i]}"

        # Check if inputs are x,y wires
        local is_xy_input=0
        if [[ ($in1 == x* || $in1 == y*) && ($in2 == x* || $in2 == y*) ]]; then
            is_xy_input=1
        fi

        # Rule: XOR gates that don't take x,y as input should output to z
        if [[ "$op" == "XOR" ]]; then
            if [[ $is_xy_input -eq 0 ]]; then
                # Second-level XOR (sum XOR carry), should output to z
                if [[ $out != z* ]]; then
                    swapped[$out]=1
                fi
            fi
        fi

        # Rule: z outputs (except last bit) should come from XOR
        if [[ $out == z* ]]; then
            local bit_num="${out#z}"
            bit_num=$((10#$bit_num))

            if [[ $bit_num -ne $max_bit ]]; then
                if [[ "$op" != "XOR" ]]; then
                    swapped[$out]=1
                fi
            fi
        fi

        # Rule: AND gates (except x00 AND y00) should feed into OR
        if [[ "$op" == "AND" ]]; then
            local is_first_bit=0
            if [[ ($in1 == "x00" && $in2 == "y00") || ($in1 == "y00" && $in2 == "x00") ]]; then
                is_first_bit=1
            fi

            if [[ $is_first_bit -eq 0 ]]; then
                # This AND output should be input to an OR gate
                local used_by_or=0
                for ((j=0; j<${#gates_in1[@]}; j++)); do
                    if [[ "${gates_op[$j]}" == "OR" ]]; then
                        if [[ "${gates_in1[$j]}" == "$out" || "${gates_in2[$j]}" == "$out" ]]; then
                            used_by_or=1
                            break
                        fi
                    fi
                done

                if [[ $used_by_or -eq 0 ]]; then
                    swapped[$out]=1
                fi
            fi
        fi

        # Rule: XOR of x,y should feed into another XOR and AND
        if [[ "$op" == "XOR" && $is_xy_input -eq 1 ]]; then
            # Skip z00 which is x00 XOR y00 directly
            local is_z00=0
            if [[ ($in1 == "x00" && $in2 == "y00") || ($in1 == "y00" && $in2 == "x00") ]]; then
                is_z00=1
            fi

            if [[ $is_z00 -eq 0 ]]; then
                # Should be used by both XOR and AND
                local used_by_xor=0
                local used_by_and=0

                for ((j=0; j<${#gates_in1[@]}; j++)); do
                    if [[ "${gates_in1[$j]}" == "$out" || "${gates_in2[$j]}" == "$out" ]]; then
                        if [[ "${gates_op[$j]}" == "XOR" ]]; then
                            used_by_xor=1
                        elif [[ "${gates_op[$j]}" == "AND" ]]; then
                            used_by_and=1
                        fi
                    fi
                done

                if [[ $used_by_xor -eq 0 || $used_by_and -eq 0 ]]; then
                    swapped[$out]=1
                fi
            fi
        fi
    done

    # Sort and join swapped wires
    local -a sorted_swapped=()
    for wire in "${!swapped[@]}"; do
        sorted_swapped+=("$wire")
    done

    IFS=$'\n' sorted_swapped=($(sort <<< "${sorted_swapped[*]}"))

    local result=""
    for wire in "${sorted_swapped[@]}"; do
        if [[ -z "$result" ]]; then
            result="$wire"
        else
            result="$result,$wire"
        fi
    done

    echo "$result"
}

main() {
    local input_file="../input.txt"

    parse_and_simulate "$input_file"

    echo "Part 1: $(part1)"
    echo "Part 2: $(part2)"
}

main
