package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Gate struct {
	in1 string
	op  string
	in2 string
	out string
}

func parseInput(filename string) (map[string]int, []Gate, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return nil, nil, err
	}

	parts := strings.Split(strings.TrimSpace(string(content)), "\n\n")
	if len(parts) != 2 {
		return nil, nil, fmt.Errorf("invalid input format")
	}

	// Parse initial wire values
	wires := make(map[string]int)
	for _, line := range strings.Split(parts[0], "\n") {
		parts := strings.Split(line, ": ")
		name := parts[0]
		val, _ := strconv.Atoi(parts[1])
		wires[name] = val
	}

	// Parse gates
	var gates []Gate
	for _, line := range strings.Split(parts[1], "\n") {
		fields := strings.Fields(line)
		// Format: "x00 AND y00 -> z00"
		gates = append(gates, Gate{
			in1: fields[0],
			op:  fields[1],
			in2: fields[2],
			out: fields[4],
		})
	}

	return wires, gates, nil
}

func simulate(wires map[string]int, gates []Gate) map[string]int {
	// Make a copy of wires
	result := make(map[string]int)
	for k, v := range wires {
		result[k] = v
	}

	remaining := make([]Gate, len(gates))
	copy(remaining, gates)

	for len(remaining) > 0 {
		madeProgress := false
		var newRemaining []Gate

		for _, gate := range remaining {
			v1, has1 := result[gate.in1]
			v2, has2 := result[gate.in2]

			if has1 && has2 {
				var output int
				switch gate.op {
				case "AND":
					output = v1 & v2
				case "OR":
					output = v1 | v2
				case "XOR":
					output = v1 ^ v2
				}
				result[gate.out] = output
				madeProgress = true
			} else {
				newRemaining = append(newRemaining, gate)
			}
		}

		remaining = newRemaining
		if !madeProgress && len(remaining) > 0 {
			panic("Circuit stuck - missing inputs")
		}
	}

	return result
}

func getZValue(wires map[string]int) int64 {
	// Get all z wires and sort them
	var zWires []string
	for name := range wires {
		if strings.HasPrefix(name, "z") {
			zWires = append(zWires, name)
		}
	}
	sort.Strings(zWires)

	// Build number from highest to lowest bit
	var result int64
	for i := len(zWires) - 1; i >= 0; i-- {
		result = (result << 1) | int64(wires[zWires[i]])
	}

	return result
}

func part1(wires map[string]int, gates []Gate) int64 {
	finalWires := simulate(wires, gates)
	return getZValue(finalWires)
}

func part2(gates []Gate) string {
	swapped := make(map[string]bool)

	// Build lookup: output -> gate
	gateByOutput := make(map[string]Gate)
	for _, gate := range gates {
		gateByOutput[gate.out] = gate
	}

	// Build lookup: (inputs set, op) -> output
	type InputsOp struct {
		inputs string // sorted comma-separated
		op     string
	}
	gateByInputsOp := make(map[InputsOp]string)
	for _, gate := range gates {
		inputs := []string{gate.in1, gate.in2}
		sort.Strings(inputs)
		key := InputsOp{
			inputs: strings.Join(inputs, ","),
			op:     gate.op,
		}
		gateByInputsOp[key] = gate.out
	}

	// Find the highest bit number
	maxBit := 0
	for _, gate := range gates {
		if strings.HasPrefix(gate.out, "z") {
			bit, _ := strconv.Atoi(gate.out[1:])
			if bit > maxBit {
				maxBit = bit
			}
		}
	}

	maxBitStr := fmt.Sprintf("z%02d", maxBit)

	for _, gate := range gates {
		// Rule: XOR gates that don't take x,y as input should output to z
		if gate.op == "XOR" {
			isXYXor := (strings.HasPrefix(gate.in1, "x") || strings.HasPrefix(gate.in1, "y")) &&
				(strings.HasPrefix(gate.in2, "x") || strings.HasPrefix(gate.in2, "y"))
			if !isXYXor {
				// This is a second-level XOR (sum XOR carry), should output to z
				if !strings.HasPrefix(gate.out, "z") {
					swapped[gate.out] = true
				}
			}
		}

		// Rule: z outputs (except highest bit) should come from XOR
		if strings.HasPrefix(gate.out, "z") && gate.out != maxBitStr {
			if gate.op != "XOR" {
				swapped[gate.out] = true
			}
		}

		// Rule: AND gates (except x00 AND y00) should feed into OR
		if gate.op == "AND" {
			isFirstBit := (gate.in1 == "x00" && gate.in2 == "y00") ||
				(gate.in1 == "y00" && gate.in2 == "x00")
			if !isFirstBit {
				// This AND output should be input to an OR gate
				usedByOr := false
				for _, gate2 := range gates {
					if gate2.op == "OR" && (gate.out == gate2.in1 || gate.out == gate2.in2) {
						usedByOr = true
						break
					}
				}
				if !usedByOr {
					swapped[gate.out] = true
				}
			}
		}

		// Rule: XOR of x,y should feed into another XOR (for z output) or AND (for carry)
		if gate.op == "XOR" {
			isXYXor := (strings.HasPrefix(gate.in1, "x") || strings.HasPrefix(gate.in1, "y")) &&
				(strings.HasPrefix(gate.in2, "x") || strings.HasPrefix(gate.in2, "y"))
			// Skip z00 which is x00 XOR y00 directly
			isZ00 := (gate.in1 == "x00" && gate.in2 == "y00") ||
				(gate.in1 == "y00" && gate.in2 == "x00")
			if isXYXor && !isZ00 {
				// Should be used by XOR and AND
				usedByXor := false
				usedByAnd := false
				for _, gate2 := range gates {
					if gate.out == gate2.in1 || gate.out == gate2.in2 {
						if gate2.op == "XOR" {
							usedByXor = true
						} else if gate2.op == "AND" {
							usedByAnd = true
						}
					}
				}
				if !(usedByXor && usedByAnd) {
					swapped[gate.out] = true
				}
			}
		}
	}

	// Convert to sorted list
	var swappedList []string
	for wire := range swapped {
		swappedList = append(swappedList, wire)
	}
	sort.Strings(swappedList)

	return strings.Join(swappedList, ",")
}

func main() {
	wires, gates, err := parseInput("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(wires, gates))
	fmt.Printf("Part 2: %s\n", part2(gates))
}
