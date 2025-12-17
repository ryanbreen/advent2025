// Day 17: Chronospatial Computer - 3-bit VM emulator
package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func parseInput(text string) (int64, int64, int64, []int) {
	lines := strings.Split(strings.TrimSpace(text), "\n")

	reA := regexp.MustCompile(`Register A: (\d+)`)
	reB := regexp.MustCompile(`Register B: (\d+)`)
	reC := regexp.MustCompile(`Register C: (\d+)`)
	reProg := regexp.MustCompile(`Program: ([\d,]+)`)

	a, _ := strconv.ParseInt(reA.FindStringSubmatch(lines[0])[1], 10, 64)
	b, _ := strconv.ParseInt(reB.FindStringSubmatch(lines[1])[1], 10, 64)
	c, _ := strconv.ParseInt(reC.FindStringSubmatch(lines[2])[1], 10, 64)

	progStr := reProg.FindStringSubmatch(lines[4])[1]
	progParts := strings.Split(progStr, ",")
	program := make([]int, len(progParts))
	for i, p := range progParts {
		val, _ := strconv.Atoi(p)
		program[i] = val
	}

	return a, b, c, program
}

func runProgram(a, b, c int64, program []int) []int {
	ip := 0
	var output []int

	combo := func(operand int) int64 {
		switch operand {
		case 0, 1, 2, 3:
			return int64(operand)
		case 4:
			return a
		case 5:
			return b
		case 6:
			return c
		default:
			panic(fmt.Sprintf("Invalid combo operand: %d", operand))
		}
	}

	for ip < len(program) {
		opcode := program[ip]
		operand := program[ip+1]

		switch opcode {
		case 0: // adv - A = A >> combo
			a = a >> combo(operand)
		case 1: // bxl - B = B XOR literal
			b = b ^ int64(operand)
		case 2: // bst - B = combo % 8
			b = combo(operand) & 7
		case 3: // jnz - jump if A != 0
			if a != 0 {
				ip = operand
				continue
			}
		case 4: // bxc - B = B XOR C
			b = b ^ c
		case 5: // out - output combo % 8
			output = append(output, int(combo(operand)&7))
		case 6: // bdv - B = A >> combo
			b = a >> combo(operand)
		case 7: // cdv - C = A >> combo
			c = a >> combo(operand)
		}

		ip += 2
	}

	return output
}

func part1(text string) string {
	a, b, c, program := parseInput(text)
	output := runProgram(a, b, c, program)

	strs := make([]string, len(output))
	for i, v := range output {
		strs[i] = strconv.Itoa(v)
	}
	return strings.Join(strs, ",")
}

func part2(text string) int64 {
	_, b, c, program := parseInput(text)

	// The program loops, outputting one digit per iteration, dividing A by 8 each time.
	// We need to find A such that output == program.
	// Work backwards from the last digit - build A 3 bits at a time.

	var search func(targetIdx int, currentA int64) (int64, bool)
	search = func(targetIdx int, currentA int64) (int64, bool) {
		if targetIdx < 0 {
			return currentA, true
		}

		// Try all 8 possible 3-bit values for this position
		for bits := int64(0); bits < 8; bits++ {
			candidateA := (currentA << 3) | bits
			if candidateA == 0 && targetIdx == len(program)-1 {
				continue // A can't be 0 at start (would halt immediately without output)
			}

			output := runProgram(candidateA, b, c, program)

			// Check if output matches the suffix of the program
			expected := program[targetIdx:]
			if sliceEqual(output, expected) {
				result, found := search(targetIdx-1, candidateA)
				if found {
					return result, true
				}
			}
		}

		return 0, false
	}

	result, _ := search(len(program)-1, 0)
	return result
}

func sliceEqual(a []int, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		panic(err)
	}
	text := string(data)

	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
