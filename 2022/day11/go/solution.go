package main

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

type Monkey struct {
	items       []uint64
	operator    string
	operand     string
	divisor     uint64
	ifTrue      int
	ifFalse     int
	inspections uint64
}

func parseMonkeys(text string) []Monkey {
	blocks := strings.Split(strings.TrimSpace(text), "\n\n")
	monkeys := make([]Monkey, len(blocks))
	numRe := regexp.MustCompile(`\d+`)
	opRe := regexp.MustCompile(`new = old ([+*]) (\w+)`)

	for i, block := range blocks {
		lines := strings.Split(strings.TrimSpace(block), "\n")

		// Parse starting items
		itemMatches := numRe.FindAllString(lines[1], -1)
		items := make([]uint64, len(itemMatches))
		for j, s := range itemMatches {
			val, _ := strconv.ParseUint(s, 10, 64)
			items[j] = val
		}

		// Parse operation
		opMatch := opRe.FindStringSubmatch(lines[2])
		operator := opMatch[1]
		operand := opMatch[2]

		// Parse divisor and targets
		divisor, _ := strconv.ParseUint(numRe.FindString(lines[3]), 10, 64)
		ifTrue, _ := strconv.Atoi(numRe.FindString(lines[4]))
		ifFalse, _ := strconv.Atoi(numRe.FindString(lines[5]))

		monkeys[i] = Monkey{
			items:       items,
			operator:    operator,
			operand:     operand,
			divisor:     divisor,
			ifTrue:      ifTrue,
			ifFalse:     ifFalse,
			inspections: 0,
		}
	}
	return monkeys
}

func applyOperation(old uint64, operator, operand string) uint64 {
	var val uint64
	if operand == "old" {
		val = old
	} else {
		v, _ := strconv.ParseUint(operand, 10, 64)
		val = v
	}

	if operator == "+" {
		return old + val
	}
	return old * val
}

func simulate(monkeys []Monkey, rounds int, reliefDivisor uint64, useModulo bool) {
	var modValue uint64 = 1
	if useModulo {
		for _, m := range monkeys {
			modValue *= m.divisor
		}
	}

	for round := 0; round < rounds; round++ {
		for i := range monkeys {
			for len(monkeys[i].items) > 0 {
				item := monkeys[i].items[0]
				monkeys[i].items = monkeys[i].items[1:]
				monkeys[i].inspections++

				// Apply operation
				newVal := applyOperation(item, monkeys[i].operator, monkeys[i].operand)

				// Apply relief
				if reliefDivisor > 1 {
					newVal /= reliefDivisor
				}

				// Apply modulo to prevent overflow
				if useModulo {
					newVal %= modValue
				}

				// Test and throw
				var target int
				if newVal%monkeys[i].divisor == 0 {
					target = monkeys[i].ifTrue
				} else {
					target = monkeys[i].ifFalse
				}
				monkeys[target].items = append(monkeys[target].items, newVal)
			}
		}
	}
}

func monkeyBusiness(monkeys []Monkey) uint64 {
	inspections := make([]uint64, len(monkeys))
	for i, m := range monkeys {
		inspections[i] = m.inspections
	}
	sort.Slice(inspections, func(i, j int) bool {
		return inspections[i] > inspections[j]
	})
	return inspections[0] * inspections[1]
}

func part1(text string) uint64 {
	monkeys := parseMonkeys(text)
	simulate(monkeys, 20, 3, false)
	return monkeyBusiness(monkeys)
}

func part2(text string) uint64 {
	monkeys := parseMonkeys(text)
	simulate(monkeys, 10000, 1, true)
	return monkeyBusiness(monkeys)
}

func main() {
	exePath, _ := os.Executable()
	dir := filepath.Dir(exePath)
	inputFile := filepath.Join(dir, "..", "input.txt")

	// Fallback to current working directory approach
	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		inputFile = "../input.txt"
	}

	data, err := os.ReadFile(inputFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	text := string(data)
	fmt.Println("Part 1:", part1(text))
	fmt.Println("Part 2:", part2(text))
}
