package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

// memoKey is a struct key for memoization
type memoKey struct {
	value  int
	blinks int
}

func countStones(value int, blinks int, memo map[memoKey]int) int {
	if blinks == 0 {
		return 1
	}

	// Check memoization cache
	key := memoKey{value, blinks}
	if cached, exists := memo[key]; exists {
		return cached
	}

	var result int

	// Rule 1: 0 becomes 1
	if value == 0 {
		result = countStones(1, blinks-1, memo)
	} else {
		// Check if even number of digits
		s := strconv.Itoa(value)
		if len(s)%2 == 0 {
			// Rule 2: Even number of digits -> split in half
			mid := len(s) / 2
			// These conversions are safe: s is a digit string from Itoa
			left, _ := strconv.Atoi(s[:mid])
			right, _ := strconv.Atoi(s[mid:])
			result = countStones(left, blinks-1, memo) + countStones(right, blinks-1, memo)
		} else {
			// Rule 3: Multiply by 2024
			result = countStones(value*2024, blinks-1, memo)
		}
	}

	memo[key] = result
	return result
}

func part1(stones []int) int {
	memo := make(map[memoKey]int)
	total := 0
	for _, stone := range stones {
		total += countStones(stone, 25, memo)
	}
	return total
}

func part2(stones []int) int {
	memo := make(map[memoKey]int)
	total := 0
	for _, stone := range stones {
		total += countStones(stone, 75, memo)
	}
	return total
}

func main() {
	// Read input file
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	// Parse space-separated numbers
	input := strings.TrimSpace(string(data))
	parts := strings.Fields(input)
	stones := make([]int, 0, len(parts))
	for _, part := range parts {
		num, err := strconv.Atoi(part)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error parsing number '%s': %v\n", part, err)
			os.Exit(1)
		}
		stones = append(stones, num)
	}

	fmt.Printf("Part 1: %d\n", part1(stones))
	fmt.Printf("Part 2: %d\n", part2(stones))
}
