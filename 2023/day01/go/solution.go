package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// wordDigits uses a slice for deterministic iteration order
var wordDigits = []struct {
	word  string
	digit string
}{
	{"one", "1"},
	{"two", "2"},
	{"three", "3"},
	{"four", "4"},
	{"five", "5"},
	{"six", "6"},
	{"seven", "7"},
	{"eight", "8"},
	{"nine", "9"},
}

func part1(lines []string) (int, error) {
	total := 0
	for _, line := range lines {
		first, last := "", ""

		// Track first and last digits directly without building a list
		for _, c := range line {
			if c >= '0' && c <= '9' {
				if first == "" {
					first = string(c)
				}
				last = string(c)
			}
		}

		if first != "" {
			numStr := first + last
			num, err := strconv.Atoi(numStr)
			if err != nil {
				return 0, fmt.Errorf("failed to convert %s to int: %w", numStr, err)
			}
			total += num
		}
	}
	return total, nil
}

func part2(lines []string) (int, error) {
	total := 0
	for _, line := range lines {
		first, last := "", ""

		// Track first and last digits directly
		for i := 0; i < len(line); i++ {
			digit := findDigitAt(line, i)
			if digit != "" {
				if first == "" {
					first = digit
				}
				last = digit
			}
		}

		if first != "" {
			numStr := first + last
			num, err := strconv.Atoi(numStr)
			if err != nil {
				return 0, fmt.Errorf("failed to convert %s to int: %w", numStr, err)
			}
			total += num
		}
	}
	return total, nil
}

// findDigitAt checks if there's a digit (numeric or word) at the given position
func findDigitAt(line string, pos int) string {
	// Check if current character is a digit
	if line[pos] >= '0' && line[pos] <= '9' {
		return string(line[pos])
	}

	// Check for spelled-out digits using ordered slice
	for _, wd := range wordDigits {
		if strings.HasPrefix(line[pos:], wd.word) {
			return wd.digit
		}
	}

	return ""
}

func main() {
	file, err := os.Open("../input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	// Pre-allocate slice with reasonable capacity (1000 lines is typical for AoC)
	lines := make([]string, 0, 1000)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	result1, err := part1(lines)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error in part1: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Part 1: %d\n", result1)

	result2, err := part2(lines)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error in part2: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Part 2: %d\n", result2)
}
