package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

func part1(lines []string) int {
	total := 0
	for _, line := range lines {
		n := len(line)
		if n < 2 {
			continue
		}

		// Precompute max suffix: maxSuffix[i] = max digit from position i to end
		maxSuffix := make([]int, n)
		maxSuffix[n-1] = int(line[n-1] - '0')
		for i := n - 2; i >= 0; i-- {
			digit := int(line[i] - '0')
			if digit > maxSuffix[i+1] {
				maxSuffix[i] = digit
			} else {
				maxSuffix[i] = maxSuffix[i+1]
			}
		}

		maxJoltage := 0
		// For each possible first battery position
		for i := 0; i < n-1; i++ {
			firstDigit := int(line[i] - '0')
			// The maximum second digit is the max from position i+1 onwards
			maxSecond := maxSuffix[i+1]
			joltage := firstDigit*10 + maxSecond
			if joltage > maxJoltage {
				maxJoltage = joltage
			}
		}

		total += maxJoltage
	}

	return total
}

func part2(lines []string) uint64 {
	total := uint64(0)
	for _, line := range lines {
		n := len(line)
		k := 12 // Select exactly 12 batteries

		if n < k {
			continue
		}

		// Greedy algorithm to select k digits that form the maximum number
		result := make([]byte, k)
		currentPos := 0

		for i := 0; i < k; i++ {
			// How many digits we still need to select after this one
			remainingNeeded := k - i - 1
			// Latest position we can start searching from
			searchEnd := n - remainingNeeded

			// Find the maximum digit in the valid range
			maxDigit := -1
			maxPos := currentPos
			for j := currentPos; j < searchEnd; j++ {
				digit := int(line[j] - '0')
				if digit > maxDigit {
					maxDigit = digit
					maxPos = j
				}
			}

			result[i] = byte(maxDigit + '0')
			currentPos = maxPos + 1
		}

		// Convert result to uint64
		joltage := uint64(0)
		for i := 0; i < k; i++ {
			joltage = joltage*10 + uint64(result[i]-'0')
		}
		total += joltage
	}

	return total
}

func main() {
	// Read input file
	exePath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting executable path: %v\n", err)
		os.Exit(1)
	}
	exeDir := filepath.Dir(exePath)
	inputPath := filepath.Join(exeDir, "..", "input.txt")

	// Try current directory if executable path doesn't work
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = "../input.txt"
	}

	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening input file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			lines = append(lines, line)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
