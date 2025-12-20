package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// countWays returns the number of ways to construct design from patterns.
// Uses bottom-up dynamic programming with a slice for O(1) memo access.
func countWays(design string, patterns []string) int64 {
	n := len(design)
	dp := make([]int64, n+1)
	dp[n] = 1 // Base case: empty suffix has one way

	for pos := n - 1; pos >= 0; pos-- {
		for _, pattern := range patterns {
			plen := len(pattern)
			if pos+plen <= n && design[pos:pos+plen] == pattern {
				dp[pos] += dp[pos+plen]
			}
		}
	}
	return dp[0]
}

// solve computes both part1 and part2 in a single pass over designs.
// Returns the count of possible designs and total number of arrangements.
func solve(patterns, designs []string) (int, int64) {
	var possible int
	var totalWays int64
	for _, design := range designs {
		ways := countWays(design, patterns)
		if ways > 0 {
			possible++
		}
		totalWays += ways
	}
	return possible, totalWays
}

func main() {
	exePath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting executable path: %v\n", err)
		os.Exit(1)
	}
	exeDir := filepath.Dir(exePath)
	inputPath := filepath.Join(exeDir, "..", "input.txt")

	// Fallback for running with go run
	if _, err := os.Stat(inputPath); os.IsNotExist(err) {
		inputPath = filepath.Join(filepath.Dir(os.Args[0]), "..", "input.txt")
		if _, err := os.Stat(inputPath); os.IsNotExist(err) {
			// Try relative to current working directory
			inputPath = "../input.txt"
		}
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	content := strings.TrimSpace(string(data))
	parts := strings.Split(content, "\n\n")

	// Parse patterns
	patternParts := strings.Split(parts[0], ",")
	patterns := make([]string, len(patternParts))
	for i, p := range patternParts {
		patterns[i] = strings.TrimSpace(p)
	}

	// Parse designs
	designs := strings.Split(strings.TrimSpace(parts[1]), "\n")

	part1, part2 := solve(patterns, designs)
	fmt.Printf("Part 1: %d\n", part1)
	fmt.Printf("Part 2: %d\n", part2)
}
