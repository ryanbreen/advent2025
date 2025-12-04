package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

func isSafe(levels []int) bool {
	if len(levels) < 2 {
		return true
	}

	// Calculate differences
	diffs := make([]int, len(levels)-1)
	for i := 0; i < len(levels)-1; i++ {
		diffs[i] = levels[i+1] - levels[i]
	}

	// Check if all increasing (1-3) or all decreasing (-3 to -1)
	allIncreasing := true
	allDecreasing := true

	for _, d := range diffs {
		if d < 1 || d > 3 {
			allIncreasing = false
		}
		if d < -3 || d > -1 {
			allDecreasing = false
		}
	}

	return allIncreasing || allDecreasing
}

func part1(lines []string) int {
	safeCount := 0
	for _, line := range lines {
		parts := strings.Fields(line)
		levels := make([]int, len(parts))
		for i, p := range parts {
			levels[i], _ = strconv.Atoi(p)
		}
		if isSafe(levels) {
			safeCount++
		}
	}
	return safeCount
}

func part2(lines []string) int {
	safeCount := 0
	for _, line := range lines {
		parts := strings.Fields(line)
		levels := make([]int, len(parts))
		for i, p := range parts {
			levels[i], _ = strconv.Atoi(p)
		}

		// Check if already safe
		if isSafe(levels) {
			safeCount++
			continue
		}

		// Try removing each level one at a time
		foundSafe := false
		for i := 0; i < len(levels); i++ {
			modified := make([]int, 0, len(levels)-1)
			modified = append(modified, levels[:i]...)
			modified = append(modified, levels[i+1:]...)
			if isSafe(modified) {
				safeCount++
				foundSafe = true
				break
			}
		}
		_ = foundSafe
	}
	return safeCount
}

func main() {
	// Read input file
	inputPath := filepath.Join("..", "input.txt")
	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	fmt.Printf("Part 1: %d\n", part1(lines))
	fmt.Printf("Part 2: %d\n", part2(lines))
}
