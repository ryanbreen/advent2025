package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// findMarker finds the first position where the last windowSize characters are all unique
func findMarker(data string, windowSize int) int {
	for i := windowSize; i <= len(data); i++ {
		window := data[i-windowSize : i]
		if allUnique(window) {
			return i
		}
	}
	return -1
}

// allUnique checks if all characters in a string are unique
func allUnique(s string) bool {
	seen := make(map[rune]bool)
	for _, c := range s {
		if seen[c] {
			return false
		}
		seen[c] = true
	}
	return true
}

func part1(data string) int {
	return findMarker(data, 4)
}

func part2(data string) int {
	return findMarker(data, 14)
}

func main() {
	// Get the directory of the current executable/script
	execPath, err := os.Executable()
	if err != nil {
		// Fallback to using current working directory
		execPath, _ = os.Getwd()
	}
	dir := filepath.Dir(execPath)

	// Try to find input.txt relative to the script location
	inputFile := filepath.Join(dir, "..", "input.txt")
	content, err := os.ReadFile(inputFile)
	if err != nil {
		// Fallback: try relative to current working directory
		inputFile = filepath.Join(".", "..", "input.txt")
		content, err = os.ReadFile(inputFile)
		if err != nil {
			// Last fallback: try from go subdirectory perspective
			content, err = os.ReadFile("../input.txt")
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error reading input file: %v\n", err)
				os.Exit(1)
			}
		}
	}

	data := strings.TrimSpace(string(content))

	fmt.Println("Part 1:", part1(data))
	fmt.Println("Part 2:", part2(data))
}
