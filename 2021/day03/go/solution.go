package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
)

func parseInput() ([]string, error) {
	exePath, err := os.Executable()
	if err != nil {
		return nil, err
	}
	inputPath := filepath.Join(filepath.Dir(exePath), "..", "input.txt")

	file, err := os.Open(inputPath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var numbers []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line != "" {
			numbers = append(numbers, line)
		}
	}
	return numbers, scanner.Err()
}

func part1(numbers []string) int {
	numBits := len(numbers[0])
	gamma := 0

	for pos := 0; pos < numBits; pos++ {
		ones := 0
		for _, n := range numbers {
			if n[pos] == '1' {
				ones++
			}
		}
		zeros := len(numbers) - ones

		if ones >= zeros {
			gamma |= 1 << (numBits - 1 - pos)
		}
	}

	// epsilon is bitwise NOT of gamma (within numBits)
	epsilon := gamma ^ ((1 << numBits) - 1)

	return gamma * epsilon
}

func binaryToInt(s string) int {
	result := 0
	for _, c := range s {
		result <<= 1
		if c == '1' {
			result |= 1
		}
	}
	return result
}

func findRating(numbers []string, useMostCommon bool) int {
	numBits := len(numbers[0])
	candidates := make([]string, len(numbers))
	copy(candidates, numbers)

	for pos := 0; pos < numBits; pos++ {
		if len(candidates) == 1 {
			break
		}

		ones := 0
		for _, n := range candidates {
			if n[pos] == '1' {
				ones++
			}
		}
		zeros := len(candidates) - ones

		var target byte
		if useMostCommon {
			if ones >= zeros {
				target = '1'
			} else {
				target = '0'
			}
		} else {
			if zeros <= ones {
				target = '0'
			} else {
				target = '1'
			}
		}

		var filtered []string
		for _, n := range candidates {
			if n[pos] == target {
				filtered = append(filtered, n)
			}
		}
		candidates = filtered
	}

	return binaryToInt(candidates[0])
}

func part2(numbers []string) int {
	oxygen := findRating(numbers, true)
	co2 := findRating(numbers, false)
	return oxygen * co2
}

func main() {
	numbers, err := parseInput()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", part1(numbers))
	fmt.Printf("Part 2: %d\n", part2(numbers))
}
